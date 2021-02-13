module LibDworkin.Renderer

open OpenTK.Graphics.ES20
open OpenTK

open Geometry
open PharoCompatibility
open SpecializedCollections
open AgentAbstractions
open RendererComponents
open RendererComponents.Helpers
open DworkinSettings

#if __ANDROID__
open Android.Opengl
open Javax.Microedition.Khronos


type EGLContextFactory private() =
    inherit Java.Lang.Object()
    
    static let mutable currentContext : Egl.EGLContext = Egl.EGL10.EglNoContext

    interface GLSurfaceView.IEGLContextFactory with
        member this.CreateContext(egl, display, eglConfig) =
            // see https://www.khronos.org/registry/EGL/sdk/docs/man/html/eglCreateContext.xhtml
            let EGL_CONTEXT_CLIENT_VERSION = 0x3098;
            let attribs_config = [| 
                EGL_CONTEXT_CLIENT_VERSION; 2;
                Egl.EGL10.EglNone
            |]
            let context = egl.EglCreateContext(display, eglConfig, Egl.EGL10.EglNoContext, attribs_config)
            currentContext <- context
            context

        member this.DestroyContext(egl, display, context) = 
            ResettableLazy<Shader>.ResetAll()
            ResettableLazy<int>.ResetAll() // textures
            if egl.EglDestroyContext(display, context) then
                currentContext <- Egl.EGL10.EglNoContext
    
    static member CurrentContext = currentContext

    static member val Instance = new EGLContextFactory()

    /// Global variable that indicates need to reset shader uniforms after they were modified from another OpenGL context
    static member val NeedsShaderUniformsReset = false with get, set
#endif

let maxSDFDistance = 16.0f



type BaseRenderer(platform : AbstractPlatform) =
#if __ANDROID__
    inherit Java.Lang.Object()
#endif
    // fields
#if __ANDROID__
    let mutable lastContext : Egl.EGLContext = null
#endif
    let mutable shapeVertices = Buffer<Vector2>(128)
    let mutable polygonSegmentsVertices = Buffer<Vector2>(128)
    let mutable polygonVertexIndices = Buffer<int>(128)

    let mutable circleVertices = Buffer<Vector2>(128)
    let mutable circleVertexData = Buffer<Vector4>(128) // center, radius, innerRadius
    let mutable circleVertexIndices = Buffer<int>(128) 

    let sdfTextures = [| 0; 0 |]
    let mutable sdfFrameBuffer = 0

    let materialTextures = [| 0; 0 |]
    let mutable materialFrameBuffer = 0

    let stencilSize = 128
    let stencil = MakeshiftStencil(stencilSize)
    let mutable stencilTexture = 0

    let mutable quadVBO = 0
    let mutable shapesVBO = 0
    let mutable polygonSegmentsVBO = 0
    let mutable circlesVBO = 0
    let mutable circleDataVBO = 0

    let mutable foregroundMaterial : Material = SolidColorMaterial(platform, Graphics.Color4.Black) :> Material
    let mutable backgroundMaterial : Material = SolidColorMaterial(platform, Graphics.Color4.White) :> Material
    let mutable effect : Effect = SDFVisualizerEffect(platform, 32.0f) :> Effect

    let polygonDrawingShader = platform.GetShader("drawPolygons.vert", "drawPolygons.frag")
    let circleDrawingShader = platform.GetShader("drawCircles.vert", "drawCircles.frag")
    let calcPositiveDistShader = platform.GetShader("outerShell.vert", "positiveDist.frag")
    let calcNegativeDistShaderHorizontal = platform.GetShader("fullScreenQuad.vert", "negativeDistHorizontal.frag")
    let calcNegativeDistShaderVertical = platform.GetShader("fullScreenQuad.vert", "negativeDistVertical.frag")

    let mutable wallpaperWidth = 0
    let mutable wallpaperHeight = 0
    let mutable scale = Point(1.0f, 1.0f)

    /// maximal absolute values of distances in signed distance field
    member this.MaxDistance = maxSDFDistance

    /// frames per second
    member this.FPS = 30

    member this.WallpaperSize = (wallpaperWidth, wallpaperHeight)
#if __ANDROID__
    member internal this.LastContext = lastContext
#endif

    member this.BackgroundTexture = materialTextures.[0]
    member this.ForegroundTexture = materialTextures.[1]
    /// Use this to draw to sdf
    abstract SDFTexture : int
    default this.SDFTexture = sdfTextures.[0]
    member internal this.SDFTexture2 = sdfTextures.[1]
    member internal this.SDFFrameBuffer = sdfFrameBuffer
    member internal this.QuadVBO = quadVBO
    member internal this.CalcPositiveDistShader = calcPositiveDistShader.Value
    member internal this.CalcNegativeDistShaderHorizontal = calcNegativeDistShaderHorizontal.Value
    member internal this.CalcNegativeDistShaderVertical = calcNegativeDistShaderVertical.Value

    member this.Foreground
        with get() : Material = foregroundMaterial
        and set(material : Material) =
            material.Scale <- scale
            foregroundMaterial <- material
            if this.ForegroundTexture <> 0 then
                this.DrawMaterial(foregroundMaterial, this.ForegroundTexture)
    
    member this.Background
        with get() : Material = backgroundMaterial
        and set(material : Material) =
            material.Scale <- scale
            backgroundMaterial <- material
            if this.BackgroundTexture <> 0 then
                this.DrawMaterial(backgroundMaterial, this.BackgroundTexture)
    
    member this.Effect
        with get() : Effect = effect
        and set(e : Effect) =
            effect <- e
            effect.ScreenDimensions <- Point(1.0f / scale.X, 1.0f / scale.Y)

#if __ANDROID__
    interface GLSurfaceView.IRenderer with
        member this.Dispose() = 
            GL.DeleteTextures(sdfTextures.Length, sdfTextures)
            if stencilTexture <> 0 then GL.DeleteTexture(stencilTexture)
            GL.DeleteFramebuffers(1, [| sdfFrameBuffer |])
            GL.DeleteBuffers(5, [| quadVBO; shapesVBO; polygonSegmentsVBO; circlesVBO; circleDataVBO |])
            let disposeShader (shader : ResettableLazy<Shader>) =
                if shader.IsValueCreated then
                    (shader.Value :> System.IDisposable).Dispose()
            [| polygonDrawingShader; circleDrawingShader; calcPositiveDistShader; calcNegativeDistShaderHorizontal; calcNegativeDistShaderVertical |]
                |> Array.iter disposeShader
            base.Dispose()

        member this.Handle: nativeint = 
            base.Handle

        member this.OnSurfaceChanged(gl: Opengles.IGL10, width: int, height: int) = 
            this.OnSurfaceChanged(width, height)

        member this.OnSurfaceCreated(gl: Opengles.IGL10, config: Egl.EGLConfig) = 
            this.OnSurfaceCreated()

        member this.OnDrawFrame(gl : Opengles.IGL10) =
            this.OnDrawFrame()
#endif 
    // Methods that perform some OpenGL-specific tasks
    member this.PerformDrawElements(indicesBuffer : Buffer<int>) =
#if __ANDROID__            
            GL.DrawElements(BeginMode.Triangles, indicesBuffer.Size, DrawElementsType.UnsignedInt, indicesBuffer.Array)
#else       
            Graphics.ES30.GL.DrawElements(Graphics.ES30.PrimitiveType.Triangles, indicesBuffer.Size, 
                                          Graphics.ES30.DrawElementsType.UnsignedInt, indicesBuffer.Array)
#endif
    /// Transfer data from buffer to VBO
    member this.TransferBufferData(buffer : Buffer<'T>, vbo : int, bufferTarget : BufferTarget) = 
        GL.BindBuffer(bufferTarget, vbo)
        GL.BufferData(bufferTarget, nativeint (buffer.Size * sizeof<'T>), buffer.Array, BufferUsage.StreamDraw)

    // shapes buliding (adding data to VBOs)
    member private this.BuildCircle(center, radius, strokeWidth) =
        let innerRadius = if strokeWidth > 0.0f then max 0.0f (radius - strokeWidth*0.5f) else 0.0f
        let radius' = radius + (max 0.0f strokeWidth*0.5f) + this.MaxDistance
        let numSteps = this.GetNumCircleSegments(radius') / 8 |> max 4
        let firstIndex = circleVertices.Size
        circleVertices.Add center
        circleVertexData.Add(Vector4(center.X, center.Y, radius, innerRadius))
        let angleStep = Single.TwoPi / (float32 numSteps)
        let r = radius' / cos(angleStep * 0.5f)
        for i=1 to numSteps do
            let angle = angleStep * (float32 i)
            circleVertices.Add(center + Point(r * cos angle, r * sin angle))
            circleVertexData.Add(Vector4(center.X, center.Y, radius, innerRadius))
            circleVertexIndices.Add firstIndex
            circleVertexIndices.Add (firstIndex + i)
            circleVertexIndices.Add (firstIndex + (i % numSteps) + 1)
        
    member this.BuildCircle(center, radius) =
        this.BuildCircle(center, radius, 0.0f)

    member this.BuildCircleContour(center, radius, strokeWidth) =
        this.BuildCircle(center, radius, strokeWidth)
        
    member this.BuildPolygon(points : Point[]) =
        let fisrtVertexIndex = shapeVertices.Size
        for p in points do shapeVertices.Add p
        for i=fisrtVertexIndex+1 to fisrtVertexIndex+points.Length-2 do
            polygonVertexIndices.Add fisrtVertexIndex
            polygonVertexIndices.Add i
            polygonVertexIndices.Add (i + 1)
    
    member this.GetQuadsForPolygonContour(points : Point[], width : float32) : Point[][] =
        let n = points.Length

        let inline segment p1 p2 = Point.Normalize(p1 - p2)
        let offset (s1 : Point) (s2 : Point) : Point =
            let offset = s1 - s2
            let factor = offset.NearestPointAlongLineFromTo(Point.Zero, s1.PerpendicularLeft).Length
            offset / factor * width * 0.5f 

        let mutable s1 = segment points.[0] points.[1]
        let mutable offset1 = offset s1 (segment points.[1] points.[2])
        points |> Array.mapi (fun i p1 ->
            let p2 = points.[(i+1)%n]
            let s2 = segment p2 (points.[(i+2)%n])
            let offset2 = offset s1 s2
            let quad = [| p1 - offset1; p1 + offset1; p2 + offset2; p2 - offset2 |]
            s1 <- s2
            offset1 <- offset2
            quad)

    member this.BuildPolygonContour(points : Point[], width : float32) =
        for quadPoints in this.GetQuadsForPolygonContour(points, width) do
            this.BuildPolygon(quadPoints)
    
    
    member this.GetNumCircleSegments(radius: float32) =
        let size = radius * float32(min wallpaperWidth wallpaperHeight) / (min scale.X scale.Y)
        // account for DPI?
        max 6 ( min (int (size * 4.0f)) 120)
    
    member private this.SetCommonTextureParameters(wrapMode : TextureWrapMode) =
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int wrapMode)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int wrapMode)

    /// initialize sdf texture and framebuffer
    member private this.InitializeSDF() =
        if sdfTextures |> Array.exists ((<>) 0) then GL.DeleteTextures(sdfTextures.Length, sdfTextures) 
        GL.GenTextures(sdfTextures.Length, sdfTextures)
        if sdfFrameBuffer = 0 then 
            sdfFrameBuffer <- gen1 GL.GenFramebuffers
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.SDFFrameBuffer)

        for texture in sdfTextures do
            GL.BindTexture(TextureTarget.Texture2D, texture)
            // NOTE: UnsignedShort565 may be the only supported pixel type
            GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgb, wallpaperWidth, wallpaperHeight, 0, 
                            PixelFormat.Rgb, PixelType.UnsignedByte, nativeint 0)
            this.SetCommonTextureParameters(TextureWrapMode.ClampToEdge)

            GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                    TextureTarget.Texture2D, texture, 0)      
            GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
            GL.Clear(ClearBufferMask.ColorBufferBit)
        
        GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                TextureTarget.Texture2D, this.SDFTexture, 0)
    
    member this.InitializeMaterialTextures() =
        if materialTextures |> Array.exists ((<>) 0) then GL.DeleteTextures(materialTextures.Length, materialTextures) 
        GL.GenTextures(materialTextures.Length, materialTextures)
        if materialFrameBuffer = 0 then 
            materialFrameBuffer <- gen1 GL.GenFramebuffers
        
        Array.iter2 
            (fun (texture : int) (material : Material) ->
                GL.BindTexture(TextureTarget.Texture2D, texture)
                // NOTE: UnsignedShort565 may be the only supported pixel type
                GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgb, wallpaperWidth, wallpaperHeight, 0, 
                                PixelFormat.Rgb, PixelType.UnsignedByte, nativeint 0)
                this.SetCommonTextureParameters(TextureWrapMode.ClampToEdge)
                this.DrawMaterial(material, texture))
            [| this.ForegroundTexture; this.BackgroundTexture |] 
            [| this.Foreground; this.Background |]
    
    member private this.InitializeStencilTexture() =
        if stencilTexture <> 0 then GL.DeleteTexture(stencilTexture)
        stencilTexture <- GL.GenTexture()
        GL.BindTexture(TextureTarget.Texture2D, stencilTexture)
        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Luminance, stencilSize, stencilSize, 0, 
                      PixelFormat.Luminance, PixelType.UnsignedByte, stencil.TextureData)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Nearest)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Nearest)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)
    
    member this.InitializeQuadVBO() =
        quadVBO <- gen1 GL.GenBuffers
        GL.BindBuffer(BufferTarget.ArrayBuffer, this.QuadVBO)
        GL.BufferData(BufferTarget.ArrayBuffer, 
                      nativeint (fullScreenQuadVertices.Length * sizeof<float32>), 
                      fullScreenQuadVertices, 
                      BufferUsage.StaticDraw)

    member this.ResetStencil() = stencil.Reset()

    member this.DrawMaterial(material : Material, texture : int) =
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        GL.BindTexture(TextureTarget.Texture2D, texture)
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, materialFrameBuffer)
        GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                TextureTarget.Texture2D, texture, 0)
        material.Use()
        let aTexCoords = material.Shader.GetAttributeLocation("texCoords")
        GL.BindBuffer(BufferTarget.ArrayBuffer, quadVBO)
        GL.EnableVertexAttribArray(aTexCoords)
        GL.VertexAttribPointer(aTexCoords, 2, VertexAttribPointerType.Float, false, 0, 0)
        GL.DrawArrays(BeginMode.Triangles, 0, 6)
        stopWatch.Stop()
#if __ANDROID__
        let materialKind = if texture = this.ForegroundTexture then "foreground" else "background"
        let materialName = material.GetType().Name
        Android.Util.Log.Debug("frameTime", sprintf "Rendering %s (%s): %gms" materialKind materialName stopWatch.Elapsed.TotalMilliseconds) |> ignore
#endif

    abstract member OnSurfaceChanged : int * int -> unit
    default this.OnSurfaceChanged(width: int, height: int) = 
        // Set the OpenGL viewport to fill the entire surface. 
        GL.Viewport(0, 0, width, height)
        // setup scale and offset
        let dimensions = this.GetDimensions(width, height)
        this.SetScale(dimensions, width, height)

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        this.InitializeSDF()
        this.InitializeMaterialTextures()
        this.InitializeStencilTexture()
        stopWatch.Stop()
#if __ANDROID__
        Android.Util.Log.Debug("frameTime", sprintf "(re-)creating textures: %gms" stopWatch.Elapsed.TotalMilliseconds) |> ignore
#endif
        
        GL.BindTexture(TextureTarget.Texture2D, 0)
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)

        this.ResetEnvironment(dimensions)
    
    abstract member OnSurfaceCreated : unit -> unit
    default this.OnSurfaceCreated() = 
        GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)

        this.ResetStencil()

        // Initialize stuff that is not dependent on screen size. Everything else is (re-)initialized in OnSurfaceChanged
        this.CalcPositiveDistShader.DeferredSetUniform("h", this.MaxDistance)
        circleDrawingShader.Value.DeferredSetUniform("h", this.MaxDistance)
        
        this.InitializeQuadVBO()
        
        shapesVBO <- gen1 GL.GenBuffers 
        polygonSegmentsVBO <- gen1 GL.GenBuffers 
        circlesVBO <- gen1 GL.GenBuffers
        circleDataVBO <- gen1 GL.GenBuffers

        GL.Disable(EnableCap.DepthTest)
        GL.Disable(EnableCap.CullFace)
#if __ANDROID__
        if lastContext <> EGLContextFactory.CurrentContext then // OpenGL Context was lost, restart
            this.OnSurfaceChanged(wallpaperWidth, wallpaperHeight)
            lastContext <- EGLContextFactory.CurrentContext
#endif
    
    abstract member OnDrawFrame : unit -> unit
    default this.OnDrawFrame() =
        // Clear the rendering surface. 
        GL.Clear(ClearBufferMask.ColorBufferBit)
    
    abstract member ResetEnvironment : Point -> unit
    default this.ResetEnvironment(dimensions) = ()

    member this.ClearSDF() =
        GL.ColorMask(true, true, true, true)
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.SDFFrameBuffer)
        for texture in sdfTextures do
            GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                    TextureTarget.Texture2D, texture, 0) 
            GL.ClearColor(1.0f, 1.0f, 0.0f, 1.0f)
            GL.Clear(ClearBufferMask.ColorBufferBit)

    member this.DrawImage() =
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)

        this.Effect.Use()

        GL.ActiveTexture(TextureUnit.Texture0)
        GL.BindTexture(TextureTarget.Texture2D, this.SDFTexture)
        GL.Uniform1(this.Effect.Shader.GetUniformLocation("sdf"), 0)
        GL.ActiveTexture(TextureUnit.Texture1)
        GL.BindTexture(TextureTarget.Texture2D, this.BackgroundTexture)
        GL.Uniform1(this.Effect.Shader.GetUniformLocation("background"), 1)
        GL.ActiveTexture(TextureUnit.Texture2)
        GL.BindTexture(TextureTarget.Texture2D, this.ForegroundTexture)
        GL.Uniform1(this.Effect.Shader.GetUniformLocation("foreground"), 2)

        if this.Effect.UsesStencil then
            GL.ActiveTexture(TextureUnit.Texture3)
            GL.BindTexture(TextureTarget.Texture2D, stencilTexture)
            GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Luminance, stencilSize, stencilSize, 0, 
                          PixelFormat.Luminance, PixelType.UnsignedByte, stencil.TextureData)
            GL.Uniform1(this.Effect.Shader.GetUniformLocation("stencil"), 3)

        let aTexCoords = this.Effect.Shader.GetAttributeLocation("texCoords")
        GL.BindBuffer(BufferTarget.ArrayBuffer, quadVBO)
        GL.EnableVertexAttribArray(aTexCoords)
        GL.VertexAttribPointer(aTexCoords, 2, VertexAttribPointerType.Float, false, 0, 0)

        GL.DrawArrays(BeginMode.Triangles, 0, 6)
        
        GL.DisableVertexAttribArray(aTexCoords)
    
    member this.BuildOuterShellForPolygon(points : Point[]) =
        let n = points.Length
        let h = this.MaxDistance
        let clockwise = (points.[1] - points.[n-1]).Cross(points.[0] - points.[1]) < 0.0f // maybe counterclockwise, but it doesn't matter

        points |> Array.iteri (fun i O ->
            let Oprev, O2 = 
                if clockwise then points.[(i-1+n)%n], points.[(i+1)%n]
                else points.[(i+1)%n], points.[(i-1+n)%n]
            let OA = (O - Oprev).LeftRotated.Normalized() * h
            let OB = (O2 - O).LeftRotated.Normalized() * h
            let OC = (OA + OB).Normalized() * h
            let cosAOC = Point.Dot(OA, OC) / (h*h) // we know that both OA and OC have length=h by construction
            let cosAOD = sqrt((1.0f + cosAOC) * 0.5f)
            let OD = (OA + OC) / (2.0f * cosAOD * cosAOD) // (OA + OC).Normalized() * (h / cosAOD)
            let OE = (OB + OC) / (2.0f * cosAOD * cosAOD) // (OB + OC).Normalized() * (h / cosAOD)

            let vertices = [| O; O + OA; O + OD; O + OE; O + OB |]
            this.BuildPolygon(vertices)
            for _j=1 to 5 do polygonSegmentsVertices.Add O
            
            this.BuildPolygon([| O; O + OB; O2 + OB; O2 |])
            for _j=1 to 2 do polygonSegmentsVertices.Add O
            for _j=1 to 2 do polygonSegmentsVertices.Add O2 )

    member private this.ClearBlueChannel() =
        GL.ColorMask(false, false, true, false)
        GL.ClearColor(0.0f, 0.0f, 1.0f, 0.0f)
        GL.Clear(ClearBufferMask.ColorBufferBit)
        GL.ColorMask(true, true, true, true)

    member this.RenderShapes(shapes : ShapeAndPaint[]) =
        // Update sdf (signed distance field) texture.
        // Distances >= 0 are stored in R channel; [absolute values of] distances < 0 are stored in G channel.
        // Distances are in range [-maxDistance; maxDistance]. 
        // maxDistance is defined as a multiple of unit distance in agent environment coordinates (may not be the same distance in screen space coordinates).
        // B channel acts as a stencil buffer for negative distances calculation (pixels with B>0 are skipped)
        if not (Array.isEmpty shapes) then
            GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                    TextureTarget.Texture2D, this.SDFTexture, 0) // needed for some reason
            
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.SDFFrameBuffer)
            this.ClearBlueChannel()

            if this.Effect.UsesStencil then
                stencil.Update(shapes, scale, this.MaxDistance)

            let polygons, circles = 
                shapes 
                |> Array.partition (function | struct (Polygon(_), _) -> true | struct (Circle(_), _) -> false)
            // Step 0: build and draw shapes.            
            for struct (Polygon(points), paint) in polygons do
                if paint.Fill then this.BuildPolygon(points)
                if paint.StrokeWidth > 0.0f then this.BuildPolygonContour(points, paint.StrokeWidth)
                
            if polygonVertexIndices.Size > 0 then
                // draw polygons
                this.TransferBufferData(shapeVertices, shapesVBO, BufferTarget.ArrayBuffer)

                polygonDrawingShader.Value.Use()
                let aPos = polygonDrawingShader.Value.GetAttributeLocation("pos")
                GL.EnableVertexAttribArray(aPos)
                GL.VertexAttribPointer(aPos, 2, VertexAttribPointerType.Float, false, 0, 0)

                this.PerformDrawElements(polygonVertexIndices)
            
            shapeVertices.Reset()
            polygonVertexIndices.Reset()

            for struct (Circle(center, radius), paint) in circles do
                if paint.Fill then this.BuildCircle(center, radius + paint.StrokeWidth * 0.5f)
                elif paint.StrokeWidth > 0.0f then this.BuildCircleContour(center, radius, paint.StrokeWidth)

            // Build outer shell and area of influence for each shape.
            for struct (Polygon(points), paint) in polygons do
                if paint.Fill then 
                    this.BuildOuterShellForPolygon(points)
                if paint.StrokeWidth > 0.0f then 
                    // for now build  outer shell and aoi for each contour segment individually - not optimal, but works
                    let contourSegmentQuads = this.GetQuadsForPolygonContour(points, paint.StrokeWidth)
                    for quadPoints in contourSegmentQuads do
                        this.BuildOuterShellForPolygon(quadPoints)
            
            this.TransferBufferData(shapeVertices, shapesVBO, BufferTarget.ArrayBuffer)

            GL.Enable(EnableCap.Blend)
            // Min blend equation is not specified by OpenGL ES 2.0 standard - need OpenGL ES 3.0 or GL_EXT_blend_minmax extension
            GL.BlendEquation(All.Min) 

            // Step 1(polygons): calculate [positive] distances outside of shapes.
            if polygonVertexIndices.Size > 0 then
                this.TransferBufferData(polygonSegmentsVertices, polygonSegmentsVBO, BufferTarget.ArrayBuffer)
                this.CalcPositiveDistShader.Use()
                let aSegment = this.CalcPositiveDistShader.GetAttributeLocation("pointOnSegment")
                GL.EnableVertexAttribArray(aSegment)
                GL.VertexAttribPointer(aSegment, 2, VertexAttribPointerType.Float, false, 0, 0)
                
                GL.BindBuffer(BufferTarget.ArrayBuffer, shapesVBO)
                let aPos = this.CalcPositiveDistShader.GetAttributeLocation("pos")
                GL.EnableVertexAttribArray(aPos)
                GL.VertexAttribPointer(aPos, 2, VertexAttribPointerType.Float, false, 0, 0)

                this.PerformDrawElements(polygonVertexIndices)
            
            // Step 1 (circles): calculate [positive] distances outside of circles and draw circles.
            if circleVertexIndices.Size > 0 then
                circleDrawingShader.Value.Use()
                this.TransferBufferData(circleVertexData, circleDataVBO, BufferTarget.ArrayBuffer)
                let aData = circleDrawingShader.Value.GetAttributeLocation("data")
                GL.EnableVertexAttribArray(aData)
                GL.VertexAttribPointer(aData, 4, VertexAttribPointerType.Float, false, 0, 0)

                this.TransferBufferData(circleVertices, circlesVBO, BufferTarget.ArrayBuffer)
                let aPos = circleDrawingShader.Value.GetAttributeLocation("pos")
                GL.EnableVertexAttribArray(aPos)
                GL.VertexAttribPointer(aPos, 2, VertexAttribPointerType.Float, false, 0, 0)

                this.PerformDrawElements(circleVertexIndices)

            GL.BlendEquation(BlendEquationMode.FuncAdd)
            GL.Disable(EnableCap.Blend)
            
            this.CalculateNegativeDistances()
            // cleanup
            this.ClearBuffers()
    
    member private this.CalculateNegativeDistances() =
            // Step 2: calculate [negative] distances inside of shapes in area of influence.
            // Since one cannot read from and write to the same texture, 
            // two textures are used and are swapped between horizontal and vertical passes.
            GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                    TextureTarget.Texture2D, this.SDFTexture2, 0)
            GL.ActiveTexture(TextureUnit.Texture0)
            GL.BindTexture(TextureTarget.Texture2D, this.SDFTexture)
            GL.BindBuffer(BufferTarget.ArrayBuffer, quadVBO)

            this.ClearBlueChannel()

            // horizontal pass
            this.CalcNegativeDistShaderHorizontal.Use()
            GL.Uniform1(this.CalcNegativeDistShaderHorizontal.GetUniformLocation("sdf"), 0)
            let aPos = this.CalcNegativeDistShaderHorizontal.GetAttributeLocation("texCoords")
            GL.EnableVertexAttribArray(aPos)
            GL.VertexAttribPointer(aPos, 2, VertexAttribPointerType.Float, false, 0, 0)
            GL.DrawArrays(BeginMode.Triangles, 0, 6)

            // swap textures; vertical pass
            GL.BindTexture(TextureTarget.Texture2D, this.SDFTexture2)
            this.CalcNegativeDistShaderVertical.Use()
            GL.Uniform1(this.CalcNegativeDistShaderVertical.GetUniformLocation("sdf"), 0)
            let aPos = this.CalcNegativeDistShaderVertical.GetAttributeLocation("texCoords")
            GL.EnableVertexAttribArray(aPos)
            GL.VertexAttribPointer(aPos, 2, VertexAttribPointerType.Float, false, 0, 0)
            GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                    TextureTarget.Texture2D, this.SDFTexture, 0)
            GL.DrawArrays(BeginMode.Triangles, 0, 6)

    member this.ClearBuffers() =
        shapeVertices.Reset()
        polygonVertexIndices.Reset()
        polygonSegmentsVertices.Reset()
        circleVertices.Reset()
        circleVertexData.Reset()
        circleVertexIndices.Reset()
    
    /// Separated into separate method so that subclasses can set scale/wallpaperWidth/wallpaperHeight variables directly if needed
    member this.SetSizeAndScaleVariables(dimensions : Point, screenWidth : int, screenHeight : int) = 
        scale <- Point(1.0f / dimensions.X, 1.0f / dimensions.Y)
        wallpaperWidth <- screenWidth
        wallpaperHeight <- screenHeight

    /// Set scales used in shaders for drawing shapes, calculating SDF; and in material and effect shaders
    member this.SetScale(dimensions : Point, screenWidth : int, screenHeight : int) = 
        this.SetSizeAndScaleVariables(dimensions, screenWidth, screenHeight)
        
        let xs, ys = scale.X, scale.Y
        let width, height = float32 wallpaperWidth, float32 wallpaperHeight

        polygonDrawingShader.Value.DeferredSetUniform("scale", xs, ys)
        calcPositiveDistShader.Value.DeferredSetUniform("scale", xs, ys)
        let h = this.MaxDistance * width * xs // account for scale
        this.CalcNegativeDistShaderHorizontal.DeferredSetUniform("h", h)
        this.CalcNegativeDistShaderVertical.DeferredSetUniform("h", h)
        circleDrawingShader.Value.DeferredSetUniform("scale", xs, ys)
        this.Foreground.Scale <- scale
        this.Background.Scale <- scale
        
        this.CalcPositiveDistShader.DeferredSetUniform("screenDimensions", width, height)
        this.Effect.ScreenDimensions <- dimensions
        this.CalcNegativeDistShaderHorizontal.DeferredSetUniform("xScale", 1.0f / width)
        this.CalcNegativeDistShaderVertical.DeferredSetUniform("yScale", 1.0f / height)
    
    ///<summary>Get environment dimensions given screen width and height in pixels.</summary>
    /// Constraints:
    /// - between 480 and 600 on shorter side
    /// Preferences:
    /// - screen size being multiple of environment dimensions or vice versa
    member this.GetDimensions(width : int, height : int) : Point =
        let shortMin = 480.0f
        let shortMax = 600.0f
        let shortSide = float32(min width height)
        
        let multiple =
            let preferredMultiples = [| 1.0f; 2.0f; 3.0f; 4.0f; 0.5f; 1.5f; (1.0f/1.5f); (5.0f/3.0f); (4.0f/3.0f); 0.75f; 2.5f; 3.5f |]
            match preferredMultiples |> Array.tryFind (fun k -> between shortMin shortMax (shortSide / k)) with
            | Some(k) -> k
            | None -> shortSide / shortMin

        Point(float32(width) / multiple, float32(height) / multiple)



type DworkinRenderer(platform, settingsRef : DworkinSettings.SettingsHolder ref, ?stepper : AbstractEnvironmentStepper) as this =
    inherit BaseRenderer(platform)

    let mutable stepsPerFrame = 1
    let mutable currentSettings = !settingsRef
    let mutable environmentConfig = currentSettings.Environment.Build(currentSettings.Seed) :> Agents.IDworkinEnvironmentConfig
    let mutable framesAfterStop = 0
    do
        let random = System.Random(currentSettings.Seed)
        let fgColor, bgColor = currentSettings.GetColors(System.Random(currentSettings.Seed)) // use independent PRNG for colors
        this.Effect <- currentSettings.Effect.Build(platform, random)
        this.Foreground <- currentSettings.Foreground.Build(platform, random, fgColor)
        this.Background <- currentSettings.Background.Build(platform, random, bgColor)
        this.Effect.SetMaterialProperties(
            { Color=fgColor; Specular=this.Foreground.Specular; Roughness=this.Foreground.Roughness },
            { Color=bgColor; Specular=this.Background.Specular; Roughness=this.Background.Roughness })
    
    member val EnvironmentStepper : AbstractEnvironmentStepper = 
        match stepper with
        | Some(value) -> value
        | None -> AsyncEnvironmentStepper() :> AbstractEnvironmentStepper
#if __ANDROID__    
    member val StatsPrinted = false with get, set
#endif

    member this.StepsPerFrame 
        with get() : int = stepsPerFrame
        and set(value : int) = 
            if value <= 0 then
                failwithf "Number of steps must be > 0, %d given" value
            stepsPerFrame <- value

    member this.EnvironmentConfig 
        with get() : Agents.IDworkinEnvironmentConfig = environmentConfig
        and set(config : Agents.IDworkinEnvironmentConfig) =
            environmentConfig <- config
            this.ResetEnvironment(this.GetDimensions(this.WallpaperSize))
    
    member this.ApplyChangesInSettings() =
        framesAfterStop <- 0
        let changes = CurrentSettings.getChanges()
        if changes <> 0 || currentSettings <> !settingsRef then
            let previousSettings = currentSettings
            currentSettings <- !settingsRef
            let seedChanged = previousSettings.Seed <> currentSettings.Seed
            let getRandom() = System.Random(currentSettings.Seed) // use independent PRNGs for each setting
            let fgColor, bgColor = currentSettings.GetColors(getRandom()) 

            if seedChanged || (changes &&& int(SettingsChange.EnvironmentChanged) <> 0) then
                this.EnvironmentConfig <- currentSettings.Environment.Build(currentSettings.Seed)
            
            if changes &&& int(SettingsChange.EffectChanged) <> 0 || 
               (seedChanged && currentSettings.Effect.HasRandomizedValues) then
                this.Effect <- currentSettings.Effect.Build(platform, getRandom())
            
            if changes &&& int(SettingsChange.BackgroundChanged) <> 0 || 
               (seedChanged && (currentSettings.Background.HasRandomizedValues || currentSettings.BackgroundColor.IsRandomized)) then
                this.Background <- currentSettings.Background.Build(platform, getRandom(), bgColor)
            
            if changes &&& int(SettingsChange.ForegroundChanged) <> 0 || 
               (seedChanged && (currentSettings.Foreground.HasRandomizedValues || currentSettings.ForegroundColor.IsRandomized)) then
                this.Foreground <- currentSettings.Foreground.Build(platform, getRandom(), fgColor)
                
            if changes &&& int(SettingsChange.EffectChanged ||| SettingsChange.BackgroundChanged ||| SettingsChange.ForegroundChanged) <> 0 then
                this.Effect.SetMaterialProperties(
                    { Color=fgColor; Specular=this.Foreground.Specular; Roughness=this.Foreground.Roughness },
                    { Color=bgColor; Specular=this.Background.Specular; Roughness=this.Background.Roughness })
            
            CurrentSettings.resetChanges()

            this.ResetStencil()

    override this.ResetEnvironment(dimensions : Point) =
        framesAfterStop <- 0
        this.ClearSDF()
        TimeSamplers.resetAll()
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let env = environmentConfig.GetEnvironment(dimensions)
        // skip steps when nothing happens
        while not env.HasActiveAgents && env.Age < 1000 do
            env.Step() |> ignore
        stopWatch.Stop()
        this.EnvironmentStepper.Environment <- env 
#if __ANDROID__
        let dt = stopWatch.Elapsed
        Android.Util.Log.Debug("frameTime", sprintf "Setting up environment: %gms" dt.TotalMilliseconds) |> ignore
        this.StatsPrinted <- false
#else
        this.ResetStencil()
#endif

    override this.OnSurfaceChanged(width, height) =
        let contextLost =
#if __ANDROID__
            this.LastContext <> EGLContextFactory.CurrentContext
#else
            false
#endif
        if this.WallpaperSize <> (width, height) || (contextLost && width > 0 && height > 0) then
            base.OnSurfaceChanged(width, height)
        elif framesAfterStop > 0 then
            // Fixes issue of black screen after pause (e.g. waking after sleep). 
            // Value of -1 was determined experimentally
            framesAfterStop <- -1
    
    override this.OnDrawFrame() =
        let delay = 1000.0 / float(this.FPS)

        this.ApplyChangesInSettings()

        let mutable shapes = [||]
        let mutable steps = 0
        let mutable dt = 0.0
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()

        while this.EnvironmentStepper.HasMore && steps < this.StepsPerFrame do
            shapes <- 
                match shapes with
                | [||] -> this.EnvironmentStepper.FetchShapesForNextStep() 
                | _ -> Array.append shapes (this.EnvironmentStepper.FetchShapesForNextStep() )
            steps <- steps + 1
        stopwatch.Stop()
        dt <- stopwatch.Elapsed.TotalMilliseconds
        
        if not (Array.isEmpty shapes) then
            for _ in TimeSamplers.Shapes.Measure() do this.RenderShapes(shapes)
            for _ in TimeSamplers.Image.Measure() do this.DrawImage()
            dt <- dt + TimeSamplers.Shapes.Last + TimeSamplers.Image.Last
        elif framesAfterStop < 3 then // avoid flicker from triple buffering
            this.DrawImage()
            framesAfterStop <- framesAfterStop + 1
        
        if dt < delay then
            System.Threading.Thread.Sleep(System.TimeSpan.FromMilliseconds(delay - dt))
#if __ANDROID__
        if steps > 0 then
            Android.Util.Log.Debug("frameTime", 
                sprintf "env.: %7.6g; shapes: %7.6g; img: %7.6g"
                    (Array.sum <| TimeSamplers.Environment.GetLast(steps)) 
                    TimeSamplers.Shapes.Last
                    TimeSamplers.Image.Last ) |> ignore
        elif not this.StatsPrinted then
            Android.Util.Log.Debug("avgTimes", 
                sprintf "(mean)  env.: %7.6g; shapes: %7.6g; img: %7.6g"
                    (TimeStats.average TimeSamplers.Environment.Samples)
                    (TimeStats.average TimeSamplers.Shapes.Samples)
                    (TimeStats.average TimeSamplers.Image.Samples) ) |> ignore
            Android.Util.Log.Debug("avgTimes", 
                sprintf "(stdev) env.: %7.6g; shapes: %7.6g; img: %7.6g"
                    (TimeStats.stdev TimeSamplers.Environment.Samples)
                    (TimeStats.stdev TimeSamplers.Shapes.Samples)
                    (TimeStats.stdev TimeSamplers.Image.Samples) ) |> ignore
            this.StatsPrinted <- true
#else
        framesAfterStop <- 0 // always do redraw on PC
#endif