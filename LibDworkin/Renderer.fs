module LibDworkin.Renderer

open OpenTK.Graphics.ES20
open OpenTK

open Geometry
open PharoCompatibility
open SpecializedCollections
open AgentAbstractions

open Microsoft.FSharp.NativeInterop

#if __ANDROID__
open Android.Opengl
open Javax.Microedition.Khronos
#endif


#nowarn "9"
[<AutoOpen>]
module Helpers =
    /// Wrapper around GL.Generate<...> functions that lets you return result explicitly.
    let gen1 (generatingFunction : int * nativeptr<int> -> unit) : int = 
        let output = NativePtr.stackalloc<int> 1
        generatingFunction(1, output)
        NativePtr.read output
    
    let fullScreenQuadVertices = [| 
        -1.0f; -1.0f; 
         1.0f; -1.0f; 
         1.0f;  1.0f; 
         1.0f;  1.0f; 
        -1.0f;  1.0f;
        -1.0f; -1.0f;|]
    
    let measureTime (action : unit -> unit) =
        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        action()
        stopwatch.Stop()
        stopwatch.Elapsed
    
    let setDefaultTexParameters(terxtureTarget : TextureTarget) = 
        GL.TexParameter(terxtureTarget, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        GL.TexParameter(terxtureTarget, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        GL.TexParameter(terxtureTarget, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        GL.TexParameter(terxtureTarget, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)

 
/// Platform-independent facade used by renderers to load shaders and other stuff
[<AbstractClass>]
type AbstractPlatform() =
    abstract member TextFromAsset : string -> string
    abstract member GLVersion : int*int

#if __ANDROID__
 type AndroidPlatform(context : Android.Content.Context) =
    inherit AbstractPlatform()
    override this.TextFromAsset(filename : string) =
        use reader = new System.IO.StreamReader(context.Assets.Open(filename))
        reader.ReadToEnd()
    
    override this.GLVersion =
        let activityManager = context.GetSystemService(Android.Content.Context.ActivityService) :?> Android.App.ActivityManager
        let rawVersion = activityManager.DeviceConfigurationInfo.ReqGlEsVersion
        (rawVersion >>> 16, rawVersion &&& 0xffff)
#else
type NETCorePlatform() =
    inherit AbstractPlatform()
    override this.TextFromAsset(filename : string) =
        System.IO.File.ReadAllText(@"D:\Projects\DworkinLWP\LibDworkin\Assets\" + filename)
    override this.GLVersion = 
        (3, 1) // for now
#endif

type Shader(vertexShader : string, fragmentShader : string) =
    // adapted code from OpenTK examples
    let status_code = ref 0
    let info = ref ""

    let compile (shaderType : ShaderType) source =
        let res = GL.CreateShader(shaderType)
        GL.ShaderSource(res, source)
        GL.CompileShader(res)
        GL.GetShaderInfoLog(res, info)
        GL.GetShader(res, ShaderParameter.CompileStatus, status_code)
        if !status_code <> 1 then failwith !info
        res
    
    let program = GL.CreateProgram()
    do
        let vs = compile ShaderType.VertexShader vertexShader
        let fs = compile ShaderType.FragmentShader fragmentShader
        GL.AttachShader(program, vs)
        GL.AttachShader(program, fs)
        GL.LinkProgram(program)
        GL.DeleteShader(vs)
        GL.DeleteShader(fs)
        GL.GetProgramInfoLog(program, info)
        GL.GetProgram(program, ProgramParameter.LinkStatus, status_code)

        if !status_code <> 1 then failwith !info
    
    let uniformLocations = System.Collections.Generic.Dictionary<string, int>()
    let attributeLocations = System.Collections.Generic.Dictionary<string, int>()
    
    member this.Program = program

    member this.GetUniformLocation(name : string) =
        getOrSetIfAbsent uniformLocations name (fun () -> GL.GetUniformLocation(this.Program, name))
    
    member this.GetAttributeLocation(name : string) =
        getOrSetIfAbsent attributeLocations name (fun () -> GL.GetAttribLocation(this.Program, name))
    
    interface System.IDisposable with
        member this.Dispose() = 
            GL.DeleteProgram(program)
 

type BaseRenderer(platform : AbstractPlatform) =
#if __ANDROID__
    inherit Java.Lang.Object()
#endif
    // fields
    let mutable shapesVertices = Buffer<Vector3>(128)
    let mutable shapesVerticesAlphas = Buffer<float32>(256)
    let mutable shapesVertexIndices = Buffer<int>(128)
    let mutable aoiVertexIndices = Buffer<int>(128)

    let mutable sdfTextures = (0, 0)
    let mutable sdfFrameBuffers = (0, 0)
    let mutable sdfDepthBuffer = 0
    let mutable quadVBO = 0
    let mutable shapesVBO = 0
    let mutable alphasVBO = 0
    let sdfVisualizerShader = lazy(new Shader(platform.TextFromAsset "shaders/sdfVisualizer.vert", platform.TextFromAsset "shaders/sdfVisualizer.frag"))
    let shapesDrawingShader = lazy(new Shader(platform.TextFromAsset "shaders/DrawShapes.vert", platform.TextFromAsset "shaders/DrawShapes.frag"))
    let calcPositiveDistShader = lazy(new Shader(platform.TextFromAsset "shaders/outerShell.vert", platform.TextFromAsset "shaders/positiveDist.frag"))
    let calcNegativeDistShader = lazy(new Shader(platform.TextFromAsset "shaders/areaOfInfluence.vert", platform.TextFromAsset "shaders/negativeDist.frag"))

    let mutable wallpaperWidth = 0
    let mutable wallpaperHeight = 0
    let mutable scale = Point(1.0f, 1.0f)

    // maximal absolute values of distances in signed distance field
    member this.MaxDistance = 16.0f

    member this.FPS = 30

    member internal this.WallpaperSize = (wallpaperWidth, wallpaperHeight)
    /// Use this to draw to sdf
    member internal this.SDFFrameBuffer = fst sdfFrameBuffers
    member internal this.SDFTexture = fst sdfTextures
    member internal this.QuadVBO = quadVBO
    member internal this.CalcPositiveDistShader = calcPositiveDistShader.Value
    member internal this.CalcNegativeDistShader = calcNegativeDistShader.Value
#if __ANDROID__
    interface GLSurfaceView.IRenderer with
        member this.Dispose() = 
            GL.DeleteTextures(2, [| fst sdfTextures; snd sdfTextures |])
            GL.DeleteRenderbuffers(1, [| sdfDepthBuffer |])
            GL.DeleteFramebuffers(2, [| fst sdfFrameBuffers; snd sdfFrameBuffers |])
            GL.DeleteBuffers(2, [| quadVBO; shapesVBO; alphasVBO |])
            let disposeShader (shader : Lazy<Shader>) =
                if shader.IsValueCreated then
                    (shader.Value :> System.IDisposable).Dispose()
            [| sdfVisualizerShader; shapesDrawingShader |] 
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
    member this.BuildCircle(center : Point, radius : float32) =
        let numSteps = this.GetNumCircleSegments(radius)
        let firstIndex = shapesVertices.Size
        shapesVertices.Add(Vector3(center))
        let angleStep = Single.TwoPi / (float32 numSteps)
        for i=1 to numSteps+1 do
            let angle = angleStep * (float32 i)
            shapesVertices.Add(Vector3(center + Point(radius * cos angle, radius * sin angle)))
            shapesVertexIndices.Add firstIndex
            shapesVertexIndices.Add (firstIndex + i)
            shapesVertexIndices.Add (firstIndex + (i % numSteps + 1))
        
    member this.BuildCircleContour(center, radius, width) =
        let numSteps = this.GetNumCircleSegments(radius)
        let angleStep = Single.TwoPi / (float32 numSteps)
        let r1 = radius - width*0.5f
        let r2 = radius + width*0.5f
        let mutable p1 = Point(1.0f, 0.0f)
        for i=1 to numSteps+1 do
            let angle = angleStep * (float32 i)
            let p2 = Point(cos angle, sin angle)
            this.BuildPolygon([| center + p1*r1; center + p1*r2; center + p2*r2; center + p2*r1 |])
            p1 <- p2
        
    member private this.BuildPolygon(points : 'a[], mapper : 'a -> Vector3) =
        let fisrtVertexIndex = shapesVertices.Size
        for p in points do shapesVertices.Add(mapper p)
        for i=fisrtVertexIndex+1 to fisrtVertexIndex+points.Length-2 do
            shapesVertexIndices.Add fisrtVertexIndex
            shapesVertexIndices.Add i
            shapesVertexIndices.Add (i + 1)
    
    member this.BuildPolygon(points : Point[]) =
        this.BuildPolygon(points, Vector3)
    
    member this.BuildPolygon(points : Vector3[]) =
        this.BuildPolygon(points, id)
    
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
    
    /// initialize sdf texture and framebuffer
    member private this.InitializeSDF() =
        let textures = [| fst sdfTextures; snd sdfTextures |]
        let frameBuffers = [| fst sdfFrameBuffers; snd sdfFrameBuffers |]
        if sdfTextures <> (0, 0) then GL.DeleteTextures(textures.Length, textures) 
        if sdfFrameBuffers <> (0, 0) then GL.DeleteFramebuffers(frameBuffers.Length, frameBuffers) 
        GL.GenTextures(textures.Length, textures)
        GL.GenFramebuffers(frameBuffers.Length, frameBuffers)
        if sdfDepthBuffer <> 0 then GL.DeleteRenderbuffers(1, [| sdfDepthBuffer |])
        sdfDepthBuffer <- gen1 GL.GenRenderbuffers
        GL.BindRenderbuffer(RenderbufferTarget.Renderbuffer, sdfDepthBuffer)
        GL.RenderbufferStorage(RenderbufferTarget.Renderbuffer, RenderbufferInternalFormat.DepthComponent16, wallpaperWidth, wallpaperHeight)
        
        Array.iter2 
            (fun (texture : int) (frameBuffer : int) -> 
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, frameBuffer)
                GL.FramebufferRenderbuffer(FramebufferTarget.Framebuffer, FramebufferSlot.DepthAttachment, RenderbufferTarget.Renderbuffer, sdfDepthBuffer)
                GL.BindTexture(TextureTarget.Texture2D, texture)
                GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgb, wallpaperWidth, wallpaperHeight, 0, 
                              PixelFormat.Rgb, PixelType.UnsignedByte, nativeint 0)
                setDefaultTexParameters(TextureTarget.Texture2D)
                GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                        TextureTarget.Texture2D, texture, 0)
                GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
                GL.ClearDepth(this.MaxDistance)
                GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit))
            textures
            frameBuffers
        
        sdfTextures <- (textures.[0], textures.[1])
        sdfFrameBuffers <- (frameBuffers.[0], frameBuffers.[1])

    abstract member OnSurfaceChanged : int * int -> unit
    default this.OnSurfaceChanged(width: int, height: int) = 
        // Set the OpenGL viewport to fill the entire surface. 
        GL.Viewport(0, 0, width, height)
        wallpaperWidth <- width
        wallpaperHeight <- height

        this.InitializeSDF()
        
        GL.BindTexture(TextureTarget.Texture2D, 0)
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)

        GL.UseProgram(this.CalcPositiveDistShader.Program)
        GL.Uniform2(this.CalcPositiveDistShader.GetUniformLocation("screenDimensions"), float32 width, float32 height)
        GL.UseProgram(this.CalcNegativeDistShader.Program)
        GL.Uniform2(this.CalcNegativeDistShader.GetUniformLocation("screenDimensions"), float32 width, float32 height)
    
    abstract member OnSurfaceCreated : unit -> unit
    default this.OnSurfaceCreated() = 
        GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
        
        sdfVisualizerShader.Force() |> ignore
        shapesDrawingShader.Force() |> ignore
        GL.UseProgram(this.CalcPositiveDistShader.Program)
        GL.Uniform1(this.CalcPositiveDistShader.GetUniformLocation("h"), this.MaxDistance)
        
        quadVBO <- gen1 GL.GenBuffers
        GL.BindBuffer(BufferTarget.ArrayBuffer, this.QuadVBO)
        GL.BufferData(BufferTarget.ArrayBuffer, 
                      nativeint (fullScreenQuadVertices.Length * sizeof<float32>), 
                      fullScreenQuadVertices, 
                      BufferUsage.StaticDraw)
        
        shapesVBO <- gen1 GL.GenBuffers 
        alphasVBO <- gen1 GL.GenBuffers 

        GL.DepthRange(0.0f, this.MaxDistance)
        GL.DepthFunc(DepthFunction.Less)
        GL.Disable(EnableCap.DepthTest)
        GL.Disable(EnableCap.CullFace)
    
    abstract member OnDrawFrame : unit -> unit
    default this.OnDrawFrame() =
        // Clear the rendering surface. 
        GL.Clear(ClearBufferMask.ColorBufferBit)
    
    member this.ClearSDF() =
        for frameBuffer in [| fst sdfFrameBuffers; snd sdfFrameBuffers |] do
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, frameBuffer)
            GL.ClearColor(1.0f, 1.0f, 0.0f, 1.0f)
            GL.ClearDepth(this.MaxDistance)
            GL.Clear(ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit)

    member this.DrawImage() =
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
        GL.UseProgram(sdfVisualizerShader.Value.Program)
        GL.ActiveTexture(TextureUnit.Texture0)
        GL.BindTexture(TextureTarget.Texture2D, this.SDFTexture)
        GL.Uniform1(sdfVisualizerShader.Value.GetUniformLocation("sdf"), 0)
        let aTexCoords = sdfVisualizerShader.Value.GetAttributeLocation("texCoords")
        GL.BindBuffer(BufferTarget.ArrayBuffer, quadVBO)
        GL.EnableVertexAttribArray(aTexCoords)
        GL.VertexAttribPointer(aTexCoords, 2, VertexAttribPointerType.Float, false, 0, 0)

        GL.DrawArrays(BeginMode.Triangles, 0, 6)
        
        GL.DisableVertexAttribArray(aTexCoords)
        GL.BindTexture(TextureTarget.Texture2D, 0)
    
    member this.BuildOuterShellAndAOI(points : Point[]) =
        let n = points.Length
        let baseIndex = shapesVertices.Size
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
            let OD = (OA + OC).Normalized() * (h / cosAOD)
            let OE = (OB + OC).Normalized() * (h / cosAOD)

            let vertices = [| vec3(O, 0.0f); vec3(O + OA, h); vec3(O + OD, h); vec3(O + OC, h); vec3(O + OE, h); vec3(O + OB, h) |]
            let currIndex = shapesVertices.Size
            this.BuildPolygon(vertices)
            shapesVerticesAlphas.Add 0.0f // O
            shapesVerticesAlphas.Add 0.0f // A
            shapesVerticesAlphas.Add (cosAOD |> float |> System.Math.Acos |> float32) // D
            shapesVerticesAlphas.Add 0.0f // C
            shapesVerticesAlphas.Add (cosAOD |> float |> System.Math.Acos |> float32) // E
            shapesVerticesAlphas.Add 0.0f // B
            
            for j=1 to vertices.Length do
                aoiVertexIndices.Add(baseIndex + 1)
                aoiVertexIndices.Add(currIndex + j)
                aoiVertexIndices.Add(currIndex + j + 1)
            
            let currIndex = shapesVertices.Size // not a duplicate definition, more like update
            this.BuildPolygon([| vec3(O, 0.0f); vec3(O + OB, h); vec3(O2 + OB, h); vec3(O2, 0.0f) |])
            for _j=1 to 4 do
                shapesVerticesAlphas.Add 0.0f

            aoiVertexIndices.Add(baseIndex + 1)
            aoiVertexIndices.Add(currIndex + 1)
            aoiVertexIndices.Add(currIndex + 2) )

    member this.RenderShapes(shapes : ShapeAndPaint[]) =
        // Update sdf (signed distance field) texture.
        // Distances >= 0 are stored in R channel; [absolute values of] distances < 0 are stored in B channel.
        // Distances are in range [-maxDistance; maxDistance]. In texture, distance values are scaled to take all available range (e.g. 0-255). 
        // maxDistance is defined as a multiple of unit distance in agent environment coordinates (may not be the same distance in screen space coordinates).
        if not (Array.isEmpty shapes) then
            GL.Enable(EnableCap.DepthTest)
            
            let polygons, circles = 
                shapes 
                |> Array.partition (function | struct (Polygon(_), _) -> true | struct (Circle(_), _) -> false)
            // Step 0: build and draw shapes.
            for struct (Polygon(points), paint) in polygons do
                if paint.Fill then this.BuildPolygon(points)
                if paint.StrokeWidth > 0.0f then this.BuildPolygonContour(points, paint.StrokeWidth)

            assert (shapesVertexIndices.Array |> Seq.take shapesVertexIndices.Size |> Seq.forall (fun i -> i >= 0 && i < shapesVertices.Size))

            this.TransferBufferData(shapesVertices, shapesVBO, BufferTarget.ArrayBuffer)
            
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.SDFFrameBuffer)
            GL.UseProgram(shapesDrawingShader.Value.Program)
            let aPos = shapesDrawingShader.Value.GetAttributeLocation("pos")
            GL.EnableVertexAttribArray(aPos)
            GL.VertexAttribPointer(aPos, 3, VertexAttribPointerType.Float, false, 0, 0)

            this.PerformDrawElements(shapesVertexIndices)
            
            shapesVertices.Reset()
            shapesVertexIndices.Reset()

            // Build outer shell and area of influence for each shape.
            for struct (Polygon(points), paint) in polygons do
                if paint.Fill then 
                    this.BuildOuterShellAndAOI(points)
                if paint.StrokeWidth > 0.0f then 
                    // for now build  outer shell and aoi for each contour segment individually - not optimal, but works
                    let contourSegmentQuads = this.GetQuadsForPolygonContour(points, paint.StrokeWidth)
                    for quadPoints in contourSegmentQuads do
                        this.BuildOuterShellAndAOI(quadPoints)
            
            // Step 1: calculate [positive] distances outside of shapes.
            this.TransferBufferData(shapesVerticesAlphas, alphasVBO, BufferTarget.ArrayBuffer)
            GL.UseProgram(calcPositiveDistShader.Value.Program)
            let aAlpha = calcPositiveDistShader.Value.GetAttributeLocation("inAlpha")
            GL.EnableVertexAttribArray(aAlpha)
            GL.VertexAttribPointer(aAlpha, 1, VertexAttribPointerType.Float, false, 0, 0)

            this.TransferBufferData(shapesVertices, shapesVBO, BufferTarget.ArrayBuffer)
            let aPos = calcPositiveDistShader.Value.GetAttributeLocation("pos")
            GL.EnableVertexAttribArray(aPos)
            GL.VertexAttribPointer(aPos, 3, VertexAttribPointerType.Float, false, 0, 0)
            
            this.PerformDrawElements(shapesVertexIndices)
            GL.Disable(EnableCap.DepthTest)
            
            // Step 2: calculate [negative] distances inside of shapes in area of influence.
            // Since one cannot read from and write to the same texture, 
            // two pairs of FBO and texture are used and are swapped between horizontal and vertical passes.
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, snd sdfFrameBuffers)
            GL.ActiveTexture(TextureUnit.Texture0)
            GL.BindTexture(TextureTarget.Texture2D, fst sdfTextures)
            GL.UseProgram(calcNegativeDistShader.Value.Program)
            GL.Uniform1(calcNegativeDistShader.Value.GetUniformLocation("sdf"), 0)
            let aPos = calcNegativeDistShader.Value.GetAttributeLocation("pos")
            GL.EnableVertexAttribArray(aPos)
            GL.VertexAttribPointer(aPos, 3, VertexAttribPointerType.Float, false, 0, 0)

            GL.Uniform1(calcNegativeDistShader.Value.GetUniformLocation("horizontal"), 1) // horizontal=true
            this.PerformDrawElements(aoiVertexIndices)

            // swap framebuffers and textures
            GL.BindTexture(TextureTarget.Texture2D, snd sdfTextures)
            GL.Uniform1(calcNegativeDistShader.Value.GetUniformLocation("sdf"), 0)
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, fst sdfFrameBuffers)
            
            GL.Uniform1(calcNegativeDistShader.Value.GetUniformLocation("horizontal"), 0) // horizontal=false
            this.PerformDrawElements(aoiVertexIndices)
            
            this.ClearBuffers()

    member this.ClearBuffers() =
        shapesVertices.Reset()
        shapesVertexIndices.Reset()
        shapesVerticesAlphas.Reset()
        aoiVertexIndices.Reset()

    /// Set scale used in vertex shader for drawing shapes
    member this.SetScale(xs : float32, ys : float32) =
        scale <- Point(xs, ys)
        GL.UseProgram(shapesDrawingShader.Value.Program)
        GL.Uniform2(shapesDrawingShader.Value.GetUniformLocation("scale"), xs, ys)
        GL.UseProgram(calcPositiveDistShader.Value.Program)
        GL.Uniform2(calcPositiveDistShader.Value.GetUniformLocation("scale"), xs, ys)
        GL.UseProgram(this.CalcNegativeDistShader.Program)
        GL.Uniform2(calcNegativeDistShader.Value.GetUniformLocation("scale"), xs, ys)
        GL.Uniform1(this.CalcNegativeDistShader.GetUniformLocation("h"), this.MaxDistance * float32(wallpaperWidth) * xs) // account for scale
    
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


type DworkinRenderer(platform, environmentConfig : Agents.IDworkinEnvironmentConfig) =
    inherit BaseRenderer(platform)

    let mutable environmentConfig = environmentConfig
    let mutable environment : Agents.DworkinEnvironment option = None

    member this.EnvironmentConfig 
        with get() : Agents.IDworkinEnvironmentConfig = environmentConfig
        and set(config : Agents.IDworkinEnvironmentConfig) =
            environmentConfig <- config
            this.ResetEnvironment(this.GetDimensions(this.WallpaperSize))
    
    member this.ResetEnvironment(dimensions : Point) =
        this.ClearSDF()
        let startTime = System.DateTime.Now
        let env = environmentConfig.GetEnvironment(dimensions)
        environment <- Some(env)
        let dt = System.DateTime.Now - startTime
#if __ANDROID__
        Android.Util.Log.Debug("frameTime", sprintf "Setting up environment: %gms" dt.TotalMilliseconds) |> ignore
#endif
        // skip steps when nothing happens
        while not env.HasActiveAgents && env.Age < 1000 do
            env.Step() |> ignore

    override this.OnSurfaceChanged(width, height) =
        base.OnSurfaceChanged(width, height)
        // setup scale and offset
        let dimensions = this.GetDimensions(width, height)
        this.ResetEnvironment(dimensions)
        
        this.SetScale(1.0f / dimensions.X, 1.0f / dimensions.Y)
    
    override this.OnDrawFrame() =
        let delay = 1000.0 / float(this.FPS)

        let mutable agentsCount = 0
        let mutable shapes = [||]
        let envStepTime = 
            measureTime (fun () -> 
                match environment with
                | Some(env) -> 
                    agentsCount <- env.AgentsCount
                    shapes <- env.Step()
                | None -> () )
        let renderShapesTime = measureTime (fun () -> this.RenderShapes(shapes))
        let drawImageTime = measureTime this.DrawImage

        let dt = envStepTime + renderShapesTime + drawImageTime
        if dt.TotalMilliseconds < delay then
            System.Threading.Thread.Sleep(System.TimeSpan.FromMilliseconds(delay - dt.TotalMilliseconds))
#if __ANDROID__
        //else
        Android.Util.Log.Debug("frameTime", 
            sprintf "env.(%3d agents): %7.6g; shapes: %7.6g; img: %7.6g"
                agentsCount
                envStepTime.TotalMilliseconds 
                renderShapesTime.TotalMilliseconds
                drawImageTime.TotalMilliseconds ) |> ignore
#endif