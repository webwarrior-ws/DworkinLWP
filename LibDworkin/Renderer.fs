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
    
    let getOrSetIfAbsent (dict : System.Collections.Generic.IDictionary<'K, 'V>) (key : 'K) (value : unit -> 'V) =
        match dict.TryGetValue(key) with
        | (true, v) -> v
        | (false, _) -> 
            let computedValue = value()
            dict.[key] <- computedValue
            computedValue
 
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
        failwith "notimplemented"
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
        GL.AttachShader(program, compile ShaderType.FragmentShader fragmentShader)
        GL.AttachShader(program, compile ShaderType.VertexShader vertexShader)
        GL.LinkProgram(program)
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
 

type BaseRenderer(platform : AbstractPlatform) =
#if __ANDROID__
    inherit Java.Lang.Object()
#endif
    let glVersion = platform.GLVersion

    // fields
    let mutable shapesVertices = Buffer<Vector2>(128)
    let mutable shapesVertexIndices = Buffer<int>(128)

    let mutable maskTexture = 0
    let mutable maskTextureMultisample = 0
    let mutable maskFrameBuffer = 0
    let mutable maskFrameBufferMultisample = 0
    let mutable maskRenderBuffer = 0
    let mutable quadVBO = 0
    let mutable maskedImageShader : Shader option = None
    let mutable shapesDrawingShader : Shader option = None
    let mutable shapesVBO = 0

    let mutable wallpaperWidth = 0
    let mutable wallpaperHeight = 0
    let mutable scale = Point(1.0f, 1.0f)
    let mutable canUseAA = glVersion >= (3, 1)

    member this.UsesAA = canUseAA

    member internal this.WallpaperSize = (wallpaperWidth, wallpaperHeight)
    member internal this.MaskTexture = maskTexture
    member internal this.MaskFrameBuffer = maskFrameBuffer
    member internal this.QuadVBO = quadVBO
#if __ANDROID__
    interface GLSurfaceView.IRenderer with
        member this.Dispose() = 
            GL.DeleteTexture maskTexture
            GL.DeleteFramebuffers(1, [| maskFrameBuffer |])
            if this.UsesAA then
                GL.DeleteTexture maskTextureMultisample
                GL.DeleteFramebuffers(1, [| maskFrameBufferMultisample |])
            GL.DeleteBuffers(2, [| quadVBO; shapesVBO |])
            GL.DeleteRenderbuffers(1, [| maskRenderBuffer |])
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
    interface ICanvas with
        member this.DrawTriangle(a, b, c) =
            let currIndex = shapesVertices.Size
            shapesVertices.Add a
            shapesVertices.Add b
            shapesVertices.Add c
            shapesVertexIndices.Add currIndex
            shapesVertexIndices.Add (currIndex + 1)
            shapesVertexIndices.Add (currIndex + 2)
        
        member this.DrawQuad(a, b, c, d) =
            let currIndex = shapesVertices.Size
            shapesVertices.Add a
            shapesVertices.Add b
            shapesVertices.Add c
            shapesVertices.Add d
            // triangle a b c
            shapesVertexIndices.Add currIndex
            shapesVertexIndices.Add (currIndex + 1)
            shapesVertexIndices.Add (currIndex + 2)
            // triangle c a d
            shapesVertexIndices.Add (currIndex + 2)
            shapesVertexIndices.Add currIndex
            shapesVertexIndices.Add (currIndex + 3)
        
        member this.DrawCircle(center, radius) =
            let numSteps = this.GetNumCircleSegments(radius)
            let firstIndex = shapesVertices.Size
            shapesVertices.Add center
            let angleStep = Single.TwoPi / (float32 numSteps)
            for i=1 to numSteps+1 do
                let angle = angleStep * (float32 i)
                shapesVertices.Add(center + Point(radius * cos angle, radius * sin angle))
                shapesVertexIndices.Add firstIndex
                shapesVertexIndices.Add (firstIndex + i)
                shapesVertexIndices.Add (firstIndex + (i % numSteps + 1))
        
        member this.DrawCircleContour(center, radius, width) =
            let numSteps = this.GetNumCircleSegments(radius)
            let angleStep = Single.TwoPi / (float32 numSteps)
            let r1 = radius - width*0.5f
            let r2 = radius + width*0.5f
            let mutable p1 = Point(1.0f, 0.0f)
            for i=1 to numSteps+1 do
                let angle = angleStep * (float32 i)
                let p2 = Point(cos angle, sin angle)
                (this :> ICanvas).DrawQuad(center + p1*r1, center + p1*r2, center + p2*r2, center + p2*r1)
                p1 <- p2
        
        member this.DrawPolygon(points) =
            let fisrtVertexIndex = shapesVertices.Size
            for p in points do shapesVertices.Add p
            for i=fisrtVertexIndex+1 to fisrtVertexIndex+points.Length-2 do
                shapesVertexIndices.Add fisrtVertexIndex
                shapesVertexIndices.Add i
                shapesVertexIndices.Add (i + 1)
        
        member this.DrawPolygonContour(points, width) =
            let n = points.Length

            let inline segment p1 p2 = Point.Normalize(p1 - p2)
            let offset (s1 : Point) (s2 : Point) =
                let offset = s1 - s2
                let factor = offset.NearestPointAlongLineFromTo(Point.Zero, s1.PerpendicularLeft).Length
                offset / factor * width * 0.5f 

            let mutable s1 = segment points.[0] points.[1]
            let mutable offset1 = offset s1 (segment points.[1] points.[2])
            for i=0 to points.Length do
                let p1 = points.[i%n]
                let p2 = points.[(i+1)%n]
                let s2 = segment p2 (points.[(i+2)%n])
                let offset2 = offset s1 s2
                if offset1.Angle > offset2.Angle then
                    (this :> ICanvas).DrawQuad(p1 - offset1, p1 + offset1, p2 + offset2, p2 - offset2)
                else
                    (this :> ICanvas).DrawQuad(p1 - offset1, p1 + offset1, p2 - offset2, p2 + offset2)
                s1 <- s2
                offset1 <- offset2
    
    
    member this.GetNumCircleSegments(radius: float32) =
        let size = radius * float32(min wallpaperWidth wallpaperHeight) / (min scale.X scale.Y)
        // account for DPI?
        max 6 ( min (int (size * 4.0f)) 120)
    
    /// initialize mask texture and framebuffer
    member private this.InitializeMask() =
        if maskTexture <> 0 then
            GL.DeleteTexture maskTexture 
        maskTexture <- gen1 GL.GenTextures
        GL.BindTexture(TextureTarget.Texture2D, maskTexture)
        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgb, wallpaperWidth, wallpaperHeight, 0, 
                      PixelFormat.Rgb, PixelType.UnsignedShort565, nativeint 0)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)

        maskFrameBuffer <- gen1 GL.GenFramebuffers
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, maskFrameBuffer)
        GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                TextureTarget.Texture2D, maskTexture, 0)      
        GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
        GL.Clear(ClearBufferMask.ColorBufferBit)

    abstract member OnSurfaceChanged : int * int -> unit
    default this.OnSurfaceChanged(width: int, height: int) = 
        // Set the OpenGL viewport to fill the entire surface. 
        GL.Viewport(0, 0, width, height)
        wallpaperWidth <- width
        wallpaperHeight <- height

        this.InitializeMask()

        if this.UsesAA then
            try
                if maskTextureMultisample <> 0 then
                    GL.DeleteTexture maskTextureMultisample
                maskTextureMultisample <- gen1 GL.GenTextures
                maskFrameBufferMultisample <- gen1 GL.GenFramebuffers
                let sampleCount = 2
#if __ANDROID__
                Graphics.ES31.GL.BindTexture(Graphics.ES31.All.Texture2DMultisample, maskTextureMultisample)
                
                Graphics.ES31.GL.TexStorage2DMultisample(Graphics.ES31.All.Texture2DMultisample, sampleCount, Graphics.ES31.All.Rgb8, 
                                                         wallpaperWidth, wallpaperHeight, true)                
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, maskFrameBufferMultisample)
                Graphics.ES31.GL.FramebufferTexture2D(Graphics.ES31.All.Framebuffer, Graphics.ES31.All.ColorAttachment0, 
                                                      Graphics.ES31.All.Texture2DMultisample, maskTextureMultisample, 0)
#else
                Graphics.OpenGL.GL.BindTexture(Graphics.OpenGL.TextureTarget.Texture2DMultisample, maskTextureMultisample)
                
                Graphics.OpenGL.GL.TexStorage2DMultisample(Graphics.OpenGL.TextureTargetMultisample2d.Texture2DMultisample, sampleCount, 
                                                           Graphics.OpenGL.SizedInternalFormat.Rgba8,  wallpaperWidth, wallpaperHeight, true)                
                GL.BindFramebuffer(FramebufferTarget.Framebuffer, maskFrameBufferMultisample)
                Graphics.OpenGL.GL.FramebufferTexture2D(Graphics.OpenGL.FramebufferTarget.Framebuffer, Graphics.OpenGL.FramebufferAttachment.ColorAttachment0, 
                                                      Graphics.OpenGL.TextureTarget.Texture2DMultisample, maskTextureMultisample, 0)
#endif
                GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
                GL.Clear(ClearBufferMask.ColorBufferBit)
            with 
                | :? System.EntryPointNotFoundException -> // no OpenGL ES 3.1 support
                    canUseAA <- false
                    GL.DeleteTexture maskTextureMultisample
                    if maskFrameBufferMultisample <> 0 then
                        GL.DeleteFramebuffers(1, [| maskFrameBufferMultisample |])

        GL.BindTexture(TextureTarget.Texture2D, 0)
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
    
    abstract member OnSurfaceCreated : unit -> unit
    default this.OnSurfaceCreated() = 
        GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
        
        maskedImageShader <- Some(Shader(platform.TextFromAsset "shaders/DrawMaskedImage.vert", platform.TextFromAsset "shaders/DrawMaskedImage.frag"))
        shapesDrawingShader <- Some(Shader(platform.TextFromAsset "shaders/DrawShapes.vert", platform.TextFromAsset "shaders/DrawShapes.frag"))
        
        quadVBO <- gen1 GL.GenBuffers
        GL.BindBuffer(BufferTarget.ArrayBuffer, this.QuadVBO)
        GL.BufferData(BufferTarget.ArrayBuffer, 
                      nativeint (fullScreenQuadVertices.Length * sizeof<float32>), 
                      fullScreenQuadVertices, 
                      BufferUsage.StaticDraw)
        
        shapesVBO <- gen1 GL.GenBuffers 

        GL.Disable(EnableCap.DepthTest)
        GL.Disable(EnableCap.CullFace)

    
    abstract member OnDrawFrame : unit -> unit
    default this.OnDrawFrame() =
        // Clear the rendering surface. 
        GL.Clear(ClearBufferMask.ColorBufferBit)
    
    member this.ClearMask() =
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, maskFrameBuffer)
        GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
        GL.Clear(ClearBufferMask.ColorBufferBit)

    member this.DrawMaskedImage() =
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
        GL.UseProgram(maskedImageShader.Value.Program)
        GL.ActiveTexture(TextureUnit.Texture0)
        GL.BindTexture(TextureTarget.Texture2D, maskTexture)
        GL.Uniform1(maskedImageShader.Value.GetUniformLocation("mask"), 0)
        GL.Uniform2(maskedImageShader.Value.GetUniformLocation("screenDimensions"), float32 wallpaperWidth, float32 wallpaperHeight)
        let aTexCoords = maskedImageShader.Value.GetAttributeLocation("texCoords")
        GL.BindBuffer(BufferTarget.ArrayBuffer, quadVBO)
        GL.EnableVertexAttribArray(aTexCoords)
        GL.VertexAttribPointer(aTexCoords, 2, VertexAttribPointerType.Float, false, 0, 0)

        GL.DrawArrays(BeginMode.Triangles, 0, 6)
        
        GL.DisableVertexAttribArray(aTexCoords)
        GL.BindTexture(TextureTarget.Texture2D, 0)
    
    member this.RenderShapes() =
        if shapesVertexIndices.Size > 0 then
            assert (shapesVertexIndices.Array |> Seq.take shapesVertexIndices.Size |> Seq.forall (fun i -> i >= 0 && i < shapesVertices.Size))
            
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, if this.UsesAA then maskFrameBufferMultisample else maskFrameBuffer)
            GL.Viewport(0, 0, wallpaperWidth, wallpaperHeight)
            GL.UseProgram(shapesDrawingShader.Value.Program)
            let aPos = shapesDrawingShader.Value.GetAttributeLocation("pos")
            GL.BindBuffer(BufferTarget.ArrayBuffer, shapesVBO)
            GL.BufferData(BufferTarget.ArrayBuffer, 
                          nativeint (shapesVertices.Size * sizeof<Vector2>), 
                          shapesVertices.Array, 
                          BufferUsage.StreamDraw)
            GL.EnableVertexAttribArray(aPos)
            GL.VertexAttribPointer(aPos, 2, VertexAttribPointerType.Float, false, 0, 0)
#if __ANDROID__            
            GL.DrawElements(BeginMode.Triangles, shapesVertexIndices.Size, DrawElementsType.UnsignedInt, shapesVertexIndices.Array)
#else        
            GL.DrawElements(PrimitiveType.Triangles, shapesVertexIndices.Size, DrawElementsType.UnsignedShort, shapesVertexIndices.Array)
#endif

            GL.DisableVertexAttribArray(aPos)
            
            shapesVertices.Reset()
            shapesVertexIndices.Reset()

            if this.UsesAA then
#if __ANDROID__            
                Graphics.ES31.GL.BindFramebuffer(Graphics.ES31.FramebufferTarget.ReadFramebuffer, maskFrameBufferMultisample)
                Graphics.ES31.GL.BindFramebuffer(Graphics.ES31.FramebufferTarget.DrawFramebuffer, maskFrameBuffer)
                Graphics.ES31.GL.BlitFramebuffer(0, 0, wallpaperWidth, wallpaperHeight, 0, 0, wallpaperWidth, wallpaperHeight, 
                                                 Graphics.ES31.ClearBufferMask.ColorBufferBit, Graphics.ES31.BlitFramebufferFilter.Nearest)
#else
                Graphics.OpenGL.GL.BindFramebuffer(Graphics.OpenGL.FramebufferTarget.ReadFramebuffer, maskFrameBufferMultisample)
                Graphics.OpenGL.GL.BindFramebuffer(Graphics.OpenGL.FramebufferTarget.DrawFramebuffer, maskFrameBuffer)
                Graphics.OpenGL.GL.BlitFramebuffer(0, 0, wallpaperWidth, wallpaperHeight, 0, 0, wallpaperWidth, wallpaperHeight, 
                                                 Graphics.OpenGL.ClearBufferMask.ColorBufferBit, Graphics.OpenGL.BlitFramebufferFilter.Nearest)
#endif
    
    /// Set scale used in vertex shader for drawing shapes
    member this.SetScale(xs : float32, ys : float32) =
        scale <- Point(xs, ys)
        GL.UseProgram(shapesDrawingShader.Value.Program)
        GL.Uniform2(shapesDrawingShader.Value.GetUniformLocation("scale"), xs, ys)
    
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


type DworkinRenderer(context, environmentConfig : Agents.IDworkinEnvironmentConfig) =
    inherit BaseRenderer(context)

    let mutable environment : Agents.DworkinEnvironment option = None
    
    override this.OnSurfaceChanged(width, height) =
        base.OnSurfaceChanged(width, height)
        // setup scale and offset
        let dimensions = this.GetDimensions(width, height)
        let env = environmentConfig.GetEnvironment(dimensions)
        environment <- Some(env)
        // skip steps when nothing happens
        while not env.HasActiveAgents && env.Age < 1000 do
            env.Step(this)
        
        this.SetScale(1.0f / dimensions.X, 1.0f / dimensions.Y)
        this.ClearMask()
    
    override this.OnDrawFrame() =
        match environment with
        | Some(env) -> env.Step(this)
        | None -> ()
        this.RenderShapes()
        this.DrawMaskedImage()
