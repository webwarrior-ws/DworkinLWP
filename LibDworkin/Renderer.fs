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
        let startTime = System.DateTime.Now
        action()
        System.DateTime.Now - startTime

 
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
    let mutable shapesVertices = Buffer<Vector2>(128)
    let mutable shapesVertexIndices = Buffer<int>(128)

    let mutable sdfTexture = 0
    let mutable sdfFrameBuffer = 0
    let mutable quadVBO = 0
    let mutable shapesVBO = 0
    let sdfVisualizerShader = lazy(new Shader(platform.TextFromAsset "shaders/sdfVisualizer.vert", platform.TextFromAsset "shaders/sdfVisualizer.frag"))
    let shapesDrawingShader = lazy(new Shader(platform.TextFromAsset "shaders/DrawShapes.vert", platform.TextFromAsset "shaders/DrawShapes.frag"))

    let mutable wallpaperWidth = 0
    let mutable wallpaperHeight = 0
    let mutable scale = Point(1.0f, 1.0f)

    member this.FPS = 30

    member internal this.WallpaperSize = (wallpaperWidth, wallpaperHeight)
    member internal this.SDFTexture = sdfTexture
    /// Use this to draw to sdf
    member internal this.SDFFrameBuffer = sdfFrameBuffer
    member internal this.QuadVBO = quadVBO
#if __ANDROID__
    interface GLSurfaceView.IRenderer with
        member this.Dispose() = 
            GL.DeleteTexture sdfTexture
            GL.DeleteFramebuffers(1, [| sdfFrameBuffer |])
            GL.DeleteBuffers(2, [| quadVBO; shapesVBO |])
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
    // shapes buliding (adding data to VBOs)
    member this.BuildCircle(center, radius) =
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
        
    member this.BuildPolygon(points : Point[]) =
        let fisrtVertexIndex = shapesVertices.Size
        for p in points do shapesVertices.Add p
        for i=fisrtVertexIndex+1 to fisrtVertexIndex+points.Length-2 do
            shapesVertexIndices.Add fisrtVertexIndex
            shapesVertexIndices.Add i
            shapesVertexIndices.Add (i + 1)
        
    member this.BuildPolygonContour(points : Point[], width : float32) =
        let n = points.Length

        let inline segment p1 p2 = Point.Normalize(p1 - p2)
        let offset (s1 : Point) (s2 : Point) : Point =
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
                this.BuildPolygon([| p1 - offset1; p1 + offset1; p2 + offset2; p2 - offset2 |])
            else
                this.BuildPolygon([| p1 - offset1; p1 + offset1; p2 - offset2; p2 + offset2 |])
            s1 <- s2
            offset1 <- offset2
    
    
    member this.GetNumCircleSegments(radius: float32) =
        let size = radius * float32(min wallpaperWidth wallpaperHeight) / (min scale.X scale.Y)
        // account for DPI?
        max 6 ( min (int (size * 4.0f)) 120)
    
    /// initialize sdf texture and framebuffer
    member private this.InitializeSDF() =
        if sdfTexture <> 0 then
            GL.DeleteTexture sdfTexture 
        sdfTexture <- gen1 GL.GenTextures
        GL.BindTexture(TextureTarget.Texture2D, sdfTexture)
        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgb, wallpaperWidth, wallpaperHeight, 0, 
                      PixelFormat.Rgb, PixelType.UnsignedByte, nativeint 0)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.ClampToEdge)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.ClampToEdge)

        sdfFrameBuffer <- gen1 GL.GenFramebuffers
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, sdfFrameBuffer)
        GL.FramebufferTexture2D(FramebufferTarget.Framebuffer, FramebufferSlot.ColorAttachment0, 
                                TextureTarget.Texture2D, sdfTexture, 0)      
        GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
        GL.Clear(ClearBufferMask.ColorBufferBit)

    abstract member OnSurfaceChanged : int * int -> unit
    default this.OnSurfaceChanged(width: int, height: int) = 
        // Set the OpenGL viewport to fill the entire surface. 
        GL.Viewport(0, 0, width, height)
        wallpaperWidth <- width
        wallpaperHeight <- height

        this.InitializeSDF()
        
        GL.BindTexture(TextureTarget.Texture2D, 0)
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
    
    abstract member OnSurfaceCreated : unit -> unit
    default this.OnSurfaceCreated() = 
        GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
        
        sdfVisualizerShader.Force() |> ignore
        shapesDrawingShader.Force() |> ignore
        
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
    
    member this.ClearSDF() =
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.SDFFrameBuffer)
        GL.ClearColor(1.0f, 1.0f, 0.0f, 1.0f)
        GL.Clear(ClearBufferMask.ColorBufferBit)

    member this.DrawImage() =
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
        GL.UseProgram(sdfVisualizerShader.Value.Program)
        GL.ActiveTexture(TextureUnit.Texture0)
        GL.BindTexture(TextureTarget.Texture2D, sdfTexture)
        GL.Uniform1(sdfVisualizerShader.Value.GetUniformLocation("sdf"), 0)
        let aTexCoords = sdfVisualizerShader.Value.GetAttributeLocation("texCoords")
        GL.BindBuffer(BufferTarget.ArrayBuffer, quadVBO)
        GL.EnableVertexAttribArray(aTexCoords)
        GL.VertexAttribPointer(aTexCoords, 2, VertexAttribPointerType.Float, false, 0, 0)

        GL.DrawArrays(BeginMode.Triangles, 0, 6)
        
        GL.DisableVertexAttribArray(aTexCoords)
        GL.BindTexture(TextureTarget.Texture2D, 0)
    
    member this.RenderShapes(shapes : ShapeAndPaint[]) =
        if not (Array.isEmpty shapes) then
            // step 0: build and draw shapes
            for struct (shape, paint) in shapes do
                match shape with
                | Circle(center, radius) ->
                    if paint.Fill then this.BuildCircle(center, radius)
                    if paint.StrokeWidth > 0.0f then this.BuildCircleContour(center, radius, paint.StrokeWidth)
                | Polygon(points) ->
                    if paint.Fill then this.BuildPolygon(points)
                    if paint.StrokeWidth > 0.0f then this.BuildPolygonContour(points, paint.StrokeWidth)

            assert (shapesVertexIndices.Array |> Seq.take shapesVertexIndices.Size |> Seq.forall (fun i -> i >= 0 && i < shapesVertices.Size))
            
            GL.BindFramebuffer(FramebufferTarget.Framebuffer, this.SDFFrameBuffer)
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
            Graphics.ES30.GL.DrawElements(Graphics.ES30.PrimitiveType.Triangles, shapesVertexIndices.Size, 
                                          Graphics.ES30.DrawElementsType.UnsignedInt, shapesVertexIndices.Array)
#endif

            GL.DisableVertexAttribArray(aPos)
            
            shapesVertices.Reset()
            shapesVertexIndices.Reset()

            // build outer shell and area of influence for each shape
            //...
    
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