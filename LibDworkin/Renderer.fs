module LibDworkin.Renderer

open Geometry
open SpecializedCollections
open AgentAbstractions

open Microsoft.FSharp.NativeInterop

open Android.Opengl
open Javax.Microedition.Khronos

open OpenTK.Graphics.ES20
open OpenTK


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
 

type Shader(vertexShader, fragmentShader) =
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
    
    member this.Program = program


type BaseRenderer(context : Android.Content.Context) =
    inherit Java.Lang.Object()

    // fields
    let mutable shapesVertices = Buffer<Vector2>(128)
    let mutable shapesVertexIndices = Buffer<int>(128)

    let mutable maskTexture = 0
    let mutable maskFrameBuffer = 0
    let mutable maskRenderBuffer = 0
    let mutable quadVBO = 0
    let mutable maskedImageShader : Shader option = None
    let mutable shapesDrawingShader : Shader option = None
    let mutable shapesVBO = 0

    let mutable wallpaperWidth = 0
    let mutable wallpaperHeight = 0
    let mutable scale = Point(1.0f, 1.0f)

    member internal this.WallpaperSize = (wallpaperWidth, wallpaperHeight)
    member internal this.MaskTexture = maskTexture
    member internal this.MaskFrameBuffer = maskFrameBuffer
    member internal this.QuadVBO = quadVBO

    member this.TextFromAsset(filename : string) =
        use reader = new System.IO.StreamReader(context.Assets.Open(filename))
        reader.ReadToEnd()

    interface GLSurfaceView.IRenderer with
        member this.Dispose() = 
            GL.DeleteTexture maskTexture
            GL.DeleteFramebuffers(1, [| maskFrameBuffer |])
            GL.DeleteBuffers(2, [| quadVBO; shapesVBO |])
            GL.DeleteRenderbuffers(1, [| maskRenderBuffer |])
            base.Dispose()

        member this.Handle: nativeint = 
            base.Handle

        member this.OnSurfaceChanged(gl: Opengles.IGL10, width: int, height: int) = 
            this.OnSurfaceChanged(gl, width, height)

        member this.OnSurfaceCreated(gl: Opengles.IGL10, config: Egl.EGLConfig) = 
            this.OnSurfaceCreated(gl, config)

        member this.OnDrawFrame(gl : Opengles.IGL10) =
            this.OnDrawFrame(gl)
    
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

    abstract member OnSurfaceChanged : Opengles.IGL10 * int * int -> unit
    default this.OnSurfaceChanged(gl: Opengles.IGL10, width: int, height: int) = 
        // Set the OpenGL viewport to fill the entire surface. 
        GL.Viewport(0, 0, width, height)
        wallpaperWidth <- width
        wallpaperHeight <- height

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

        GL.BindTexture(TextureTarget.Texture2D, 0)
        GL.BindFramebuffer(FramebufferTarget.Framebuffer, 0)
    
    abstract member OnSurfaceCreated : Opengles.IGL10 * Egl.EGLConfig -> unit
    default this.OnSurfaceCreated(gl: Opengles.IGL10, config: Egl.EGLConfig) = 
        GL.ClearColor(0.0f, 0.0f, 0.0f, 1.0f)
        
        maskedImageShader <- Some(Shader(this.TextFromAsset "shaders/DrawMaskedImage.vert", this.TextFromAsset "shaders/DrawMaskedImage.frag"))
        shapesDrawingShader <- Some(Shader(this.TextFromAsset "shaders/DrawShapes.vert", this.TextFromAsset "shaders/DrawShapes.frag"))
        
        quadVBO <- gen1 GL.GenBuffers
        GL.BindBuffer(BufferTarget.ArrayBuffer, this.QuadVBO)
        GL.BufferData(BufferTarget.ArrayBuffer, 
                      nativeint (fullScreenQuadVertices.Length * sizeof<float32>), 
                      fullScreenQuadVertices, 
                      BufferUsage.StaticDraw)
        
        shapesVBO <- gen1 GL.GenBuffers 

        GL.Disable(EnableCap.DepthTest)
        GL.Disable(EnableCap.CullFace)

    
    abstract member OnDrawFrame : Opengles.IGL10 -> unit
    default this.OnDrawFrame(gl : Opengles.IGL10) =
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
        GL.Uniform1(GL.GetUniformLocation(maskedImageShader.Value.Program, "mask"), 0)
        GL.Uniform2(GL.GetUniformLocation(maskedImageShader.Value.Program, "screenDimensions"), float32 wallpaperWidth, float32 wallpaperHeight)
        let aTexCoords = GL.GetAttribLocation(maskedImageShader.Value.Program, "texCoords")
        GL.BindBuffer(BufferTarget.ArrayBuffer, quadVBO)
        GL.EnableVertexAttribArray(aTexCoords)
        GL.VertexAttribPointer(aTexCoords, 2, VertexAttribPointerType.Float, false, 0, 0)
        
        GL.DrawArrays(BeginMode.Triangles, 0, 6)
        
        GL.DisableVertexAttribArray(aTexCoords)
        GL.BindTexture(TextureTarget.Texture2D, 0)
    
    member this.RenderShapes() =
        if shapesVertexIndices.Size > 0 then
            assert (shapesVertexIndices.Array |> Seq.take shapesVertexIndices.Size |> Seq.forall (fun i -> i >= 0 && i < shapesVertices.Size))

            GL.BindFramebuffer(FramebufferTarget.Framebuffer, maskFrameBuffer)
            GL.Viewport(0, 0, wallpaperWidth, wallpaperHeight)
            GL.UseProgram(shapesDrawingShader.Value.Program)
            let aPos = GL.GetAttribLocation(shapesDrawingShader.Value.Program, "pos")
            GL.BindBuffer(BufferTarget.ArrayBuffer, shapesVBO)
            GL.BufferData(BufferTarget.ArrayBuffer, 
                          nativeint (shapesVertices.Size * sizeof<Vector2>), 
                          shapesVertices.Array, 
                          BufferUsage.StreamDraw)
            GL.EnableVertexAttribArray(aPos)
            GL.VertexAttribPointer(aPos, 2, VertexAttribPointerType.Float, false, 0, 0)
            
            GL.DrawElements(BeginMode.Triangles, shapesVertexIndices.Size, DrawElementsType.UnsignedInt, shapesVertexIndices.Array)

            GL.DisableVertexAttribArray(aPos)
            
            shapesVertices.Reset()
            shapesVertexIndices.Reset()
    
    member this.SetScale(xs : float32, ys : float32) =
        scale <- Point(xs, ys)
        GL.UseProgram(shapesDrawingShader.Value.Program)
        GL.Uniform2(GL.GetUniformLocation(shapesDrawingShader.Value.Program, "scale"), xs, ys)


type DworkinRenderer(context, environmentConfig : Agents.IDworkinEnvironmentConfig) =
    inherit BaseRenderer(context)

    let mutable environment : Agents.DworkinEnvironment option = None
    
    override this.OnSurfaceChanged(gl, width, height) =
        base.OnSurfaceChanged(gl, width, height)
        // setup scale and offset
        environment <- Some(environmentConfig.GetEnvironment(Point(float32 width, float32 height)))
        this.SetScale(1.0f / float32 width, 1.0f / float32 height)
        this.ClearMask()
    
    override this.OnDrawFrame(gl : Opengles.IGL10) =
        match environment with
        | Some(env) -> env.Step(this)
        | None -> ()
        this.RenderShapes()
        this.DrawMaskedImage()
