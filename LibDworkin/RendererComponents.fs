module rec LibDworkin.RendererComponents

open System

open PharoCompatibility

open OpenTK.Graphics.ES20
open OpenTK

open Microsoft.FSharp.NativeInterop

#nowarn "9"
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

#if __ANDROID__
module Matrix2 =
    let CreateRotation (angle : float32) : Matrix2 =
        let mutable rotation = Matrix2.Identity
        rotation.Rotate(MathHelper.RadiansToDegrees(angle))
        rotation
#endif

// NOTE: to get OpenGL version, use GL.GetString(StringName.Version) 
// OpenGL<space>ES<space><version number><space><vendor-specific information>
 
/// Platform-independent facade used by renderers to load shaders and other stuff
[<AbstractClass>]
type AbstractPlatform() =
    let shaders = Collections.Generic.Dictionary<string * string, ResettableLazy<Shader>>()
    let textures = Collections.Generic.Dictionary<string, ResettableLazy<int>>()

    abstract member TextFromAsset : string -> string
    abstract member LoadTextureFromAsset : string -> int

    member this.GetShader(vertexShaderName : string, fragmentShaderName : string) : ResettableLazy<Shader> =
        getOrSetIfAbsent 
            shaders
            (vertexShaderName, fragmentShaderName)
            (fun () -> 
                let readSource fname = this.TextFromAsset(IO.Path.Combine("shaders", fname))
                ResettableLazy(fun () -> new Shader(readSource vertexShaderName, readSource fragmentShaderName)))        
    
    member this.GetTexture(filename : string) : ResettableLazy<int> =
        getOrSetIfAbsent
            textures
            filename
            (fun () -> ResettableLazy(fun () -> this.LoadTextureFromAsset(filename)))

#if __ANDROID__
 type AndroidPlatform(context : Android.Content.Context) =
    inherit AbstractPlatform()
    
    override this.TextFromAsset(filename : string) =
        use reader = new IO.StreamReader(context.Assets.Open(filename))
        reader.ReadToEnd()
    
    override this.LoadTextureFromAsset(filename : string) =
        use opts = new Android.Graphics.BitmapFactory.Options(InScaled=false)
        use stream = context.Assets.Open(IO.Path.Combine("textures", filename))
        use bitmap = Android.Graphics.BitmapFactory.DecodeStream(stream, new Android.Graphics.Rect(), opts)
        let texture = GL.GenTexture()
        GL.BindTexture(TextureTarget.Texture2D, texture)
        Android.Opengl.GLUtils.TexImage2D(Android.Opengl.GLES20.GlTexture2d, 0, bitmap, 0)
        bitmap.Recycle();
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
        texture
#else
type DotNETPlatform() =
    inherit AbstractPlatform()
    static let assetsPath = @"D:\Projects\DworkinLWP\LibDworkin\Assets\"
    
    override this.TextFromAsset(filename : string) =
        IO.File.ReadAllText(IO.Path.Combine(assetsPath, filename))
    
    override this.LoadTextureFromAsset(filename : string) =
        use bmp = new Drawing.Bitmap(IO.Path.Combine(assetsPath, "textures", filename))
        let imgData = bmp.LockBits(Drawing.Rectangle(Drawing.Point(0, 0), bmp.Size), Drawing.Imaging.ImageLockMode.ReadOnly, Drawing.Imaging.PixelFormat.Format24bppRgb)
        let pixels = imgData.Scan0
        // swap R and B channels
        let pixelsPtr : nativeptr<uint8> = NativePtr.ofNativeInt pixels
        for i in 0 .. 3 .. (imgData.Height * imgData.Stride - 1) do
            let r = NativePtr.get pixelsPtr i
            let b = NativePtr.get pixelsPtr (i+2)
            NativePtr.set pixelsPtr i b
            NativePtr.set pixelsPtr (i+2) r
        
        let texture = this.UploadTexture(pixels, imgData.Width, imgData.Height)
        bmp.UnlockBits(imgData)
        texture
    
    member private this.UploadTexture(pixels : nativeint, width, height) : int =
        let texture = GL.GenTexture()
        GL.BindTexture(TextureTarget.Texture2D, texture)
        // NOTE: UnsignedShort565 may be the only supported pixel type
        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgb, width, height, 0, PixelFormat.Rgb, PixelType.UnsignedByte, pixels)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
        texture
#endif

/// Like Lazy<'T>, but with ability to "forget" computed value.
/// This class tracks its instances and can reset them all.
type ResettableLazy<'T>(valueFactory : unit -> 'T) as this =
    static let allInstances = ResizeArray<WeakReference<ResettableLazy<'T>>>()

    let mutable lazyContainer = Lazy(valueFactory)
    do
        allInstances.Add(WeakReference<ResettableLazy<'T>>(this))

    member this.Value = lazyContainer.Value
    member this.IsValueCreated = lazyContainer.IsValueCreated

    member this.Reset() =
        lazyContainer <- Lazy(valueFactory)
    
    static member ResetAll() : unit =
        allInstances.RemoveAll(fun each -> 
            match each.TryGetTarget() with
            | true, inst -> 
                inst.Reset() 
                false
            | false, _ -> true )
        |> ignore


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
    
    let uniformLocations = Collections.Generic.Dictionary<string, int>()
    let attributeLocations = Collections.Generic.Dictionary<string, int>()

    let deferredActions = Collections.Concurrent.ConcurrentDictionary<int, unit -> unit>()
    
    member this.Program = program

    member this.Use() = 
        GL.UseProgram(program)
        if not deferredActions.IsEmpty then
            let actions = deferredActions.Values
            deferredActions.Clear()
            for action in actions do action()

    member this.GetUniformLocation(name : string) =
        getOrSetIfAbsent uniformLocations name (fun () -> GL.GetUniformLocation(this.Program, name))
    
    member this.GetAttributeLocation(name : string) =
        getOrSetIfAbsent attributeLocations name (fun () -> GL.GetAttribLocation(this.Program, name))
    
    // These methods defer setting uniform to the next time Use() method is called.
    // Do not use when setting samplers.
    member this.DeferredSetUniform(name : string, value : int) =
        let loc = this.GetUniformLocation(name)
        deferredActions.[loc] <- (fun () -> GL.Uniform1(loc, value))
    
    member this.DeferredSetUniform(name : string, value : float32) =
        let loc = this.GetUniformLocation(name)
        deferredActions.[loc] <- (fun () -> GL.Uniform1(loc, value))
    
    member this.DeferredSetUniform(name : string, value1 : float32, value2 : float32) =
        let loc = this.GetUniformLocation(name)
        deferredActions.[loc] <- (fun () -> GL.Uniform2(loc, value1, value2))
    
    interface IDisposable with
        member this.Dispose() = 
            GL.DeleteProgram(program)


[<AbstractClass>]
type ShaderBasedComponent(shader : ResettableLazy<Shader>) = 
    member internal this.Shader = shader.Value
    member this.ResetShader() = shader.Reset()

    abstract SetUniforms : unit -> unit
    default this.SetUniforms() = ()

    abstract Use: unit -> unit
    default this.Use() =
        this.Shader.Use()        


/// Component that is responsible for rendering background/foreground textures
[<AbstractClass>]
type Material(shader : ResettableLazy<Shader>) = 
    inherit ShaderBasedComponent(shader)
    /// Scale as used in Renderer
    member val internal Scale = Geometry.Point(1.0f) with get, set

    override this.Use() =
        base.Use()
        this.SetUniforms()
    

type SolidColorMaterial(platform : AbstractPlatform, color : Graphics.Color4) =
    inherit Material(platform.GetShader("fullScreenQuad.vert", "materials/solidColor.frag"))

    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("color"), color)


type NormalmapAndAOMaterial(platform : AbstractPlatform, 
                            color : Graphics.Color4, 
                            normalMap : string, 
                            lightPos : Geometry.Point, 
                            lightHeight : float32, 
                            scaleFactor : float32,
                            ao : (string * float32) option ) =
    inherit Material(platform.GetShader("materials/normalmap+ao.vert", "materials/normalmap+ao.frag"))

    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("color"), color)
        GL.Uniform2(this.Shader.GetUniformLocation("lightPos"), Vector2.Normalize(lightPos))
        GL.Uniform1(this.Shader.GetUniformLocation("lightHeight"), lightHeight)
        GL.Uniform1(this.Shader.GetUniformLocation("scaleFactor"), scaleFactor)
        // textures
        GL.ActiveTexture(TextureUnit.Texture0)
        GL.BindTexture(TextureTarget.Texture2D, platform.GetTexture(normalMap).Value)
        GL.Uniform1(this.Shader.GetUniformLocation("normalMap"), 0)
        match ao with
        | Some(filename, strength) ->
            GL.ActiveTexture(TextureUnit.Texture1)
            GL.BindTexture(TextureTarget.Texture2D, platform.GetTexture(filename).Value)
            GL.Uniform1(this.Shader.GetUniformLocation("aoMap"), 1)
            GL.Uniform1(this.Shader.GetUniformLocation("aoStrength"), strength)
        | None -> 
            GL.Uniform1(this.Shader.GetUniformLocation("aoStrength"), 0.0f)
        // screen dimensions
        GL.Uniform2(this.Shader.GetUniformLocation("screenDimensions"), Vector2(1.0f / this.Scale.X, 1.0f / this.Scale.Y))


type ColoredTextureMaterial(platform : AbstractPlatform, 
                            color : Graphics.Color4, 
                            shadeMap : string, 
                            flatShade : float32, 
                            contrast : float32,
                            scaleFactor : float32 ) =
    inherit Material(platform.GetShader("materials/coloredtexture.vert", "materials/coloredtexture.frag"))

    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("color"), color)
        GL.Uniform1(this.Shader.GetUniformLocation("flatShade"), flatShade)
        GL.Uniform1(this.Shader.GetUniformLocation("contrast"), contrast)
        GL.Uniform1(this.Shader.GetUniformLocation("scaleFactor"), scaleFactor)
        // texture
        GL.ActiveTexture(TextureUnit.Texture0)
        GL.BindTexture(TextureTarget.Texture2D, platform.GetTexture(shadeMap).Value)
        GL.Uniform1(this.Shader.GetUniformLocation("shadeMap"), 0)
        // screen dimensions
        GL.Uniform2(this.Shader.GetUniformLocation("screenDimensions"), 1.0f/this.Scale.X, 1.0f/this.Scale.Y)


type RoughSurfaceMaterial(platform : AbstractPlatform, color : Graphics.Color4, bumpHeight : float32, dim : float32, featureScale : float32, flatArea : float32) =
    inherit Material(platform.GetShader("fullScreenQuad.vert", "materials/roughSurface.frag"))
    
    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("color"), color)
        GL.Uniform1(this.Shader.GetUniformLocation("bumpHeight"), bumpHeight)
        GL.Uniform1(this.Shader.GetUniformLocation("dim"), dim)
        GL.Uniform1(this.Shader.GetUniformLocation("featureScale"), featureScale)
        GL.Uniform1(this.Shader.GetUniformLocation("flatArea"), flatArea)
        GL.Uniform2(this.Shader.GetUniformLocation("lightPos"), Vector2.Normalize(Vector2(0.5f, 0.75f)))
        GL.Uniform1(this.Shader.GetUniformLocation("lightHeight"), 1.0f)
        GL.Uniform2(this.Shader.GetUniformLocation("scale"), this.Scale)


type MetalMaterial(platform : AbstractPlatform, color : Graphics.Color4, highlight : float32, scale : float32, offset : Vector2, angle : float32, stretch : float32,
                   strokeFreq : float32, strokeLength : float32, strokeIntensity : float32, strokeAngle : float32) =
    inherit Material(platform.GetShader("fullScreenQuad.vert", "materials/metal.frag"))
    
    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("color"), color)
        GL.Uniform1(this.Shader.GetUniformLocation("highlight"), highlight)
        GL.Uniform1(this.Shader.GetUniformLocation("scale"), scale)
        GL.Uniform2(this.Shader.GetUniformLocation("offset"), offset)
        GL.Uniform1(this.Shader.GetUniformLocation("stretch"), stretch)
        GL.Uniform1(this.Shader.GetUniformLocation("strokeFreq"), strokeFreq)
        GL.Uniform1(this.Shader.GetUniformLocation("strokeLength"), strokeLength)
        GL.Uniform1(this.Shader.GetUniformLocation("strokeIntensity"), strokeIntensity)
        let rotation = Matrix2.CreateRotation(LibDworkin.Geometry.deg2rad angle)
        let strokeRotation = Matrix2.CreateRotation(LibDworkin.Geometry.deg2rad strokeAngle)
        GL.UniformMatrix2(this.Shader.GetUniformLocation("rotation"), false, ref rotation)
        GL.UniformMatrix2(this.Shader.GetUniformLocation("strokeRotation"), false, ref strokeRotation)
        GL.Uniform1(this.Shader.GetUniformLocation("aspectRatio"), this.Scale.Y / this.Scale.X)


type WoodMaterial(platform : AbstractPlatform, color : Graphics.Color4, angle : float32, ringsScale : float32, ringsStretch : float32, contrast : float32) =
    inherit Material(platform.GetShader("fullScreenQuad.vert", "materials/wood.frag"))
    
    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("color"), color)
        GL.Uniform1(this.Shader.GetUniformLocation("ringsScale"), ringsScale)
        GL.Uniform1(this.Shader.GetUniformLocation("ringsStretch"), ringsStretch)
        GL.Uniform1(this.Shader.GetUniformLocation("contrast"), contrast)
        let rotation = Matrix2.CreateRotation(LibDworkin.Geometry.deg2rad angle)
        GL.UniformMatrix2(this.Shader.GetUniformLocation("rotation"), false, ref rotation)
        GL.Uniform2(this.Shader.GetUniformLocation("screenDimensions"), 1.0f/this.Scale.X, 1.0f/this.Scale.Y)
        // grain texture
        GL.ActiveTexture(TextureUnit.Texture0)
        GL.BindTexture(TextureTarget.Texture2D, platform.GetTexture("Wood_grain.png").Value)
        GL.Uniform1(this.Shader.GetUniformLocation("grainTexture"), 0)


/// Component that is responsible for drawing image using SDF, background, and foreground textures.
/// No two Effect subclasses should share a shader.
[<AbstractClass>]
type Effect(shader : ResettableLazy<Shader>) =
    inherit ShaderBasedComponent(shader)
    let mutable wasUsed = false
    let mutable screenDimensions = Geometry.Point()

    member this.ScreenDimensions 
        with get() = screenDimensions
        and set(value) = 
            screenDimensions <- value
            wasUsed <- false
    
    override this.Use() =
        let needsInit = not shader.IsValueCreated || not wasUsed
        base.Use()
        if needsInit then 
            this.Shader.DeferredSetUniform("screenDimensions", this.ScreenDimensions.X, this.ScreenDimensions.Y)        
            this.SetUniforms()
        wasUsed <- true

/// Effect used for debugging. Doesn't use background or foreground
type SDFVisualizerEffect(platform : AbstractPlatform, frequency : float32) =
    inherit Effect(platform.GetShader("fullScreenQuad.vert", "sdfVisualizer.frag"))
    
    override this.SetUniforms() =
        GL.Uniform1(this.Shader.GetUniformLocation("frequency"), frequency)


type PlainEffect(platform : AbstractPlatform) =
    inherit Effect(platform.GetShader("fullScreenQuad.vert", "effects/plain.frag"))


type ShadowEffect(platform : AbstractPlatform, 
                  shadowDir : Vector2, 
                  shadowOffset : float32, 
                  shadowSize : float32, 
                  shadowSoftness : float32, 
                  shadowStrength : float32,
                  inner : bool) =
    inherit Effect(platform.GetShader("fullScreenQuad.vert", "effects/shadow.frag"))

    override this.SetUniforms() =
        GL.Uniform2(this.Shader.GetUniformLocation("shadowDir"), Vector2.Normalize(shadowDir))
        GL.Uniform1(this.Shader.GetUniformLocation("shadowOffset"), shadowOffset)
        GL.Uniform1(this.Shader.GetUniformLocation("shadowSize"), shadowSize)
        GL.Uniform1(this.Shader.GetUniformLocation("shadowSoftness"), shadowSoftness)
        GL.Uniform1(this.Shader.GetUniformLocation("shadowStrength"), shadowStrength)
        GL.Uniform1(this.Shader.GetUniformLocation("inner"), if inner then 1 else 0)


type OutlineEffect(platform : AbstractPlatform, outlineWidth : float32) =
    inherit Effect(platform.GetShader("fullScreenQuad.vert", "effects/outline.frag"))

    override this.SetUniforms() =
        GL.Uniform1(this.Shader.GetUniformLocation("outlineWidth"), outlineWidth)


type BevelProfile =
    | Linear = 0
    | Hyperbolic = 1
    | Parabola1 = 2
    | Parabola2 = 3
    | Exp = 4

type BevelMode =
    | Inner = 0
    | Outer = 1
    | Ridge = 2
    | Trench = 3

type BevelEffect(platform : AbstractPlatform, contrast : float32, maxDistance : float32, lightPos : Geometry.Point, bevelProfile : BevelProfile, mode : BevelMode) =
    inherit Effect(platform.GetShader("effects/bevel.vert", "effects/bevel.frag"))

    override this.SetUniforms() =
        GL.Uniform1(this.Shader.GetUniformLocation("contrast"), contrast)
        GL.Uniform1(this.Shader.GetUniformLocation("maxDistance"), maxDistance)
        GL.Uniform2(this.Shader.GetUniformLocation("lightPos"), Vector2.Normalize(lightPos))
        GL.Uniform1(this.Shader.GetUniformLocation("lightHeight"), 1.0f)
        GL.Uniform1(this.Shader.GetUniformLocation("bevelProfile"), int bevelProfile)
        GL.Uniform1(this.Shader.GetUniformLocation("mode"), int mode)


type HighlightEffect(platform : AbstractPlatform, lightPos : Geometry.Point, lightIntensity : float32) =
    inherit Effect(platform.GetShader("effects/highlight.vert", "effects/highlight.frag"))
    
    override this.SetUniforms() =
        GL.Uniform2(this.Shader.GetUniformLocation("lightPos"), lightPos)  
        GL.Uniform1(this.Shader.GetUniformLocation("lightIntensity"), lightIntensity)


type GlowType =
    | Outer = 0
    | Inner = 1
    
type GlowEffect(platform : AbstractPlatform, glowColor : Graphics.Color4, glowStrength : float32, glowType : GlowType) =
    inherit Effect(platform.GetShader("fullScreenQuad.vert", "effects/glow.frag"))
    
    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("glowColor"), glowColor)  
        GL.Uniform1(this.Shader.GetUniformLocation("glowStrength"), glowStrength)  
        GL.Uniform1(this.Shader.GetUniformLocation("glowType"), int glowType)

    
type TimeMeasurementSample = int * float
    
/// Use Measure() method in for loops to take measurement.
/// E.g.  for _ in sampler.Measure(0) do doSmth()
type TimeSampler(name : string) =
    let stopwatch = Diagnostics.Stopwatch()
    let samples = ResizeArray<TimeMeasurementSample>() // (agents count, time in ms)
        
    let mutable currAgents = 0

    member this.Measure(agentsCount) =
        currAgents <- agentsCount
        this

    member this.Name = name
    member this.Samples = samples.ToArray()
    member this.Last = samples.Last
    member this.LastTime = snd samples.Last
        
    interface Collections.Generic.IEnumerable<TimeSampler> with
        member this.GetEnumerator() : Collections.Generic.IEnumerator<TimeSampler> = this :> Collections.Generic.IEnumerator<TimeSampler>
        member this.GetEnumerator() : Collections.IEnumerator = this :> Collections.IEnumerator
        
    interface Collections.Generic.IEnumerator<TimeSampler> with
        member this.Current: TimeSampler = this
        member this.Current: obj = (this :> Collections.Generic.IEnumerator<TimeSampler>).Current :> obj
        member this.Dispose(): unit = ()

        member this.MoveNext(): bool = 
            if stopwatch.IsRunning then
                stopwatch.Stop()
                samples.Add(currAgents, stopwatch.Elapsed.TotalMilliseconds)
                stopwatch.Reset()
                false
            else
                stopwatch.Start()
                true
            
        member this.Reset(): unit = ()


module TimeStats =
    let average (samples : TimeMeasurementSample[]) = 
        samples |> Seq.averageBy snd

    let max (samples : TimeMeasurementSample[]) = 
        samples |> Seq.map snd |> Seq.max

    let stdev (samples : TimeMeasurementSample[]) = 
        let avg = average samples
        let squares = [ for (_, x) in samples -> (x - avg) * (x - avg) ]
        sqrt ((Seq.sum squares) / (float samples.Length))

    let top (fraction : float) (samples : TimeMeasurementSample[]) =
        let sorted = samples |> Seq.map snd |> Seq.sortDescending |> Seq.toArray
        let top = sorted.[0..int(ceil(float samples.Length * fraction))]
        top |> Array.average
    
    let add (a : TimeMeasurementSample[]) (b : TimeMeasurementSample[]) =
        Array.map2 (fun (count, t1) (_, t2) -> (count, t1+t2)) a b
    
module TimeSamplers =
    let Environment = new TimeSampler("env")
    let Shapes = new TimeSampler("shapes")
    let Image = new TimeSampler("img")
