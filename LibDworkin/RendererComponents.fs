module rec LibDworkin.RendererComponents

open System

open PharoCompatibility
open Geometry

open OpenTK.Graphics.ES20
open OpenTK

open Microsoft.FSharp.NativeInterop

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
            ResettableLazy<Shader>.ResetAll(context.JniIdentityHashCode)
            ResettableLazy<int>.ResetAll(context.JniIdentityHashCode) // textures
            if egl.EglDestroyContext(display, context) then
                currentContext <- Egl.EGL10.EglNoContext
    
    static member CurrentContext = currentContext

    static member val Instance = new EGLContextFactory()

    /// Global variable that indicates need to reset shader uniforms after they were modified from another OpenGL context
    static member val NeedsShaderUniformsReset = false with get, set
#endif


/// Like Lazy<'T>, but stores different values for different contexts and can "forget" computed value.
/// This class tracks its instances and can reset them all (for specific context or all contexts).
/// Context is defined by context marker (int).
type ResettableLazy<'T>(valueFactory : unit -> 'T) as this =
    static let allInstances = ResizeArray<WeakReference<ResettableLazy<'T>>>()

    let dict = SpecializedCollections.SmallDictionary<int, 'T>.Create()
    do
        allInstances.Add(WeakReference<ResettableLazy<'T>>(this))

    static member CurrentContextMarker : int = 
#if __ANDROID__
        EGLContextFactory.CurrentContext.JniIdentityHashCode
#else
        0
#endif

    member this.Value = dict.GetOrSetIfAbsent(ResettableLazy<_>.CurrentContextMarker, valueFactory)
    member this.IsValueCreated = dict.ContainsKey(ResettableLazy<_>.CurrentContextMarker)

    member this.Reset() =
        dict.Clear()

    member this.Reset(contextMarker : int) =
        dict.Remove(contextMarker) |> ignore
    
    static member ResetAll(?contextMarker : int) : unit =
        for each in allInstances do
            match each.TryGetTarget() with
            | true, inst -> 
                match contextMarker with
                | Some(marker) -> inst.Reset(marker) 
                | None -> inst.Reset()
            | false, _ -> ()


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

    /// Create OpenGL texture based on given image data. Return texture ID.
    let uploadTexture(pixels : nativeint, width, height) : int =
        let texture = GL.GenTexture()
        GL.BindTexture(TextureTarget.Texture2D, texture)
        GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgb, width, height, 0, PixelFormat.Rgb, PixelType.UnsignedByte, pixels)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, int TextureMinFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, int TextureMagFilter.Linear)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, int TextureWrapMode.Repeat)
        GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, int TextureWrapMode.Repeat)
        texture

    let create1x1Texture(color : Graphics.Color4) : int =
        let bytes = NativePtr.stackalloc<uint8> 3
        NativePtr.set bytes 0 (uint8(color.R * 255.0f))
        NativePtr.set bytes 1 (uint8(color.G * 255.0f))
        NativePtr.set bytes 2 (uint8(color.B * 255.0f))
        uploadTexture(NativePtr.toNativeInt bytes, 1, 1)

    // 1x1 textures
    let emptyAOMap = ResettableLazy<int>(fun () -> create1x1Texture(Graphics.Color4.White))
    let flatNormalMap = ResettableLazy<int>(fun () -> create1x1Texture(Graphics.Color4(0.5f, 0.5f, 1.0f, 1.0f)))

    let lightPosition = Vector2(0.4f, 0.25f)

    let reflectionMapTextureName = "Pmetal_003_mod.png"


#if __ANDROID__
module Matrix2 =
    let CreateRotation (angle : float32) : Matrix2 =
        let mutable rotation = Matrix2.Identity
        rotation.Rotate(MathHelper.RadiansToDegrees(angle))
        rotation

type Graphics.Color4 with
    static member ToHsv(rgb : Graphics.Color4) =
        let max = Math.Max(rgb.R, Math.Max(rgb.G, rgb.B));
        let min = Math.Min(rgb.R, Math.Min(rgb.G, rgb.B));
        let diff = max - min;

        let mutable h = 0.0f;
        if (diff = 0.0f) then 
            h <- 0.0f
        elif (max = rgb.R) then
            h <- ((rgb.G - rgb.B) / diff) % 6.0f
            if (h < 0.0f) then
                h <- h + 6.0f
        elif (max = rgb.G) then
            h <- ((rgb.B - rgb.R) / diff) + 2.0f
        elif (max = rgb.B) then
            h <- ((rgb.R - rgb.G) / diff) + 4.0f

        let hue = h * 60.0f / 360.0f

        let mutable saturation = 0.0f;
        if (max <> 0.0f) then
            saturation <- diff / max

        Vector4(hue, saturation, max, rgb.A)

    static member FromHsv(hsv : Vector4) =
        let hue = hsv.X * 360.0f
        let saturation = hsv.Y
        let value = hsv.Z

        let c = value * saturation

        let h = hue / 60.0f
        let x = c * (1.0f - Math.Abs((h % 2.0f) - 1.0f))

        let r, g, b =
            if (h >= 0.0f && h < 1.0f) then
                c, x, 0.0f
            elif (h >= 1.0f && h < 2.0f) then
                x, c, 0.0f
            elif (h >= 2.0f && h < 3.0f) then
                0.0f, c ,x
            elif (h >= 3.0f && h < 4.0f) then
                0.0f, x, c
            elif (h >= 4.0f && h < 5.0f) then
                x, 0.0f, c
            elif (h >= 5.0f && h < 6.0f) then
                c, 0.0f, x
            else
                0.0f, 0.0f, 0.0f

        let m = value - c;
        Graphics.Color4(r + m, g + m, b + m, hsv.W)
#endif


// NOTE: to get OpenGL version, use GL.GetString(StringName.Version) 
// OpenGL<space>ES<space><version number><space><vendor-specific information>
 
/// Platform-independent facade used by renderers to load shaders and other stuff
[<AbstractClass>]
type AbstractPlatform() =
    static let shaders = Collections.Generic.Dictionary<string * string, ResettableLazy<Shader>>()
    static let textures = Collections.Generic.Dictionary<string, ResettableLazy<int>>()

    abstract member TextFromAsset : string -> string
    abstract member LoadTextureFromAsset : string -> int

    member this.GetShader(vertexShaderName : string, fragmentShaderName : string) : ResettableLazy<Shader> =
        getOrSetIfAbsent 
            shaders
            (vertexShaderName, fragmentShaderName)
            (fun () -> 
                let readSource fname = this.TextFromAsset(IO.Path.Combine("shaders", fname))
                ResettableLazy(fun () -> 
#if __ANDROID__
                    Android.Util.Log.Debug(
                        "preview-render", 
                        sprintf "Load shader %s on thread#%A" fragmentShaderName System.Threading.Thread.CurrentThread.ManagedThreadId) 
                    |> ignore
#endif    
                    let s = new Shader(readSource vertexShaderName, readSource fragmentShaderName)          
                    s))        
    
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
        let path = IO.Path.Combine(assetsPath, "textures", filename)
        use bmp = new Drawing.Bitmap(path)
        let imgData = bmp.LockBits(Drawing.Rectangle(Drawing.Point(0, 0), bmp.Size), Drawing.Imaging.ImageLockMode.ReadOnly, Drawing.Imaging.PixelFormat.Format24bppRgb)
        let pixels = imgData.Scan0
        // swap R and B channels because System.Drawing.Bitmap contains data in BGR format
        let pixelsPtr : nativeptr<uint8> = NativePtr.ofNativeInt pixels
        for i in 0 .. 3 .. (imgData.Height * imgData.Stride - 1) do
            let r = NativePtr.get pixelsPtr i
            let b = NativePtr.get pixelsPtr (i+2)
            NativePtr.set pixelsPtr i b
            NativePtr.set pixelsPtr (i+2) r
        
        let texture = Helpers.uploadTexture(pixels, imgData.Width, imgData.Height)
        bmp.UnlockBits(imgData)
        texture
#endif


type Shader(vertexShader : string, fragmentShader : string) =
    // adapted code from OpenTK examples
    let status_code = ref 0
    let info = ref ""

    let compile (shaderType : ShaderType) source =
        let res = GL.CreateShader(shaderType)
        if res = 0 then
            failwith "GL.CreateShader() failed"
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

    member internal this.SetTexture(uniformName : string, textureSlot : TextureUnit, texture : int) =
        GL.ActiveTexture(textureSlot)
        GL.BindTexture(TextureTarget.Texture2D, texture)
        GL.Uniform1(this.Shader.GetUniformLocation(uniformName), int textureSlot - int TextureUnit.Texture0)

    abstract SetUniforms : unit -> unit
    default this.SetUniforms() = ()

    abstract Use: unit -> unit
    default this.Use() =
        assert(GL.IsProgram(this.Shader.Program))
        this.Shader.Use()        


/// Component that is responsible for rendering background/foreground textures
[<AbstractClass>]
type Material(shader : ResettableLazy<Shader>) = 
    inherit ShaderBasedComponent(shader)
    /// Scale as used in Renderer
    member val Scale = Geometry.Point(1.0f) with get, set

    override this.Use() =
        base.Use()
        this.SetUniforms()
    
    /// Average roughness value of material in range [0; 1]
    abstract member Roughness : float32
    default this.Roughness = 1.0f

    /// Specular color of material. White for non-metallic materials.
    abstract member Specular : Graphics.Color4
    default this.Specular = Graphics.Color4.White
    

type SolidColorMaterial(platform : AbstractPlatform, color : Graphics.Color4) =
    inherit Material(platform.GetShader("fullScreenQuad.vert", "materials/solidColor.frag"))

    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("color"), color)



/// Material defined by AO, Normal, Roughness maps
[<AbstractClass>]
type AbstractUniversalMaterial(platform : AbstractPlatform,
                               shader : ResettableLazy<Shader>, 
                               color : Graphics.Color4,
                               aoMap : string option,
                               normalMap : string option,
                               roughness : (string option * float32),
                               lightHeight : float32, 
                               scaleFactor : float32,
                               metalness : float32 option) =
    inherit Material(shader)

    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("color"), color)
        GL.Uniform1(this.Shader.GetUniformLocation("scaleFactor"), scaleFactor)
        // obtain all textures first so that OpenGL state is not messed up
        let reflectionTexture = platform.GetTexture(Helpers.reflectionMapTextureName).Value
        let aoTexture =
            match aoMap with
            | Some(textureName) -> platform.GetTexture(textureName).Value
            | None -> Helpers.emptyAOMap.Value 
        let normalTexture =
            match normalMap with
            | Some(textureName) -> platform.GetTexture(textureName).Value
            | None -> Helpers.flatNormalMap.Value
        let roughnessTexture = fst roughness |> Option.map (fun textureName -> platform.GetTexture(textureName).Value)
        // now set textures
        assert(GL.IsTexture(reflectionTexture))
        assert(GL.IsTexture(aoTexture))
        assert(GL.IsTexture(normalTexture))
        this.SetTexture("reflectionMap", TextureUnit.Texture0, reflectionTexture)
        this.SetTexture("aoMap", TextureUnit.Texture1, aoTexture)
        this.SetTexture("normalMap", TextureUnit.Texture2, normalTexture)
        match roughnessTexture with
        | Some(value) -> 
            this.SetTexture("roughnessMap", TextureUnit.Texture3, value)
            GL.Uniform1(this.Shader.GetUniformLocation("roughnessValue"), -1.0f)
        | None ->
            GL.Uniform1(this.Shader.GetUniformLocation("roughnessValue"), snd roughness)
        // specular color
        GL.Uniform4(this.Shader.GetUniformLocation("specularColor"), this.Specular)
        // toLight vector
        let mutable toLight = Vector3(Helpers.lightPosition.X, Helpers.lightPosition.Y, lightHeight)
        toLight.Normalize()
        GL.Uniform3(this.Shader.GetUniformLocation("toLight"), toLight)
        // screen dimensions
        GL.Uniform2(this.Shader.GetUniformLocation("screenDimensions"), Vector2(1.0f / this.Scale.X, 1.0f / this.Scale.Y))

    override this.Roughness = snd roughness
    
    override this.Specular =
        let m = defaultArg metalness 0.0f
        Graphics.Color4(lerp 1.0f color.R m, lerp 1.0f color.G m, lerp 1.0f color.B m, 1.0f)


type UniversalMaterial(platform : AbstractPlatform, 
                       color : Graphics.Color4,
                       aoMap : string option,
                       normalMap : string option,
                       roughness : (string option * float32),
                       lightHeight : float32, 
                       scaleFactor : float32,
                       ?metalness : float32) =
    inherit AbstractUniversalMaterial(platform,
                                      platform.GetShader("materials/universalMaterial.vert", "materials/universalMaterial.frag"),
                                      color,
                                      aoMap,
                                      normalMap,
                                      roughness,
                                      lightHeight, 
                                      scaleFactor,
                                      metalness)


/// Material defined by AO, Normal, Roughness maps
type TwoColorMaterial(platform : AbstractPlatform, 
                      color : Graphics.Color4,
                      colorDiffMap : string,
                      aoMap : string option,
                      normalMap : string option,
                      roughness : (string option * float32),
                      lightHeight : float32, 
                      scaleFactor : float32,
                      ?metalness : float32) =
    inherit AbstractUniversalMaterial(platform,
                                      platform.GetShader("materials/universalMaterial.vert", "materials/twoColorMaterial.frag"),
                                      color,
                                      aoMap,
                                      normalMap,
                                      roughness,
                                      lightHeight, 
                                      scaleFactor,
                                      metalness)

    override this.SetUniforms() =
        let colorDiffTexture = platform.GetTexture(colorDiffMap).Value
        base.SetUniforms()
        this.SetTexture("colorDiffMap", TextureUnit.Texture4, colorDiffTexture)
        let secondaryColor = 
            let mutable hsv = Graphics.Color4.ToHsv(color)
            hsv.X <- hsv.X - 0.01f
            hsv.Z <- hsv.Z * 0.58f
            Graphics.Color4.FromHsv(hsv)
        GL.Uniform4(this.Shader.GetUniformLocation("secondaryColor"), secondaryColor)


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

    override this.Specular = color
    override this.Roughness = 0.2f


/// Material properties that may be used by certain effects
type MaterialPropertiesForEffect =
    {
        Color : Graphics.Color4
        Specular : Graphics.Color4
        Roughness : float32
    }

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
    
    abstract member UsesStencil : bool
    default this.UsesStencil = false

    abstract member SetMaterialProperties : MaterialPropertiesForEffect * MaterialPropertiesForEffect -> unit
    default this.SetMaterialProperties(fgProps, bgProps) = 
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
    | Circular = 1
    | InverseCircular = 2
    | Sigmoid = 3

type BevelMode =
    | DownDown = 1
    | DownLevel = 2
    | DownUp = 3
    | LevelDown = 4
    | LevelUp = 5
    | UpDown = 6
    | UpLevel = 7
    | UpUp = 8

type BevelEffect(platform : AbstractPlatform, contrast : float32, maxDistance : float32, bevelProfile : BevelProfile, mode : BevelMode) =
    inherit Effect(platform.GetShader("effects/bevel.vert", "effects/bevel.frag"))

    static let profilesMap = ResettableLazy<int>(fun () ->
        let almostIdentity(x, n) = sqrt(x*x+n)
        let gain(x, k)  =
            let a = 0.5 * (2.0*(if x<0.5 then x else 1.0-x)) ** k
            if x<0.5 then a else 1.0-a
        let profileFuncs = [| 
            fun t -> Vector2(sqrt 2.0f, sqrt 2.0f);
            fun t -> // Circular
                let t = (1.0 - almostIdentity(1.0-t, 0.05)) * System.Math.PI / 2.0
                Vector2(float32(cos(t)), float32(sin(t)));
            fun t -> // InverseCircular
                let t = almostIdentity(t, 0.05) * System.Math.PI / 2.0
                Vector2(float32(sin(t)), float32(cos(t)));
            fun t -> // Sigmoid
                let k = 2.0
                let dx = 0.001
                let d = (gain(t+dx, k) - gain(t-dx, k)) / (2.0 * dx)
                Vector2(1.0f, float32 d).Normalized();
        |]
        let width = 32
        let textureData = Array2D.create profileFuncs.Length (width * 3) 0uy
        for y=0 to profileFuncs.Length-1 do
            for x=0 to width-1 do
                let p = profileFuncs.[y] (float(x) / float(width))
                textureData.[y, x*3] <- uint8(p.X * 255.0f)
                textureData.[y, x*3 + 1] <- uint8(p.Y * 255.0f)
        use ptr = fixed &textureData.[0,0]
        Helpers.uploadTexture (ptr |> NativePtr.toNativeInt, width, profileFuncs.Length) )

    let mutable fgRoughness = 1.0f
    let mutable bgRoughness = 1.0f
    let mutable fgSpecular = Graphics.Color4.White
    let mutable bgSpecular = Graphics.Color4.White

    member private this.SetRoughnessAndSpecularUniforms() =
        GL.Uniform1(this.Shader.GetUniformLocation("fgRoughness"), fgRoughness)
        GL.Uniform1(this.Shader.GetUniformLocation("bgRoughness"), bgRoughness)
        GL.Uniform4(this.Shader.GetUniformLocation("fgSpecular"), fgSpecular)
        GL.Uniform4(this.Shader.GetUniformLocation("bgSpecular"), bgSpecular)

    override this.UsesStencil = true

    override this.SetUniforms() =
        let outerSlope = 
            match mode with
            | BevelMode.DownDown | BevelMode.DownLevel | BevelMode.DownUp -> -1.0f
            | BevelMode.LevelDown | BevelMode.LevelUp -> 0.0f
            | _ -> 1.0f
        let innerSlope =
            match mode with
            | BevelMode.DownDown | BevelMode.LevelDown | BevelMode.UpDown -> -1.0f
            | BevelMode.DownLevel | BevelMode.UpLevel -> 0.0f
            | _ -> 1.0f
        let numBevelProfiles = 4 // see BevelProfile
        let lightHeight = 0.6f
        let toLight = Vector3((Helpers.lightPosition.X - 0.5f) *2.0f, (0.5f - Helpers.lightPosition.Y) *2.0f, lightHeight)
        GL.Uniform1(this.Shader.GetUniformLocation("contrast"), contrast)
        GL.Uniform1(this.Shader.GetUniformLocation("maxDistance"), maxDistance)
        GL.Uniform3(this.Shader.GetUniformLocation("toLight"), toLight)
        GL.Uniform1(this.Shader.GetUniformLocation("bevelProfileY"), float32(int(bevelProfile) + 1) / float32(numBevelProfiles + 1))
        GL.Uniform1(this.Shader.GetUniformLocation("outerSlope"), outerSlope)
        GL.Uniform1(this.Shader.GetUniformLocation("innerSlope"), innerSlope)
        this.SetRoughnessAndSpecularUniforms()
        this.SetTexture("reflectionMap", TextureUnit.Texture4, platform.GetTexture(Helpers.reflectionMapTextureName).Value)
        this.SetTexture("profilesMap", TextureUnit.Texture5, profilesMap.Value)

    override this.SetMaterialProperties(fgProps, bgProps) =
        base.SetMaterialProperties(fgProps, bgProps)
        fgRoughness <- fgProps.Roughness
        fgSpecular <- fgProps.Specular
        bgRoughness <- bgProps.Roughness
        bgSpecular <- bgProps.Specular
        this.SetRoughnessAndSpecularUniforms()


type HighlightEffect(platform : AbstractPlatform, lightPos : Geometry.Point, lightIntensity : float32) =
    inherit Effect(platform.GetShader("effects/highlight.vert", "effects/highlight.frag"))
    
    override this.SetUniforms() =
        GL.Uniform2(this.Shader.GetUniformLocation("lightPos"), lightPos)  
        GL.Uniform1(this.Shader.GetUniformLocation("lightIntensity"), lightIntensity)


type GlowInsideType =
    | Empty = 0
    | Glow = 1
    | Foreground = 2
    
type GlowEffect(platform : AbstractPlatform, glowStrength : float32, bloomPower : float32, insideType : GlowInsideType) =
    inherit Effect(platform.GetShader("fullScreenQuad.vert", "effects/glow.frag"))
    
    let mutable glowColor = Graphics.Color4.White

    override this.SetUniforms() =
        GL.Uniform4(this.Shader.GetUniformLocation("glowColor"), glowColor)  
        GL.Uniform1(this.Shader.GetUniformLocation("glowStrength"), glowStrength)  
        GL.Uniform1(this.Shader.GetUniformLocation("bloomPower"), bloomPower)
        GL.Uniform1(this.Shader.GetUniformLocation("insideType"), int insideType)

    override this.SetMaterialProperties(fgProps, bgProps) =
        base.SetMaterialProperties(fgProps, bgProps)
        glowColor <- fgProps.Color
        GL.Uniform4(this.Shader.GetUniformLocation("glowColor"), glowColor)


type FuzzyTextureEffect(platform : AbstractPlatform, 
                        noiseTexture : string, 
                        textureScale : float32, 
                        falloff : float32, 
                        absorbThreshold : float32, 
                        fill : float32) =
    inherit Effect(platform.GetShader("effects/fuzzyTexture.vert", "effects/fuzzyTexture.frag"))

    override this.SetUniforms() =
        GL.Uniform1(this.Shader.GetUniformLocation("scaleFactor"), textureScale)
        GL.Uniform1(this.Shader.GetUniformLocation("falloff"), falloff)
        GL.Uniform1(this.Shader.GetUniformLocation("absorbThreshold"), absorbThreshold)
        GL.Uniform1(this.Shader.GetUniformLocation("fill"), fill)
        this.SetTexture("noise", TextureUnit.Texture4, platform.GetTexture(noiseTexture).Value)

    
/// Use Measure() method in for loops to take measurement.
/// E.g.  for _ in sampler.Measure() do doSmth()
type TimeSampler(name : string) =
    let stopwatch = Diagnostics.Stopwatch()
    let samples = ResizeArray<float>() // (time in ms)

    member this.Measure() =
        this

    member this.Name = name
    member this.Samples = samples.ToArray()
    member this.Last = if samples.Length > 0 then samples.Last else 0.0
    member this.GetLast(n : int) = samples.GetRange(samples.Length - n, n).ToArray()
        
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
                samples.Add(stopwatch.Elapsed.TotalMilliseconds)
                stopwatch.Reset()
                false
            else
                stopwatch.Start()
                true
            
        member this.Reset(): unit = ()
    
    member this.Reset() = samples.Clear()


module TimeStats =
    let average (samples : float[]) = 
        if Array.isEmpty samples then 0.0
        else samples |> Array.average

    let max (samples : float[]) = 
        if Array.isEmpty samples then 0.0
        else samples |> Array.max

    let stdev (samples : float[]) = 
        if Array.isEmpty samples then 0.0
        else 
            let avg = average samples
            let squares = [| for x in samples -> (x - avg) * (x - avg) |]
            sqrt ((Array.sum squares) / (float samples.Length))

    let top (fraction : float) (samples : float[]) =
        if Array.isEmpty samples then 0.0
        else
            let sorted = samples |> Seq.sortDescending |> Seq.toArray
            let top = sorted.[0..int(ceil(float samples.Length * fraction))]
            top |> Array.average
    
    let add (a : float[]) (b : float[]) =
        Array.map2 (fun t1 t2 -> t1 + t2) a b

    let movingAverage (windowSize : int) (samples : float[]) =
        Array.windowed windowSize samples
        |> Array.map Array.average

    
module TimeSamplers =
    let Environment = new TimeSampler("env")
    let Shapes = new TimeSampler("shapes")
    let Image = new TimeSampler("img")

    let resetAll() =
        for each in [| Environment; Shapes; Image |] do each.Reset()


// Environment stepping

[<AutoOpen>]
module internal BlockingQueue =
    open System.Collections.Generic
    // code from https://docs.microsoft.com/en-us/previous-versions/visualstudio/visual-studio-2010/hh297096(v=vs.100)

    type private BlockingAgentMessage<'T> = 
        | Get of AsyncReplyChannel<'T>
        | Add of 'T * AsyncReplyChannel<unit>
    
    /// Agent-based implementation of producer/consumer problem 
    type BlockingQueueAgent<'T>(maxLength : int) =
        let queue = new Queue<'T>()

        let agent = MailboxProcessor.Start(fun agent ->
            // State machine running inside the agent
            let rec emptyQueue() = 
                agent.Scan(fun msg ->
                    match msg with 
                    | Add(value, reply) -> Some(enqueueAndContinue(value, reply))
                    | _ -> None )
            and fullQueue() = 
                agent.Scan(fun msg ->
                    match msg with 
                    | Get(reply) -> Some(dequeueAndContinue(reply))
                    | _ -> None )
            and runningQueue() = async {
                let! msg = agent.Receive() 
                match msg with 
                | Add(value, reply) -> return! enqueueAndContinue(value, reply)
                | Get(reply) -> return! dequeueAndContinue(reply) }
            and enqueueAndContinue (value, reply) = async {
                queue.Enqueue(value)
                reply.Reply() 
                return! chooseState() }
            and dequeueAndContinue (reply) = async { 
                reply.Reply(queue.Dequeue())
                return! chooseState() }
            and chooseState() = 
                if queue.Count = 0 then emptyQueue()
                elif queue.Count = maxLength then fullQueue()
                else runningQueue()

            // Start with an empty queue
            emptyQueue() )

        /// Asynchronously adds item to the queue. If the queue
        /// is full, it blocks until some items are removed.
        member x.AsyncAdd(v:'T) = 
            agent.PostAndAsyncReply(fun ch -> Add(v, ch))

        /// Asynchronously gets item from the queue. If the queue
        /// is empty, it blocks until some items are added.
        member x.AsyncGet() = 
            agent.PostAndAsyncReply(Get)
        
        member this.Length = queue.Count


[<AbstractClass>]
type AbstractEnvironmentStepper() =
    abstract member Environment : Agents.DworkinEnvironment with set
    default this.Environment 
        with set(env) = this.EnvironmentHolder <- Some(env)
    abstract member HasMore : bool
    abstract member FetchShapesForNextStep : unit -> AgentAbstractions.ShapeAndPaint[]

    member val internal EnvironmentHolder : Agents.DworkinEnvironment option = None with get, set

    member this.SpeedFactor 
        with get() : float32 =
            match this.EnvironmentHolder with
            | Some(env) -> env.SpeedFactor
            | None -> 1.0f
        and set(value : float32) = 
            match this.EnvironmentHolder with
            | Some(env) -> env.SpeedFactor <- value
            | None -> ()


type SimpleEnvironmentStepper() =
    inherit AbstractEnvironmentStepper()

    override this.HasMore = 
        match this.EnvironmentHolder with
        | Some(env) -> env.HasAgents
        | None -> false
    
    override this.FetchShapesForNextStep() =
        let mutable shapes = [||]
        for _ in TimeSamplers.Environment.Measure() do
            shapes <- this.EnvironmentHolder.Value.Step()
        shapes


type AsyncEnvironmentStepper() =
    inherit AbstractEnvironmentStepper()

    let maxQueueLength = 10
    
    let isStopped = ref false // TODO: see if start/stop functionality is really needed
    let environmentFinished = ref false
    let mutable queueAgent = new BlockingQueueAgent<AgentAbstractions.ShapeAndPaint[]>(maxQueueLength)
    let mutable cancelTokenSource : System.Threading.CancellationTokenSource = null

    member private this.StartStepping(env : Agents.DworkinEnvironment) =
        let shapesProducer =
            let rec loop () =
                async {
                    if not !isStopped then
                        if env.HasAgents then
                            let mutable shapes = [||]
                            for _ in TimeSamplers.Environment.Measure() do
                                shapes <- env.Step()
                            do! queueAgent.AsyncAdd(shapes)
                        else
                            isStopped := true
                            environmentFinished := true
                    else
                        do! Async.Sleep(10)
                    if not !environmentFinished then
                        do! loop ()
                }
            loop()
        cancelTokenSource <- new System.Threading.CancellationTokenSource()
        Async.Start(shapesProducer, cancelTokenSource.Token)
    
    override this.Environment
        with set(env) =  
            base.Environment <- env
            // empty queue and start
            isStopped := true
            if not <| isNull cancelTokenSource then
                cancelTokenSource.Cancel()
            queueAgent <- new BlockingQueueAgent<AgentAbstractions.ShapeAndPaint[]>(maxQueueLength)
            environmentFinished := false
            isStopped := false
            this.StartStepping(env)
    
    member this.IsStopped = !isStopped

    member this.QueueLength = queueAgent.Length

    override this.HasMore = 
        this.QueueLength > 0 || not (!environmentFinished || !isStopped)

    member this.Start() = 
        isStopped := false

    member this.Stop() =
        isStopped := true

    override this.FetchShapesForNextStep() =
        queueAgent.AsyncGet() |> Async.RunSynchronously


/// A kind of stencil buffer used to keep track of what parts of image need updating in effect shader.
/// Implemented as n x n grayscale 8-bit texture where value of a pixel 
/// is number of frames since region occupied by that pixel was updated.
type MakeshiftStencil(n : int) = 
    let tempBuffer : bool[,] = Array2D.zeroCreate n n
    let textureData : uint8[] = Array.zeroCreate (n*n)
    let mutable needsReset = false

    member this.TextureData = textureData

    member (*inline*) private this.FillRect(topLeft : Vector2, bottomRight : Vector2) =
        let inline clamp index =
            index |> max 0 |> min (n-1)
        for y=int(topLeft.Y) |> clamp to int(bottomRight.Y) |> clamp do
            for x=int(topLeft.X) |> clamp to int(bottomRight.X) |> clamp do
                tempBuffer.[y, x] <- true

    member this.Reset() =
        if needsReset then
            Array.fill textureData 0 (n*n) 0uy
            needsReset <- false

    member this.Update(shapes : AgentAbstractions.ShapeAndPaint[], scale : Vector2, maxDistance : float32) =
        let s = scale * float32(n)
        for each in shapes do
            match each with
            | struct(AgentAbstractions.Shape.Polygon(pts), _) -> 
                let mutable pMin = pts.[0]
                let mutable pMax = pts.[0]
                for i=1 to pts.Length-1 do
                    let p = pts.[i]
                    pMin <- Vector2.ComponentMin(pMin, p)
                    pMax <- Vector2.ComponentMax(pMax, p)
                this.FillRect(Vector2.Multiply(pMin - Vector2(maxDistance), s), Vector2.Multiply(pMax + Vector2(maxDistance), s))
            | struct(AgentAbstractions.Shape.Circle(center, radius), _) ->
                let pMin = center - Vector2(radius + maxDistance)
                let pMax = center + Vector2(radius + maxDistance)
                this.FillRect(Vector2.Multiply(pMin, s), Vector2.Multiply(pMax, s))
        for y=0 to n-1 do
            for x=0 to n-1 do
                let filled = tempBuffer.[y, x]
                let i = (n-1-y)*n + x
                if filled then
                    textureData.[i] <- 0uy
                else
                    textureData.[i] <- (min textureData.[i] 253uy) + 1uy
                tempBuffer.[y, x] <- false
        needsReset <- true
