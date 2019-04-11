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


// NOTE: to get OpenGL version, use GL.GetString(StringName.Version) 
// OpenGL<space>ES<space><version number><space><vendor-specific information>
 
/// Platform-independent facade used by renderers to load shaders and other stuff
[<AbstractClass>]
type AbstractPlatform() =
    let shaders = Collections.Generic.Dictionary<string * string, ResettableLazy<Shader>>()

    abstract member TextFromAsset : string -> string

    member internal this.GetShader(vertexShaderName : string, fragmentShaderName : string) : ResettableLazy<Shader> =
        getOrSetIfAbsent 
            shaders
            (vertexShaderName, fragmentShaderName)
            (fun () -> 
                let readSource fname = this.TextFromAsset(IO.Path.Combine("shaders", fname))
                ResettableLazy(fun () -> new Shader(readSource vertexShaderName, readSource fragmentShaderName)))        

#if __ANDROID__
 type AndroidPlatform(context : Android.Content.Context) =
    inherit AbstractPlatform()
    override this.TextFromAsset(filename : string) =
        use reader = new IO.StreamReader(context.Assets.Open(filename))
        reader.ReadToEnd()
#else
type DotNETPlatform() =
    inherit AbstractPlatform()
    override this.TextFromAsset(filename : string) =
        IO.File.ReadAllText(IO.Path.Combine(@"D:\Projects\DworkinLWP\LibDworkin\Assets\", filename))
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
