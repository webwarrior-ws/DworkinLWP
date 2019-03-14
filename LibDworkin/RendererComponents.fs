module LibDworkin.RendererComponents

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
    abstract member TextFromAsset : string -> string

#if __ANDROID__
 type AndroidPlatform(context : Android.Content.Context) =
    inherit AbstractPlatform()
    override this.TextFromAsset(filename : string) =
        use reader = new System.IO.StreamReader(context.Assets.Open(filename))
        reader.ReadToEnd()
#else
type DotNETPlatform() =
    inherit AbstractPlatform()
    override this.TextFromAsset(filename : string) =
        System.IO.File.ReadAllText(@"D:\Projects\DworkinLWP\LibDworkin\Assets\" + filename)
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

    
/// Use Measure() method in for loops to take measurement.
/// E.g.  for _ in sampler.Measure(0) do doSmth()
type TimeSampler(name : string) =
    let stopwatch = System.Diagnostics.Stopwatch()
    let samples = ResizeArray<int * float>() // (agents count, time in ms)
        
    let mutable currAgents = 0

    member this.Measure(agentsCount) =
        currAgents <- agentsCount
        this

    member this.Name = name
    member this.Samples = samples.ToArray()
    member this.Last = samples.Last
    member this.LastTime = snd samples.Last

    member this.Average = samples |> Seq.averageBy snd
    member this.Max = samples |> Seq.maxBy snd
    member this.Stdev =
        let avg = this.Average
        let squares = [ for (_, x) in samples -> (x - avg) * (x - avg) ]
        sqrt ((Seq.sum squares) / (float samples.Count))
        
    interface System.Collections.Generic.IEnumerable<TimeSampler> with
        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<TimeSampler> = this :> System.Collections.Generic.IEnumerator<TimeSampler>
        member this.GetEnumerator() : System.Collections.IEnumerator = this :> System.Collections.IEnumerator
        
    interface System.Collections.Generic.IEnumerator<TimeSampler> with
        member this.Current : TimeSampler = this
        member this.Current: obj = (this :> System.Collections.Generic.IEnumerator<TimeSampler>).Current :> obj
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
    
module TimeSamplers =
    let Environment = new TimeSampler("env")
    let Shapes = new TimeSampler("shapes")
    let Image = new TimeSampler("img")
