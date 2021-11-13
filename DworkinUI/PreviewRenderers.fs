module DworkinUI.PreviewRenderers

open OpenTK.Graphics.ES20
open OpenTK

open Android.Graphics
open Android.Runtime

open Javax.Microedition.Khronos.Egl

open LibDworkin.RendererComponents



let inline writeLog(message: string) =
#if DEBUG
    Android.Util.Log.Debug("preview-render", message) |> ignore
#else
    ()
#endif


module OffscreenRendering =
    open System.Threading
    open System.Threading.Tasks
    
    // from https://stackoverflow.com/questions/41493145/how-compose-multiple-thumbnails-of-gles-scenes-into-a-scrolling-list-on-android/41525113#41525113
    
    let lockObject = obj()

    type EGLInitializeResult = 
        struct
            val mutable egl10 : IEGL10
            val mutable display : EGLDisplay 
            val mutable display_initialized : bool
            val mutable choose_config : bool 
            val mutable config : EGLConfig 
            val mutable success : bool
        end
    
    let CheckEglError(egl10 : IEGL10, tag : string) =
        let error = egl10.EglGetError()
        if error <> EGL10.EglSuccess then
            Android.Util.Log.Error(tag, sprintf "EGL-Error=%A" error ) |> ignore
            false
        else 
            true;

    let InitializeEGLConfig(initResult : byref<EGLInitializeResult>) : unit =
        // EglChooseConfig -> OpenGL ES 2.0 Config
        let EGL_OPENGL_ES2_BIT = 4
        let attribs_config = [|
                    EGL10.EglRenderableType; EGL_OPENGL_ES2_BIT // IMPORTANT
                    EGL10.EglRedSize; 8
                    EGL10.EglGreenSize; 8
                    EGL10.EglBlueSize; 8
                    EGL10.EglAlphaSize; 8
                    EGL10.EglDepthSize; 0
                    EGL10.EglStencilSize; 0
                    EGL10.EglNone
                |]
        let _configs : EGLConfig[] = Array.zeroCreate 1
        let _numconfigs : int[] = Array.zeroCreate 1
        initResult.choose_config <- initResult.egl10.EglChooseConfig(initResult.display, attribs_config, _configs, 1, _numconfigs)
        if (not <| CheckEglError(initResult.egl10, "EglChooseConfig" ) || not initResult.choose_config) then
            initResult.success <- false
        else
            initResult.config <- _configs.[0]
            initResult.success <- not (isNull initResult.config)

    let InitializeEGL() : EGLInitializeResult =
        let mutable result = EGLInitializeResult(display_initialized=false, choose_config=false, config=null, success=false)

        //FAIL Javax.Microedition.Khronos.Egl.IEGL10 t_egl10 = (Javax.Microedition.Khronos.Egl.IEGL10)Javax.Microedition.Khronos.Egl.EGLContext.EGL;
        result.egl10 <- EGLContext.EGL.JavaCast<IEGL10>()

        // _display
        result.display <- result.egl10.EglGetDisplay(EGL10.EglDefaultDisplay) // EglGetCurrentDisplay returns NULL !
        if isNull result.display then
            result
        else
            // EglInitialize
            let major_minor : int[] = Array.zeroCreate 2
            result.display_initialized <- result.egl10.EglInitialize(result.display, major_minor)
            if (not <| CheckEglError( result.egl10, "EglInitialize" ) || not result.display_initialized) then
                result
            else
                InitializeEGLConfig(&result)
                result
        
    // vars
    let mutable private initResult : EGLInitializeResult = EGLInitializeResult()
    let mutable private surface : EGLSurface = null
    let mutable private context : EGLContext = null
    let mutable private is_current = false
    let mutable private alreadyBeginningOurGL = false
    let mutable private dimensions : int * int = 0 , 0 // width, height

    
    let EglCreateContext(initResult : byref<EGLInitializeResult>) : unit =
        // EglCreateContext -> OpenGL ES 2.0 Context
        let EGL_CONTEXT_CLIENT_VERSION = 0x3098;
        let _version = EGL10.EglVersion;
        let _attribs_config = [|
                    EGL_CONTEXT_CLIENT_VERSION; 2 // IMPORTANT
                    EGL10.EglNone
                |]
        let share_context = LibDworkin.RendererComponents.EGLContextFactory.CurrentContext
        context <- initResult.egl10.EglCreateContext(initResult.display, initResult.config, share_context, _attribs_config)
        initResult.success <- CheckEglError(initResult.egl10, "EglCreateContext") && (context <> null)

    let CreateOffscreenBuffer(initResult : byref<EGLInitializeResult>, width : int, height : int) : EGLSurface option =
        let _attribs_config = [|
                    EGL10.EglWidth; width
                    EGL10.EglHeight; height
                    EGL10.EglNone
                |]
        let surface = initResult.egl10.EglCreatePbufferSurface(initResult.display, initResult.config, _attribs_config)
        if CheckEglError(initResult.egl10, "EglCreatePbufferSurface" ) && (surface <> null) then
            Some(surface)
        else
            None
    
    let EglMakeCurrent(initResult : EGLInitializeResult, surface, context) : bool =
        let make_current = initResult.egl10.EglMakeCurrent(initResult.display, surface, surface, context)
        CheckEglError(initResult.egl10, "EglMakeCurrent") && make_current

    let EndAndDispose() =
        // EglMakeCurrent
        if (is_current) then
            initResult.egl10.EglMakeCurrent(initResult.display, EGL10.EglNoSurface, EGL10.EglNoSurface, EGL10.EglNoContext) |> ignore
            is_current <- false
        
        // EglDestroyContext
        if (context <> null) then
            initResult.egl10.EglDestroyContext(initResult.display, context ) |> ignore
            context <- null

        // EglDestroySurface
        if (surface <> null) then
            initResult.egl10.EglDestroySurface(initResult.display, surface) |> ignore
            surface <- null
        
        //
        if (initResult.config <> null) then
            initResult.config.Dispose()
            initResult.config <- null
        
        // EglTerminate
        if (initResult.display_initialized) then
            initResult.egl10.EglTerminate(initResult.display) |> ignore
            initResult.display_initialized <- false
        
        //
        if (initResult.display <> null) then
            initResult.display.Dispose()
            initResult.display <- null
    
    let EndOurGL() =
        EndAndDispose()
        if LibDworkin.RendererComponents.EGLContextFactory.CurrentContext = EGL10.EglNoContext then
            // No context is shared with our context, so reset shaders and textures (they will be lost when context is destroyed)
            ResettableLazy<Shader>.ResetAll()
            ResettableLazy<int>.ResetAll() // textures
        initResult.egl10 <- null

    let CreateGLAndSurface(width, height) : bool =
        initResult <- InitializeEGL()
        if initResult.success then
           EglCreateContext(&initResult)
        if initResult.success then
           match CreateOffscreenBuffer(&initResult, width, height) with
           | Some(s) -> surface <- s
           | None ->
                // Failed; undo any work that was done.
                EndOurGL()
                initResult.success <- false
        if initResult.success then
            dimensions <- width, height
        initResult.success

    let EnsureGLAndSurfaceInitialized(width, height) : bool =
        surface <> null || CreateGLAndSurface(width, height)

    let BeginOurGL(width, height) : bool =
        if alreadyBeginningOurGL then
            // CAUTION: Caller must not call EndOurGLThread - might be another view starting it!
            false
        else
            alreadyBeginningOurGL <- true
            try 
                EnsureGLAndSurfaceInitialized(width, height)
            finally 
                alreadyBeginningOurGL <- false

    let MakeCurrent() : bool =
        is_current <- EglMakeCurrent(initResult, surface, context)
        is_current

    // from https://stackoverflow.com/questions/30719366/run-work-on-specific-thread
    type private SingleThreadTaskScheduler(cancellationToken : CancellationToken) =
        inherit TaskScheduler()
        let cancellationToken = cancellationToken;
        let taskQueue = new System.Collections.Concurrent.BlockingCollection<Task>();

        [<System.ThreadStatic>]
        [<DefaultValue>]
        static val mutable private isExecuting : bool
    
        member this.Start() =
            (new Thread(this.RunOnCurrentThread, Name="STTS Thread")).Start()
    
        // Just a helper for the sample code
        member this.Schedule(action : unit -> 'TResult) =
            Task.Factory.StartNew
                (
                    action, 
                    CancellationToken.None, 
                    TaskCreationOptions.None, 
                    this
                )
    
        // You can have this public if you want - just make sure to hide it
        member private this.RunOnCurrentThread() =
            SingleThreadTaskScheduler.isExecuting <- true
            try
                try
                    for task in taskQueue.GetConsumingEnumerable(cancellationToken) do
                        this.TryExecuteTask(task) |> ignore
                finally
                    SingleThreadTaskScheduler.isExecuting <- false
            with
            | :? System.OperationCanceledException -> ()
        
        // Signaling this allows the task scheduler to finish after all tasks complete
        member this.Complete() = taskQueue.CompleteAdding()

        override this.GetScheduledTasks() = null
    
        override this.QueueTask(task : Task) =
            try
                taskQueue.Add(task, cancellationToken)
            with
            | :? System.OperationCanceledException -> ()
    
        override this.TryExecuteTaskInline(task, taskWasPreviouslyQueued) =
            // We'd need to remove the task from queue if it was already queued. 
            // That would be too hard.
            if (taskWasPreviouslyQueued) then 
                false
            else
                SingleThreadTaskScheduler.isExecuting && this.TryExecuteTask(task);
    
    let mutable private cts = new CancellationTokenSource()
    let mutable private taskScheduler = lazy(
        let ts = new SingleThreadTaskScheduler(cts.Token)
        ts.Start()
        ts)
    
    /// Initialize offscreen rendering context and stuff if necessary. 
    /// Re-initialize if dimensions do not match.
    /// Return true on success, false on failure.
    let Request(width : int, height : int) : bool =
        if is_current then
            if (width, height) = dimensions then
                true
            else
                EndOurGL()
                BeginOurGL(width, height) && MakeCurrent()
        else
            BeginOurGL(width, height) && MakeCurrent()

    // Stop all pending tasks and release OpenGL resources
    let Release() =
        cts.Cancel()
        if taskScheduler.IsValueCreated then taskScheduler.Value.Complete()
        EndOurGL()
        cts <- new CancellationTokenSource()
        taskScheduler <- lazy(
            let ts = new SingleThreadTaskScheduler(cts.Token)
            ts.Start()
            ts)

    /// Preform given function (request) on a dedicated thread and return its result synchronously.
    /// Requests are queued up and run sequentially.
    let processRequest(request : unit -> 'T) : Async<'T> =
        taskScheduler.Value.Schedule(request) |> Async.AwaitTask



[<AbstractClass>]
type BasePresetRenderer(platform, width : int, height : int) = 
    inherit LibDworkin.Renderer.BaseRenderer(platform)

    abstract Initialize : unit -> unit
    default this.Initialize() =
        if not <| OffscreenRendering.Request(width, height) then
            failwith "failed to get offscreen GL context"

        writeLog("Initialize renderer instance")

    /// Return bitmap containing image from current renderbuffer
    member this.CaptureRenderbuffer() : Bitmap =
        let bitmap = Bitmap.CreateBitmap(width, height, Bitmap.Config.Argb8888, false)
        let pixels = bitmap.LockPixels()
        GL.ReadPixels(0, 0, width, height, PixelFormat.Rgba, PixelType.UnsignedByte, pixels)
        bitmap.UnlockPixels()
#if DEBUG
        match GL.GetErrorCode() with
        | x when x <> ErrorCode.NoError -> writeLog(sprintf "OpenGL error: %A" x)
        | _ -> ()
#endif
        bitmap


type RendererSingleton<'T when 'T :> BasePresetRenderer>() =
    let mutable instance : 'T option = None
    
    member this.RequestInstance(rendererFactory : unit -> 'T, width, height) : Async<'T> =
        OffscreenRendering.processRequest <| fun () ->
            match instance with
            | Some(renderer) when renderer.WallpaperSize = (width, height) -> renderer
            | _ ->
                writeLog("Request renderer instance")
                let renderer = rendererFactory()
                renderer.Initialize()
                instance <- Some(renderer)
                renderer
    
    member this.ResetInstance() = 
        OffscreenRendering.processRequest <| fun () ->
            if instance.IsSome then
                OffscreenRendering.Release()
                instance <- None


type AppearancePresetRenderer(platform : AbstractPlatform, width : int, height : int) =
    inherit BasePresetRenderer(platform, width, height)

    let sdfTexture = platform.GetTexture("preview_sdf.png")

    let mutable texturesInitialized = false

    static let singleton = RendererSingleton<AppearancePresetRenderer>()

    static member RequestInstance(platform, width, height) : Async<AppearancePresetRenderer> =
        let factory () = new AppearancePresetRenderer(platform, width , height)
        singleton.RequestInstance(factory, width, height)
    
    static member ResetInstance() = 
        singleton.ResetInstance()

    override this.SDFTexture = sdfTexture.Value

    override this.Initialize() =
        base.Initialize()

        GL.Viewport(0, 0, width, height)

        let dimensions = Vector2(float32 width, float32 height)
        this.SetSizeAndScaleVariables(dimensions, width, height)
        this.InitializeQuadVBO()
        
        GL.Disable(EnableCap.DepthTest)
        GL.Disable(EnableCap.CullFace)

    member this.RenderPreview(bgColor : Graphics.Color4, fgColor : Graphics.Color4, background : Material, foreground : Material, effect : Effect) : Async<Bitmap> = 
        let hash = bgColor.GetHashCode() ^^^ fgColor.GetHashCode() ^^^ background.GetHashCode() ^^^ foreground.GetHashCode() ^^^ effect.GetHashCode()
        writeLog(sprintf "Request render %d" hash)
        
        let getBitmap() =
            writeLog(sprintf "Begin   render %d  on thread#%d" hash System.Threading.Thread.CurrentThread.ManagedThreadId)
            // account for smaller size
            background.ExtraScaleFactor <- 0.25f
            foreground.ExtraScaleFactor <- 0.25f

            this.Background <- background
            this.Foreground <- foreground
            this.Effect <- effect
            this.Effect.SetMaterialProperties(
                { Color=fgColor; Specular=this.Foreground.Specular; Roughness=this.Foreground.Roughness },
                { Color=bgColor; Specular=this.Background.Specular; Roughness=this.Background.Roughness })

            if not texturesInitialized then
                this.InitializeMaterialTextures() // call it here to avoid unnecessary DrawMaterial calls
                texturesInitialized <- true

            this.DrawImage()
            writeLog(sprintf "End     render %d" hash)
            this.CaptureRenderbuffer()

        OffscreenRendering.processRequest getBitmap


type OverallPresetRenderer(platform : AbstractPlatform, width : int, height : int) =
    inherit BasePresetRenderer(platform, width, height)

    let mutable onSurfaceChangedCalled = false

    static let singleton = RendererSingleton<OverallPresetRenderer>()

    static member RequestInstance(platform, width, height) : Async<OverallPresetRenderer> =
        let factory () = new OverallPresetRenderer(platform, width , height)
        singleton.RequestInstance(factory, width, height)
    
    static member ResetInstance() = 
        singleton.ResetInstance()

    override this.Initialize() =
        base.Initialize()
        this.OnSurfaceCreated()
        LibDworkin.RendererComponents.EGLContextFactory.NeedsShaderUniformsReset <- true

    member this.RenderPreview(settings : LibDworkin.DworkinSettings.SettingsHolder) : Async<Bitmap> = 
        let hash = settings.GetHashCode()
        writeLog(sprintf "Request render [overall] %d" hash)

        let stopwatch = System.Diagnostics.Stopwatch.StartNew()
        writeLog(sprintf "Build environment [overall] %d (%f ms)" hash stopwatch.Elapsed.TotalMilliseconds)
        let envConfig = settings.Environment.Build(settings.Seed) :> LibDworkin.Agents.IDworkinEnvironmentConfig 
        let env = envConfig.GetEnvironment(this.GetDimensions(width, height))
        writeLog(sprintf "Begin simulation [overall] %d (%f ms)" hash stopwatch.Elapsed.TotalMilliseconds)
        let shapes = ResizeArray()
        while env.HasAgents do
            shapes.AddRange(env.Step())
        writeLog(sprintf "End   simulation [overall] %d (%f ms)" hash stopwatch.Elapsed.TotalMilliseconds)
        let maxShapesInOnePass = 10000

        let getBitmap() =
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()
            writeLog(sprintf "Begin   render [overall] %d on thread#%d" hash  System.Threading.Thread.CurrentThread.ManagedThreadId)
            let random = System.Random(settings.Seed)
            let fgColor, bgColor = settings.GetColors(System.Random(settings.Seed)) // use independent PRNG for colors
            this.Effect <- settings.Effect.Build(platform, random)
            this.Background <- settings.Background.Build(platform, random, bgColor)
            this.Foreground <- 
                match settings.Foreground with
                | :? LibDworkin.DworkinSettings.Materials.SameAsBackground -> settings.Background.Build(platform, random, fgColor)
                | foreground -> foreground.Build(platform, random, fgColor)
            
            this.Effect.SetMaterialProperties(
                { Color=fgColor; Specular=this.Foreground.Specular; Roughness=this.Foreground.Roughness },
                { Color=bgColor; Specular=this.Background.Specular; Roughness=this.Background.Roughness })

            if not onSurfaceChangedCalled then
                this.OnSurfaceChanged(width, height) // call it here to avoid unnecessary DrawMaterial calls
                onSurfaceChangedCalled <- true

            this.ClearSDF()
            this.ResetStencil()
            writeLog(sprintf "Begin shapes [overall] %d (%f ms)" hash stopwatch.Elapsed.TotalMilliseconds)
            if shapes.Count > maxShapesInOnePass then
                let splitShapes = shapes |> Seq.splitInto (shapes.Count / maxShapesInOnePass + 1)
                for chunk in splitShapes do
                    this.RenderShapes(chunk)
            else
                this.RenderShapes(shapes.ToArray())
            writeLog(sprintf "End   shapes [overall] %d (%f ms)" hash stopwatch.Elapsed.TotalMilliseconds)
            this.DrawImage()
            writeLog(sprintf "End     render [overall] %d (%f ms)" hash stopwatch.Elapsed.TotalMilliseconds)
            this.CaptureRenderbuffer()

        OffscreenRendering.processRequest getBitmap
