module DworkinUI.PreviewRenderers

open OpenTK.Graphics.ES20
open OpenTK

open Android.Graphics
open Android.Runtime

open Javax.Microedition.Khronos.Egl

open LibDworkin.RendererComponents


module OffscreenRendering =
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
        let share_context = LibDworkin.Renderer.EGLContextFactory.CurrentContext
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
        if LibDworkin.Renderer.EGLContextFactory.CurrentContext = EGL10.EglNoContext then
            // No context is shared with our context, so reset shaders and textures (they will be lost when context is destroyed)
            ResettableLazy<Shader>.ResetAll()
            ResettableLazy<int>.ResetAll() // textures
        initResult.egl10 <- null

    let CreateGLAndSurface(width, height) : bool =
        initResult <- InitializeEGL()
        let mutable success = initResult.success
        if success then
           EglCreateContext(&initResult)
           success <- initResult.success
        if success then
           match CreateOffscreenBuffer(&initResult, width, height) with
           | Some(s) -> surface <- s
           | None ->
                // Failed; undo any work that was done.
                EndOurGL()
                success <- false
        if success then
            dimensions <- width, height
        success

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



type AppearancePresetRenderer(platform : AbstractPlatform, width : int, height : int) =
    inherit LibDworkin.Renderer.BaseRenderer(platform)

    let sdfTexture = platform.GetTexture("preview_sdf.png")

    static let mutable instance : AppearancePresetRenderer option = None

    static member RequestInstance(platform, width, height) : AppearancePresetRenderer =
        match instance with
        | Some(renderer) when renderer.WallpaperSize = (width, height) -> renderer
        | _ ->
            let renderer = new AppearancePresetRenderer(platform, width , height)
            renderer.Initialize()
            instance <- Some(renderer)
            renderer
    
    static member ResetInstance() = 
        if instance.IsSome then
            lock OffscreenRendering.lockObject <| fun () ->
                OffscreenRendering.EndOurGL()
                instance <- None

    static member HasInstance = instance.IsSome

    override this.SDFTexture = sdfTexture.Value

    member this.Initialize() =
        lock OffscreenRendering.lockObject <| fun () ->
            if not <| OffscreenRendering.Request(width, height) then
                failwith "failed to get offscreen GL context"

            GL.Viewport(0, 0, width, height)

            let dimensions = Vector2(float32 width, float32 height)
            this.SetSizeAndScaleVariables(dimensions, width, height)
            this.InitializeQuadVBO()
            this.InitializeMaterialTextures()

            GL.Disable(EnableCap.DepthTest)
            GL.Disable(EnableCap.CullFace)

    member this.RenderPreview(background : Material, foreground : Material, effect : Effect) : Bitmap = 
        lock OffscreenRendering.lockObject <| fun () ->
            this.Background <- background
            this.Foreground <- foreground
            this.Effect <- effect

            this.DrawMaterial(this.Background, this.BackgroundTexture)
            this.DrawMaterial(this.Foreground, this.ForegroundTexture)
            
            this.DrawImage()

            let bitmap = Bitmap.CreateBitmap(width, height, Bitmap.Config.Argb8888, false)
            let pixels = bitmap.LockPixels()
            GL.ReadPixels(0, 0, width, height, PixelFormat.Rgba, PixelType.UnsignedByte, pixels)
            bitmap.UnlockPixels()
            bitmap
