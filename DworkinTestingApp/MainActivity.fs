namespace DworkinTestingApp


open Android.App
open Android.Content.PM
open Android.Views
open Android.Opengl
open Android.OS

open LibDworkin.RendererComponents
open LibDworkin.DworkinSettings
open Settings


type Resources = DworkinTestingApp.Resource


[<Activity(Label = "DworkinTestingApp", 
           ConfigurationChanges=(ConfigChanges.Orientation ||| ConfigChanges.KeyboardHidden), 
           Icon = "@mipmap/icon", 
           MainLauncher = true)>]
type MainActivity() =
    inherit Android.Support.V7.App.AppCompatActivity()
    
    let mutable view : GLSurfaceView = null
    static let settingsHolder = 
        ref { 
            Environment=ConfigSettings.PCB_RandomPosChips(); 
            Effect=Effects.Plain();
            ForegroundColor=Defined(OpenTK.Graphics.Color4.Black.AsRGBTriple);
            BackgroundColor=Defined(OpenTK.Graphics.Color4.White.AsRGBTriple);
            Foreground=Materials.SolidColor();
            Background=Materials.SolidColor();
            Seed=42;
        }
    static member val private RendererContainer : LibDworkin.Renderer.BaseRenderer option = None with get, set
    static member val private PlatformContainer : AndroidPlatform option = None with get, set

    member this.Platform = 
        match MainActivity.PlatformContainer with
        | Some(p) -> p
        | None ->
            let p = AndroidPlatform(this)
            MainActivity.PlatformContainer <- Some(p)
            p
    
    member this.OpenSettings() =
        let intent = new Android.Content.Intent(this, typeof<DworkinUI.SettingsActivity>)
        this.StartActivity(intent)

    override this.OnCreate(bundle) =
        base.OnCreate(bundle)

        if isNull bundle then
            // TODO: see if this is no longer needed
            XamarinCrutch.updateLibraryResourceIds typeof<Resources> typeof<DworkinUI.Resources>
            XamarinCrutch.updateLibraryResourceIds typeof<Resources> typeof<Settings.Resources>

        // Create our OpenGL view, and display it
        view <- new GLSurfaceView(this)

        // Check if the system supports OpenGL ES 2.0. 
        let activityManager = this.GetSystemService(Android.Content.Context.ActivityService) :?> ActivityManager
        let configurationInfo = activityManager.DeviceConfigurationInfo
        // Check for emulator. 
        let supportsEs2 = 
            configurationInfo.ReqGlEsVersion >= 0x20000 
            || (Build.VERSION.SdkInt >= BuildVersionCodes.IceCreamSandwichMr1 && (Build.Fingerprint.StartsWith("generic") 
            || Build.Fingerprint.StartsWith("unknown") 
            || Build.Model.Contains("google_sdk") 
            || Build.Model.Contains("Emulator") 
            || Build.Model.Contains("Android SDK built for x86")))
        
        if not <| CurrentSettings.isSet() then
            CurrentSettings.set(!settingsHolder)
        else
            settingsHolder := CurrentSettings.get()
        
        let renderer = 
            match MainActivity.RendererContainer with
            | Some(r) -> r
            | None ->
                let r = 
                    new LibDworkin.Renderer.DworkinRenderer(this.Platform, settingsHolder (*, StepsPerFrame=2*) )
                    //new TestRenderers.BenchmarkRenderer(this.Platform, settingsHolder) 
                    :> LibDworkin.Renderer.BaseRenderer
                MainActivity.RendererContainer <- Some(r)
                r
        
        if supportsEs2 then
            view.SetEGLContextClientVersion(2)
            view.SetEGLContextFactory(LibDworkin.Renderer.EGLContextFactory.Instance)
            view.PreserveEGLContextOnPause <- true
            // set renderer
            view.SetRenderer(renderer)
        else
            Android.Widget.Toast.MakeText(this, "This device does not support OpenGL ES 2.0.", Android.Widget.ToastLength.Long).Show()
        
        //this.SetContentView(view)
        this.SetContentView(Resources.Layout.Main)
        this.FindViewById<ViewGroup>(Resources.Id.app_root).AddView(view, new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MatchParent, ViewGroup.LayoutParams.MatchParent))
        let toolbar = this.FindViewById<Android.Support.V7.Widget.Toolbar>(Resources.Id.app_toolbar)
        toolbar.Title <- "Dworkin"
        this.SetSupportActionBar(toolbar)
    
    override this.OnPause() =
        // never forget to do this!
        base.OnPause()
        view.OnPause()

    override this.OnResume() =
        // never forget to do this!
        base.OnResume()
        settingsHolder := CurrentSettings.get()
        view.OnResume()
        // reset size and scale shader uniforms that might be upset by overall preset preview renderer
        match MainActivity.RendererContainer with
        | Some(renderer) -> 
            let w, h = renderer.WallpaperSize
            if LibDworkin.Renderer.EGLContextFactory.NeedsShaderUniformsReset then
                renderer.SetScale(renderer.GetDimensions(w, h), w, h)
                LibDworkin.Renderer.EGLContextFactory.NeedsShaderUniformsReset <- false
        | None -> ()

    override this.OnCreateOptionsMenu(menu) =
        this.MenuInflater.Inflate(Resources.Menu.menu, menu)
        true
    
    override this.OnOptionsItemSelected(item) =
        if item.ItemId = Resources.Id.menuitem_settings then
            this.OpenSettings()
            true
        elif item.ItemId = Resources.Id.menuitem_nextseed then
            CurrentSettings.set({ CurrentSettings.get() with Seed = System.Random().Next() })
            settingsHolder := CurrentSettings.get()
            true
        else
            base.OnOptionsItemSelected(item)

