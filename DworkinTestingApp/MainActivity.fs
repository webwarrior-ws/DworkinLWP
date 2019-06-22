namespace DworkinTestingApp


open Android.App
open Android.Content.PM
open Android.Views
open Android.Opengl
open Android.OS

open LibDworkin.RendererComponents
open LibDworkin.DworkinSettings
open Settings


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
            Foreground=Materials.SolidColor(Color=Defined(OpenTK.Graphics.Color4.Black.AsRGBTriple));
            Background=Materials.SolidColor(Color=Defined(OpenTK.Graphics.Color4.White.AsRGBTriple));
            Seed=42;
        }
    static member val private RendererContainer : LibDworkin.Renderer.DworkinRenderer option = None with get, set
    static member val private PlatformContainer : AndroidPlatform option = None with get, set

    member this.Platform = 
        match MainActivity.PlatformContainer with
        | Some(p) -> p
        | None ->
            let p = AndroidPlatform(this)
            MainActivity.PlatformContainer <- Some(p)
            p
    
    member this.OpenSettings() =
        let intent = new Android.Content.Intent(this, typeof<SettingsActivity>)
        this.StartActivity(intent)

    override this.OnCreate(bundle) =
        base.OnCreate(bundle)

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
                let r = new LibDworkin.Renderer.DworkinRenderer(this.Platform, settingsHolder)
                MainActivity.RendererContainer <- Some(r)
                r
        //let renderer = //new LibDworkin.Renderer.DworkinRenderer(platform, GeneratedConfigs.MagneticSpiralsBunchBranching2_3())
            //new TestRenderers.BenchmarkRenderer(platform, GeneratedConfigs.MagneticSpiralsBunchBranching2_3())
            //new TestRenderers.BenchmarkRenderer(platform, GeneratedConfigs.PCB_RandomPosChips())
            //new LibDworkin.Renderer.DworkinRenderer(platform, GeneratedConfigs.SpiralsProbesBranching1())
            //new TestRenderers.CollidersTestRenderer(platform, TestConfigs.circleAdvancedTest)
            //new LibDworkin.Renderer.DworkinRenderer(platform, TestConfigs.circleAdvancedTest)
            //new TestRenderers.AdvancedShapesTestRenderer(platform)
            //new LibDworkin.Renderer.DworkinRenderer(platform, TestConfigs.iGeoExample40) 
            //new TestRenderers.CollidersTestRenderer(platform, TestConfigs.colliderStrategyTest)
            //new LibDworkin.Renderer.DworkinRenderer(platform, TestConfigs.brushesAndSpeedsTest) 
            //new TestRenderers.ShapesTestRenderer(platform) 
            //new TestRenderers.CollidingAgentsTestRenderer(platform)
        
        //renderer.Effect <- GlowEffect(platform, OpenTK.Graphics.Color4.BlueViolet, 0.5f, LibDworkin.RendererComponents.GlowType.Outer)
        //renderer.Effect <- LibDworkin.RendererComponents.ShadowEffect(platform, LibDworkin.Geometry.Point(1.0f, -1.0f), 10.0f, 0.2f, 0.5f, 0.5f, false)
        //renderer.Effect <- LibDworkin.RendererComponents.BevelEffect(platform, 5.0f, 0.5f, LibDworkin.Geometry.Point(1.0f, -1.0f), 
        //                                                             LibDworkin.RendererComponents.BevelProfile.Linear,
        //                                                             LibDworkin.RendererComponents.BevelMode.Ridge)
        
        //renderer.Background <- LibDworkin.RendererComponents.NormalmapAndAOMaterial(platform, OpenTK.Graphics.Color4.LightGray, "TexturesCom_Leather_Plain_512_normal.png",
        //                                                                            LibDworkin.Geometry.Point(0.0f, 1.0f), 1.0f, 1.0f, None)
        //renderer.Background <- LibDworkin.RendererComponents.ColoredTextureMaterial(platform, OpenTK.Graphics.Color4.DarkGray, "knitted-netting_@2X.png",
        //                                                                            0.775f, 1.25f, 80.0f)
        //renderer.Background <- LibDworkin.RendererComponents.MetalMaterial(platform, OpenTK.Graphics.Color4.LightGray, 0.13f, 1.0f, 
        //                                                                   LibDworkin.Geometry.Point(1.0f, 0.0f), 0.0f, 0.0f, 500.0f, 1.0f, 0.5f, 45.0f)
        //renderer.Background <- LibDworkin.RendererComponents.WoodMaterial(platform, OpenTK.Graphics.Color4.Sienna, 0.0f, 2.0f,  20.0f, 0.25f)
        if supportsEs2 then
            view.SetEGLContextClientVersion(2)
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

