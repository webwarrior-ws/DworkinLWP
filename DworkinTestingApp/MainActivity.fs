namespace DworkinTestingApp


open Android.App
open Android.Content.PM
open Android.Opengl
open Android.OS


[<Activity (Label = "DworkinTestingApp", ConfigurationChanges=(ConfigChanges.Orientation ||| ConfigChanges.KeyboardHidden), Icon = "@mipmap/icon", MainLauncher = true)>]
type MainActivity () =
    inherit Activity ()

    let mutable view : GLSurfaceView = null

    override this.OnCreate (bundle) =

        base.OnCreate (bundle)

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
        
        let platform = LibDworkin.RendererComponents.AndroidPlatform(this)
        let renderer = //new LibDworkin.Renderer.DworkinRenderer(platform, GeneratedConfigs.MagneticSpiralsBunchBranching2_3())
            //new TestRenderers.BenchmarkRenderer(platform, GeneratedConfigs.MagneticSpiralsBunchBranching2_3())
            //new TestRenderers.BenchmarkRenderer(platform, GeneratedConfigs.PCB_RandomPosChips())
            //new LibDworkin.Renderer.DworkinRenderer(platform, GeneratedConfigs.SpiralsProbesBranching1())
            new LibDworkin.Renderer.DworkinRenderer(platform, GeneratedConfigs.PCB_RandomPosChips())
            //new TestRenderers.CollidersTestRenderer(platform, TestConfigs.circleAdvancedTest)
            //new LibDworkin.Renderer.DworkinRenderer(platform, TestConfigs.circleAdvancedTest)
            //new TestRenderers.AdvancedShapesTestRenderer(platform)
            //new LibDworkin.Renderer.DworkinRenderer(platform, TestConfigs.iGeoExample40) 
            //new TestRenderers.CollidersTestRenderer(platform, TestConfigs.colliderStrategyTest)
            //new LibDworkin.Renderer.DworkinRenderer(platform, TestConfigs.brushesAndSpeedsTest) 
            //new TestRenderers.ShapesTestRenderer(platform) 
            //new TestRenderers.CollidingAgentsTestRenderer(platform)
        renderer.Effect <- LibDworkin.RendererComponents.PlainEffect(platform)
        renderer.Effect <- LibDworkin.RendererComponents.GlowEffect(platform, OpenTK.Graphics.Color4.BlueViolet, 0.5f, LibDworkin.RendererComponents.GlowType.Outer)
        //renderer.Effect <- LibDworkin.RendererComponents.ShadowEffect(platform, LibDworkin.Geometry.Point(1.0f, -1.0f), 10.0f, 0.2f, 0.5f, 0.5f, false)
        //renderer.Effect <- LibDworkin.RendererComponents.BevelEffect(platform, 5.0f, 0.5f, LibDworkin.Geometry.Point(1.0f, -1.0f), 
        //                                                             LibDworkin.RendererComponents.BevelProfile.Linear,
        //                                                             LibDworkin.RendererComponents.BevelMode.Ridge)
        renderer.Background <- LibDworkin.RendererComponents.NormalmapAndAOMaterial(platform, OpenTK.Graphics.Color4.LightGray, "TexturesCom_Leather_Plain_512_normal.png",
                                                                                    LibDworkin.Geometry.Point(0.0f, 1.0f), 1.0f, 1.0f, None)
        if supportsEs2 then
            view.SetEGLContextClientVersion(2)
            view.PreserveEGLContextOnPause <- true
            // set renderer
            view.SetRenderer(renderer)
        else
            Android.Widget.Toast.MakeText(this, "This device does not support OpenGL ES 2.0.", Android.Widget.ToastLength.Long).Show()

        
        this.SetContentView(view)

    override this.OnPause() =
        // never forget to do this!
        base.OnPause()
        view.OnPause()

    override this.OnResume() =
        // never forget to do this!
        base.OnResume()
        view.OnResume()

