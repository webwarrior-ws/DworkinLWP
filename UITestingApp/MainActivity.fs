namespace UITestingApp

open System

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget

open Settings

type Resources = UITestingApp.Resource



type AllKindsOfSettings() =
    inherit Settings()
    static do Settings.RegisterClass<AllKindsOfSettings>()

    [<Enum([| 1; 2; 3; 4; |], Name="Integer Spinner (randomized)")>]
    member val IntEnum = Defined(1) with get, set

    [<IntRange(2, 5)>]
    member val IntRange = Defined(3) with get, set

    [<FloatRange(1.0f, 2.0f, Step=0.1f)>]
    member val FloatRange = Defined(1.2f) with get, set

    [<Color>]
    member val Color = Defined(0.1f, 0.5f, 0.5f) with get, set

    [<Bool>]
    member val Bool = Defined(false) with get, set

    static member AllRandom = AllKindsOfSettings(IntEnum=Randomized, IntRange=Randomized, FloatRange=Randomized, Color=Randomized, Bool=Randomized)


[<Activity (Label = "UITestingApp", MainLauncher = true, Icon = "@mipmap/icon")>]
type MainActivity () =
    inherit Activity ()

    let mutable settings = AllKindsOfSettings()

    member this.UpdateDisplay() =
        let display = this.FindViewById<TextView>(Resources.Id.textDisplay)
        display.Text <- settings.ToString()

    override this.OnCreate (bundle) =

        base.OnCreate (bundle)

        // Set our view from the "main" layout resource
        this.SetContentView (Resources.Layout.Main)

        // TODO: see if this is no longer needed
        XamarinCrutch.updateLibraryResourceIds typeof<Resources> typeof<Settings.Resources>
        for each in Settings.AdditionalResourceValues.resourceTypesToBeUpdated do
            XamarinCrutch.updateLibraryResourceIds typeof<Resources> each

        let settingsView = settings.CreateView(this)
        settings.UseView(settingsView)
        this.FindViewById<ViewGroup>(Resources.Id.root).AddView(settingsView)

        settingsView.Changed.Add(fun _ -> 
            this.UpdateDisplay() )

        this.FindViewById<Button>(Resources.Id.reloadButton).Click.Add (fun args -> 
            settings <- AllKindsOfSettings()
            settings.UseView(settingsView)
            this.UpdateDisplay()
        )

        this.FindViewById<Button>(Resources.Id.setRandomizedButton).Click.Add (fun args -> 
            settings <- AllKindsOfSettings.AllRandom
            settings.UseView(settingsView)
            this.UpdateDisplay()
        )
