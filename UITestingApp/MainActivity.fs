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


type Endpoint = 
    | Hollow = 0
    | Filled = 1


type AllKindsOfSettings() =
    inherit Settings()
    static do 
        Settings.RegisterClass<AllKindsOfSettings>()
        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.PCBEndpoints, 
            Views.RandomizedSpinnerWithImages<Endpoint>.GetFactoryForMapping(
                function 
                | Endpoint.Hollow -> { ImageID=Resources.Drawable.endpoint_hollow; Label="hollow" } 
                | Endpoint.Filled -> { ImageID=Resources.Drawable.endpoint_filled; Label="filled" }) )
    
    [<Enum([| Endpoint.Hollow; Endpoint.Filled |], ItemType=typeof<Endpoint>, Name="Custom Spinner", 
      Representation=EnumRepresentation.PCBEndpoints)>]
    member val CustomEnum = Defined(Endpoint.Hollow) with get, set

    [<Enum([| 1; 2; 3; 4; |], Name="Integer Spinner")>]
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
        this.FindViewById<ViewGroup>(Resources.Id.root).SetPadding(20, 0 , 20, 0)

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
