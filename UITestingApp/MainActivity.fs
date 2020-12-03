namespace UITestingApp

open System

open Android.App
open Android.Content
open Android.Views
open Android.Widget
open Android.Util

open Settings
open Settings.Views



type SettingsFragment() =
    inherit Android.Support.V4.App.Fragment()

    override this.OnCreateView(inflater, container, bundle) = 
        let view = inflater.Inflate(Resources.Layout.SettingsFragment, container)
        view


[<Activity (Label = "UITestingApp", MainLauncher = true, Icon = "@mipmap/icon")>]
type MainActivity () =
    inherit Android.Support.V4.App.FragmentActivity()

    let mutable settings = AllKindsOfSettings()

    member this.UpdateDisplay() =
        let display = this.FindViewById<TextView>(Resources.Id.textDisplay)
        display.Text <- settings.ToString()

    override this.OnCreate(bundle) =

        base.OnCreate(bundle)

        // Set our view from the "main" layout resource
        this.SetContentView(Resources.Layout.Main)

        

        let settingsView = settings.CreateView(this)
        settings.UseView(settingsView)
        this.FindViewById<ViewGroup>(Resources.Id.editors).AddView(settingsView)
        this.FindViewById<ViewGroup>(Resources.Id.editors).SetPadding(20, 0 , 20, 0)

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

        this.FindViewById<Button>(Resources.Id.presetsButton).Click.Add (fun args -> 
            this.StartActivity(typeof<PresetSelectionActivity>)
        )
