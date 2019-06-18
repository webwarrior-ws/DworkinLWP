namespace DworkinTestingApp

open Android.App
open Android.Widget

open Settings
open LibDworkin.DworkinSettings
open LibDworkin.Aether
open LibDworkin.Lenses


type Resources = DworkinTestingApp.Resource


[<Activity (Label = "SettingsActivity")>]
type SettingsActivity() =
    inherit Activity()
    
    member this.SetRandomSeed(newSeed : int) =
        let oldSettings = CurrentSettings.get()
        CurrentSettings.set({ oldSettings with Seed=newSeed })

    override this.OnCreate(bundle) =
        base.OnCreate (bundle)
        // Create your application here
        this.SetContentView(Resources.Layout.SettingsScreen)
        let seedDisplay = this.FindViewById<EditText>(Resources.Id.seed_display)
        let nextSeedButton = this.FindViewById<Button>(Resources.Id.next_seed_button)
        seedDisplay.Text <- CurrentSettings.get().Seed |> string
        seedDisplay.TextChanged.Add(fun args -> this.SetRandomSeed(int seedDisplay.Text))
        nextSeedButton.Click.Add(fun args -> 
            let nextSeed : int = System.Random().Next()
            seedDisplay.Text <- string nextSeed //this.SetRandomSeed() is called here beacuse text is changed 
        )




