namespace DworkinTestingApp

open Android.App
open Android.Widget

open Settings
open LibDworkin.DworkinSettings
open LibDworkin.Aether
open LibDworkin.Lenses


type Resources = DworkinTestingApp.Resource


[<Activity (Label = "Settings")>]
type SettingsActivity() =
    inherit Android.Support.V7.App.AppCompatActivity()
    
    let mutable seedDisplay : EditText = null

    member this.SetRandomSeed(newSeed : int) =
        let oldSettings = CurrentSettings.get()
        CurrentSettings.set({ oldSettings with Seed=newSeed })

    override this.OnCreate(bundle) =
        base.OnCreate (bundle)
        // Create your application here
        this.SetContentView(Resources.Layout.SettingsScreen)
        seedDisplay <- this.FindViewById<EditText>(Resources.Id.seed_display)
        let nextSeedButton = this.FindViewById<Button>(Resources.Id.next_seed_button)
        seedDisplay.Text <- CurrentSettings.get().Seed |> string
        seedDisplay.TextChanged.Add(fun args -> this.SetRandomSeed(int seedDisplay.Text))
        nextSeedButton.Click.Add(fun args -> 
            let nextSeed : int = System.Random().Next()
            seedDisplay.Text <- string nextSeed //this.SetRandomSeed() is called here beacuse text is changed 
        )
        let toolbar = this.FindViewById<Android.Support.V7.Widget.Toolbar>(Resources.Id.app_toolbar)
        this.SetSupportActionBar(toolbar)
        this.SupportActionBar.SetDisplayHomeAsUpEnabled(true)
    
    override this.OnOptionsItemSelected(item) =
        if item.ItemId = Android.Resource.Id.Home then
            this.Finish()
            true
        else
            base.OnOptionsItemSelected(item)

    override this.OnResume() =
        base.OnResume()
        let seedString = CurrentSettings.get().Seed |> string
        if seedDisplay.Text <> seedString then
            seedDisplay.Text <- seedString




