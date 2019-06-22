namespace DworkinTestingApp

open Android.App
open Android.Widget
open Android.Views
open Android.Content

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
    
    member private this.GetEffectEditor() =
        let editor = SettingsClassChooser<EffectSettings>.Create(this, Effects.allTypes )
        editor.Title <- "Effect"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Effect |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Effect=unbox<EffectSettings>(value) } ))
        editor.Changed.Add(fun _ -> CurrentSettings.registerChange(SettingsChange.EffectChanged))
        editor
    
    member private this.GetForegroundEditor() =
        let editor = SettingsClassChooser<MaterialSettings>.Create(this, Materials.allTypes )
        editor.Title <- "Foreground"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Foreground |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Foreground=unbox<MaterialSettings>(value) } ))
        editor.Changed.Add(fun _ -> CurrentSettings.registerChange(SettingsChange.ForegroundChanged))
        editor
    
    member private this.GetBackgroundEditor() =
        let editor = SettingsClassChooser<MaterialSettings>.Create(this, Materials.allTypes )
        editor.Title <- "Background"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Background |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Background=unbox<MaterialSettings>(value) } ))
        editor.Changed.Add(fun _ -> CurrentSettings.registerChange(SettingsChange.BackgroundChanged))
        editor

    override this.OnCreate(bundle) =
        base.OnCreate (bundle)
        if isNull bundle then
            // TODO: see if this is no longer needed
            XamarinCrutch.updateLibraryResourceIds typeof<Resources> typeof<Settings.Resources>
            for each in Settings.AdditionalResourceValues.resourceTypesToBeUpdated do
                XamarinCrutch.updateLibraryResourceIds typeof<Resources> each

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
        // settings editors
        let editorsGroup = this.FindViewById<ViewGroup>(Resources.Id.editors_group)
        editorsGroup.AddView(this.GetEffectEditor())
        editorsGroup.AddView(this.GetBackgroundEditor())
        editorsGroup.AddView(this.GetForegroundEditor())
        
    
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




