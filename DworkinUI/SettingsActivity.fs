namespace DworkinUI

open Android.App
open Android.Widget
open Android.Views
open Android.Content

open Settings
open LibDworkin.DworkinSettings
open LibDworkin.Aether
open LibDworkin.Lenses


type Resources = DworkinUI.Resource


type OverallSettingsFragment() =
    inherit Android.Support.V4.App.Fragment()
    
    let mutable seedDisplay : EditText = null
    
    member this.SetRandomSeed(newSeed : int) =
        let oldSettings = CurrentSettings.get()
        CurrentSettings.set({ oldSettings with Seed=newSeed })

    override this.OnCreateView(inflater, container, bundle) = 
        let view = inflater.Inflate(Resources.Layout.OverallSettingsFragment, container)
        seedDisplay <- view.FindViewById<EditText>(Resources.Id.seed_display)
        let nextSeedButton = view.FindViewById<Button>(Resources.Id.next_seed_button)
        seedDisplay.Text <- CurrentSettings.get().Seed |> string
        seedDisplay.TextChanged.Add(fun args -> this.SetRandomSeed(int seedDisplay.Text))
        nextSeedButton.Click.Add(fun args -> 
            let nextSeed : int = System.Random().Next()
            seedDisplay.Text <- string nextSeed //this.SetRandomSeed() is called here beacuse text is changed 
        )
        view
        
    override this.OnResume() =
        base.OnResume()
        let seedString = CurrentSettings.get().Seed |> string
        if seedDisplay.Text <> seedString then
            seedDisplay.Text <- seedString


type PatternSettingsFragment() =
    inherit Android.Support.V4.App.Fragment()
    
    member private this.GetEnvironmentEditor() =
        let editor = SettingsClassChooser<EnvironmentSettings>.Create(this.Context, ConfigSettings.allEnvironmentSettingsTypes)
        editor.Title <- "Pattern"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Environment |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Environment=unbox<EnvironmentSettings>(value) } ))
        editor.Changed.Add(fun _ -> CurrentSettings.registerChange(SettingsChange.EnvironmentChanged))
        editor

    override this.OnCreateView(inflater, container, bundle) = 
        let view = inflater.Inflate(Resources.Layout.PatternSettingsFragment, container, false)
        let editorsGroup = view.FindViewById<ViewGroup>(Resources.Id.editors_group)
        editorsGroup.AddView(this.GetEnvironmentEditor())
        view


type AppearanceSettingsFragment() =
    inherit Android.Support.V4.App.Fragment()

    member private this.GetEffectEditor() =
        let editor = SettingsClassChooser<EffectSettings>.Create(this.Context, Effects.allTypes)
        editor.Title <- "Effect"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Effect |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Effect=unbox<EffectSettings>(value) } ))
        editor.Changed.Add(fun _ -> CurrentSettings.registerChange(SettingsChange.EffectChanged))
        editor
    
    member private this.GetForegroundEditor() =
        let editor = SettingsClassChooser<MaterialSettings>.Create(this.Context, Materials.allTypes)
        editor.Title <- "Foreground"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Foreground |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Foreground=unbox<MaterialSettings>(value) } ))
        editor.Changed.Add(fun _ -> CurrentSettings.registerChange(SettingsChange.ForegroundChanged))
        editor
    
    member private this.GetBackgroundEditor() =
        let editor = SettingsClassChooser<MaterialSettings>.Create(this.Context, Materials.allTypes)
        editor.Title <- "Background"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Background |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Background=unbox<MaterialSettings>(value) } ))
        editor.Changed.Add(fun _ -> CurrentSettings.registerChange(SettingsChange.BackgroundChanged))
        editor

    member private this.GetForegroundColorEditor() =
        let editor = new Settings.Views.RandomizedColorPicker(this.Context)
        editor.Initialize()
        let e = editor :> Views.ISettingEditor
        e.Title <- "Foreground color"
        e.Accessor <- Views.Accessor(
            (fun () -> CurrentSettings.get().ForegroundColor |> box), 
            (fun value -> CurrentSettings.set { CurrentSettings.get() with ForegroundColor=unbox(value) } ))
        e.Changed.Add(fun _ -> CurrentSettings.registerChange(SettingsChange.ForegroundChanged))
        editor

    member private this.GetBackgroundColorEditor() =
        let editor = new Settings.Views.RandomizedColorPicker(this.Context)
        editor.Initialize()
        let e = editor :> Views.ISettingEditor
        e.Title <- "Background color"
        e.Accessor <- Views.Accessor(
            (fun () -> CurrentSettings.get().BackgroundColor |> box), 
            (fun value -> CurrentSettings.set { CurrentSettings.get() with BackgroundColor=unbox(value) } ))
        e.Changed.Add(fun _ -> CurrentSettings.registerChange(SettingsChange.BackgroundChanged))
        editor

    override this.OnCreateView(inflater, container, bundle) = 
        let view = inflater.Inflate(Resources.Layout.AppearanceSettingsFragment, container, false)
        let editorsGroup = view.FindViewById<ViewGroup>(Resources.Id.editors_group)
        editorsGroup.AddView(this.GetBackgroundColorEditor())
        editorsGroup.AddView(this.GetForegroundColorEditor())
        editorsGroup.AddView(this.GetBackgroundEditor())
        editorsGroup.AddView(this.GetForegroundEditor())
        editorsGroup.AddView(this.GetEffectEditor())
        view


[<AbstractClass>]
type ActivityWithBackButton() =
    inherit Android.Support.V7.App.AppCompatActivity()
    
    member this.Toolbar = this.FindViewById<Android.Support.V7.Widget.Toolbar>(Resources.Id.app_toolbar)

    member this.SetupToolbar() =
        this.SetSupportActionBar(this.Toolbar)
        this.SupportActionBar.SetDisplayHomeAsUpEnabled(true)

    override this.OnOptionsItemSelected(item) =
        if item.ItemId = Android.Resource.Id.Home then
            this.Finish()
            true
        else
            base.OnOptionsItemSelected(item)


[<Activity(Label = "Pattern Settings")>]
type PatternSettingsActivity() =
    inherit ActivityWithBackButton()

    override this.OnCreate(bundle) =
        base.OnCreate (bundle)
        this.SetContentView(Resources.Layout.Empty)
        this.SetupToolbar()
        let fragment = new PatternSettingsFragment()
        this.SupportFragmentManager.BeginTransaction().Replace(Resources.Id.content, fragment).Commit() |> ignore


[<Activity(Label = "Pattern Settings")>]
type AppearanceSettingsActivity() =
    inherit ActivityWithBackButton()
        
    override this.OnCreate(bundle) =
        base.OnCreate (bundle)
        this.SetContentView(Resources.Layout.Empty)
        this.SetupToolbar()
        let fragment = new AppearanceSettingsFragment()
        this.SupportFragmentManager.BeginTransaction().Replace(Resources.Id.content, fragment).Commit() |> ignore


[<Activity(Label = "Settings")>]
type SettingsActivity() =
    inherit ActivityWithBackButton()

    override this.OnCreate(bundle) =
        base.OnCreate (bundle)
        
        if isNull bundle then
            // TODO: see if this is no longer needed
            XamarinCrutch.updateLibraryResourceIds typeof<Resources> typeof<Settings.Resources>
            for each in Settings.AdditionalResourceValues.resourceTypesToBeUpdated do
                XamarinCrutch.updateLibraryResourceIds typeof<Resources> each

        // Create your application here
        this.SetContentView(Resources.Layout.SettingsScreen)
        this.SetupToolbar()
        
        this.FindViewById<View>(Resources.Id.pattern_link).Click.Add(fun _ -> 
            this.StartActivity(typeof<PatternSettingsActivity>) )

        this.FindViewById<View>(Resources.Id.appearance_link).Click.Add(fun _ -> 
            this.StartActivity(typeof<AppearanceSettingsActivity>) )
    





