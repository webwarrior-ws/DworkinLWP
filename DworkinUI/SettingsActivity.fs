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

    let updateEditorsEvent = Event<unit>()
    let updateEditors = updateEditorsEvent.Publish

    let settingChangedEvent = Event<unit>()

    let changeHandler (change : SettingsChange) () =
        CurrentSettings.registerChange change
        settingChangedEvent.Trigger()

    member this.SettingChanged = settingChangedEvent.Publish

    member this.UpdateEditors() = updateEditorsEvent.Trigger()

    member private this.GetEffectEditor() =
        let editor = SettingsClassChooser<EffectSettings>.Create(this.Context, Effects.allTypes)
        editor.Title <- "Effect"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Effect |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Effect=unbox<EffectSettings>(value) } ))
        editor.Changed.Add(changeHandler SettingsChange.EffectChanged)
        updateEditors.Add(editor.UpdateValue)
        editor
    
    member private this.GetForegroundEditor() =
        let editor = SettingsClassChooser<MaterialSettings>.Create(this.Context, Materials.allTypes)
        editor.Title <- "Foreground"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Foreground |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Foreground=unbox<MaterialSettings>(value) } ))
        editor.Changed.Add(changeHandler SettingsChange.ForegroundChanged)
        updateEditors.Add(editor.UpdateValue)
        editor
    
    member private this.GetBackgroundEditor() =
        let editor = SettingsClassChooser<MaterialSettings>.Create(this.Context, Materials.allTypes)
        editor.Title <- "Background"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Background |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Background=unbox<MaterialSettings>(value) } ))
        editor.Changed.Add(changeHandler SettingsChange.BackgroundChanged)
        updateEditors.Add(editor.UpdateValue)
        editor

    member private this.GetForegroundColorEditor() =
        let editor = new Settings.Views.RandomizedColorPicker(this.Context)
        editor.Initialize()
        let e = editor :> Views.ISettingEditor
        e.Title <- "Foreground color"
        let accessor =
            Views.Accessor(
                (fun () -> CurrentSettings.get().ForegroundColor |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with ForegroundColor=unbox(value) } ))
        e.Accessor <- accessor
        e.Changed.Add(changeHandler SettingsChange.ForegroundChanged)
        updateEditors.Add(fun () -> e.Accessor <- accessor)
        editor

    member private this.GetBackgroundColorEditor() =
        let editor = new Settings.Views.RandomizedColorPicker(this.Context)
        editor.Initialize()
        let e = editor :> Views.ISettingEditor
        e.Title <- "Background color"
        let accessor =
            Views.Accessor(
                (fun () -> CurrentSettings.get().BackgroundColor |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with BackgroundColor=unbox(value) } ))
        e.Accessor <- accessor
        e.Changed.Add(changeHandler SettingsChange.BackgroundChanged)
        updateEditors.Add(fun () -> e.Accessor <- accessor)
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


[<Activity(Label = "Appearance Settings")>]
type AppearanceSettingsActivity() =
    inherit ActivityWithBackButton()
    
    member private this.UpdatePresetName() =
        let display = this.FindViewById<TextView>(Resources.Id.preset_name_display)
        if not (isNull display) then
            display.Text <- 
                match CurrentPresetSelection.appearance with
                | Some(preset) -> 
                    let currSettings = CurrentSettings.get()
                    if preset.Effect = currSettings.Effect && 
                       preset.BackgroundColor = currSettings.BackgroundColor &&
                       preset.ForegroundColor = currSettings.ForegroundColor &&
                       preset.Background = currSettings.Background &&
                       preset.Foreground = currSettings.Foreground then 
                        preset.Name
                    else
                        "custom"
                | None -> "custom"
    
    override this.OnRestart() =
        base.OnRestart()
        this.UpdatePresetName()
        let fragment = this.SupportFragmentManager.FindFragmentById(Resources.Id.content) :?> AppearanceSettingsFragment
        fragment.UpdateEditors()

    override this.OnCreate(bundle) =
        base.OnCreate(bundle)
        this.SetContentView(Resources.Layout.Empty)
        this.SetupToolbar()
        let fragment = new AppearanceSettingsFragment()
        fragment.SettingChanged.Add(fun() -> this.UpdatePresetName())
        let transaction = this.SupportFragmentManager.BeginTransaction()
        transaction
            .Replace(Resources.Id.content, fragment)
            .RunOnCommit(new Java.Lang.Runnable(fun () -> 
                this.UpdatePresetName()
                // open presets manager for appearance
                this.FindViewById<View>(Resources.Id.preset_control).Click.Add(fun args ->
                    this.StartActivity(typeof<AppearancePresetSelectionActivity>) ) ))
            .Commit() |> ignore

and [<Activity(Label = "Appearance Preset")>] AppearancePresetSelectionActivity() =
    inherit ActivityWithBackButton()

    override this.OnCreate(bundle) =
        base.OnCreate(bundle)

        let presetsDirPath = Presets.PresetManagement.getPresetsDirPath this AppearancePreset.DirectoryName
        use presetsDir = new Java.IO.File(presetsDirPath)
        let builtinPresets = BuiltinPresets.getAppearancePresets(this)
        let presets =
            if presetsDir.Exists() then
                [| for file in presetsDir.ListFiles() do
                    let preset : AppearancePreset = Presets.PresetManagement.loadPreset this file.AbsolutePath
                    yield preset |]
            else [||]
        let previews = [| 
            for preset in (Array.append builtinPresets presets) -> 
                new Settings.Presets.PresetPreview(this, preset) |]

        this.SetContentView(Resources.Layout.Empty)
        this.SetupToolbar()
        let fragment = new Settings.Presets.PresetSelectionFragment(Previews=previews)
        fragment.PresetSelected.Add(fun preset -> this.OnPresetSelected(preset :?> AppearancePreset) )
        this.SupportFragmentManager.BeginTransaction().Replace(Resources.Id.content, fragment).Commit() |> ignore

    member this.OnPresetSelected(preset : AppearancePreset) =
        let currSettings = CurrentSettings.get()
        let newSettings = 
            { currSettings with 
                Effect=Serialization.copy preset.Effect
                BackgroundColor=preset.BackgroundColor
                ForegroundColor=preset.ForegroundColor
                Background=Serialization.copy preset.Background
                Foreground=Serialization.copy preset.Foreground }
        CurrentSettings.set(newSettings)
        if currSettings.Effect <> newSettings.Effect then
            CurrentSettings.registerChange(SettingsChange.EffectChanged)
        if currSettings.Background <> newSettings.Background || currSettings.BackgroundColor <> newSettings.BackgroundColor then
            CurrentSettings.registerChange(SettingsChange.BackgroundChanged)
        if currSettings.Foreground <> newSettings.Foreground || currSettings.ForegroundColor <> newSettings.ForegroundColor then
            CurrentSettings.registerChange(SettingsChange.ForegroundChanged)
        CurrentPresetSelection.appearance <- Some(preset)
        this.Finish()


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
