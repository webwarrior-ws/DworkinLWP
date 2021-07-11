namespace DworkinUI


open Android.Widget
open Android.Views
open Android.Content

open Settings
open LibDworkin.DworkinSettings




type OverallSettingsFragment() =
    inherit Android.Support.V4.App.Fragment()
    
    let mutable seedDisplay : EditText = null
    let mutable speedFactorSlider : SeekBar = null

    static let minSpeedFactor = 0.25f
    static let maxSpeedFactor = 1.0f
    
    member private this.SetRandomSeed(newSeed : int) =
        let oldSettings = CurrentSettings.get()
        CurrentSettings.set({ oldSettings with Seed=newSeed })

    member private this.SetSpeedFactor(progress : int) =
        CurrentSettings.speedFactor <- LibDworkin.Geometry.lerp minSpeedFactor maxSpeedFactor (float32(progress) / 100.0f)

    member private this.UpdateSpeedFactorSlider() =
        speedFactorSlider.Progress <- 
            (CurrentSettings.speedFactor - minSpeedFactor) / (maxSpeedFactor - minSpeedFactor) * 100.0f |> round |> int

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
        speedFactorSlider <- view.FindViewById<SeekBar>(Resources.Id.speed_display)
        this.UpdateSpeedFactorSlider()
        speedFactorSlider.ProgressChanged.Add(fun args -> this.SetSpeedFactor(args.Progress))
        view
        
    override this.OnResume() =
        base.OnResume()
        let seedString = CurrentSettings.get().Seed |> string
        if seedDisplay.Text <> seedString then
            seedDisplay.Text <- seedString
        this.UpdateSpeedFactorSlider()


type PatternSettingsFragment() =
    inherit Android.Support.V4.App.Fragment()
    
    member private this.GetEnvironmentEditor() =
        let editor = SettingsClassChooser<EnvironmentSettings>.Create(this.Context, ConfigSettings.allEnvironmentSettingsTypes)
        editor.Title <- "Pattern"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Environment |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Environment=unbox<EnvironmentSettings>(value) } ))
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

    member this.SettingChanged = settingChangedEvent.Publish

    member this.UpdateEditors() = updateEditorsEvent.Trigger()

    member private this.GetEffectEditor() =
        let editor = SettingsClassChooser<EffectSettings>.Create(this.Context, Effects.allTypes)
        editor.Title <- "Effect"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Effect |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Effect=unbox<EffectSettings>(value) } ))
        editor.Changed.Add(settingChangedEvent.Trigger)
        updateEditors.Add(editor.UpdateValue)
        editor
    
    member private this.GetForegroundEditor() =
        let editor = SettingsClassChooser<MaterialSettings>.Create(this.Context, Materials.allTypes)
        editor.Title <- "Foreground"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Foreground |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Foreground=unbox<MaterialSettings>(value) } ))
        editor.Changed.Add(settingChangedEvent.Trigger)
        updateEditors.Add(editor.UpdateValue)
        editor
    
    member private this.GetBackgroundEditor() =
        let editor = SettingsClassChooser<MaterialSettings>.Create(this.Context, Materials.allTypes)
        editor.Title <- "Background"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Background |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Background=unbox<MaterialSettings>(value) } ))
        editor.Changed.Add(settingChangedEvent.Trigger)
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
        e.Changed.Add(settingChangedEvent.Trigger)
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
        e.Changed.Add(settingChangedEvent.Trigger)
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
