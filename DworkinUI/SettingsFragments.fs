namespace DworkinUI


open Android.Widget
open Android.Views
open Android.Content
open AndroidX.AppCompat.App

open Settings
open LibDworkin.DworkinSettings




type OverallSettingsFragment() =
    inherit AndroidX.Fragment.App.Fragment()
    
    let mutable seedDisplay : EditText = null
    let mutable speedFactorSlider : SeekBar = null
    let mutable intervalDisplay : TextView = null
    let mutable transitionDisplay : TextView = null

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

    member private this.UpdateRandomizationControls() =
        intervalDisplay.Text <- RandomizationInterval.DisplayString(CurrentSettings.randomizationInterval)
        transitionDisplay.Text <- CurrentSettings.transitionStrategy.ToString()
    
    override this.OnCreateView(inflater, container, bundle) = 
        let view = inflater.Inflate(Resources.Layout.OverallSettingsFragment, container)
        
        seedDisplay <- view.FindViewById<EditText>(Resources.Id.seed_display)
        let nextSeedButton = view.FindViewById<Button>(Resources.Id.next_seed_button)
        seedDisplay.Text <- CurrentSettings.get().Seed |> string
        seedDisplay.TextChanged.Add(fun args -> 
            match System.Int32.TryParse seedDisplay.Text with
            | true, seed -> this.SetRandomSeed(seed)
            | false, _ -> 
                if seedDisplay.Text.Length < 8 then // empty field (also incorrect symbols ?)
                    seedDisplay.Text <- "0" 
                else // too big, truncate
                    seedDisplay.Text <- seedDisplay.Text.[0..seedDisplay.Text.Length-2]
                seedDisplay.SetSelection(seedDisplay.Text.Length) // move cursor to the end of text
        ) 
        nextSeedButton.Click.Add(fun args -> 
            let nextSeed : int = System.Random().Next()
            seedDisplay.Text <- string nextSeed //this.SetRandomSeed() is called here beacuse text is changed 
        )
        
        speedFactorSlider <- view.FindViewById<SeekBar>(Resources.Id.speed_display)
        this.UpdateSpeedFactorSlider()
        speedFactorSlider.ProgressChanged.Add(fun args -> this.SetSpeedFactor(args.Progress))
        
        intervalDisplay <- view.FindViewById<TextView>(Resources.Id.interval_text)
        transitionDisplay <- view.FindViewById<TextView>(Resources.Id.transition_text)
        this.UpdateRandomizationControls()

        view.FindViewById<View>(Resources.Id.interval_control).Click.Add(fun _ -> 
            use builder = new AlertDialog.Builder(this.Context)
            let items = 
                [| for each in RandomizationInterval.GetValues(typeof<RandomizationInterval>) -> unbox<RandomizationInterval> each |]
                |> Array.sortBy (fun each -> int each)
            let callback _ (args : DialogClickEventArgs) =
                CurrentSettings.randomizationInterval <- items.[args.Which]
                this.UpdateRandomizationControls()
            builder
                .SetTitle("Randomization interval")
                .SetItems(items |> Array.map RandomizationInterval.DisplayString, callback)
                .Show()
                |> ignore
        )
        view.FindViewById<View>(Resources.Id.transition_control).Click.Add(fun _ -> 
            use builder = new AlertDialog.Builder(this.Context)
            let items = 
                [| for each in TransitionStrategy.GetValues(typeof<TransitionStrategy>) -> unbox<TransitionStrategy> each |]
            let callback _ (args : DialogClickEventArgs) =
                CurrentSettings.transitionStrategy <- items.[args.Which]
                this.UpdateRandomizationControls()
            builder
                .SetTitle("Transition")
                .SetItems(items |> Array.map string, callback)
                .Show()
                |> ignore
        )

        view.FindViewById<View>(Resources.Id.license_control).Click.Add(fun _ ->
            use builder = new AlertDialog.Builder(this.Context)
            let root = new RelativeLayout(this.Context)
            let scrollView = inflater.Inflate(Resources.Layout.LicenseInfo, root)
            let textView = scrollView.FindViewById<TextView>(Resources.Id.text)
            use reader = new System.IO.StreamReader(this.Context.Assets.Open("LicenseInfo.txt"))
            textView.Text <- reader.ReadToEnd()
            builder
                .SetTitle("Licenses")
                .SetView(root)
                .SetNeutralButton("OK", fun _ _ -> ())
                .Show()
                |> ignore
        )

        view
        
    override this.OnResume() =
        base.OnResume()
        let seedString = CurrentSettings.get().Seed |> string
        if seedDisplay.Text <> seedString then
            seedDisplay.Text <- seedString
        this.UpdateSpeedFactorSlider()
        this.UpdateRandomizationControls()


type PatternSettingsFragment() =
    inherit AndroidX.Fragment.App.Fragment()
    
    let mutable settingsEditor : SettingsClassChooser<EnvironmentSettings> = null

    member private this.GetEnvironmentEditor() =
        let editor = SettingsClassChooser<EnvironmentSettings>.Create(this.Context, ConfigSettings.allEnvironmentSettingsTypes)
        editor.Title <- "Pattern"
        editor.Accessor <- 
            Views.Accessor(
                (fun () -> CurrentSettings.get().Environment |> box), 
                (fun value -> CurrentSettings.set { CurrentSettings.get() with Environment=unbox<EnvironmentSettings>(value) } ))
        editor

    member this.UpdateEditor() =
        if not (isNull settingsEditor) then
            settingsEditor.UpdateValue()

    override this.OnCreateView(inflater, container, bundle) = 
        let view = inflater.Inflate(Resources.Layout.PatternSettingsFragment, container, false)
        let editorsGroup = view.FindViewById<ViewGroup>(Resources.Id.editors_group)
        settingsEditor <- this.GetEnvironmentEditor()
        editorsGroup.AddView(settingsEditor)
        view


type AppearanceSettingsFragment() =
    inherit AndroidX.Fragment.App.Fragment()

    let updateEditorsEvent = Event<unit>()
    let updateEditors = updateEditorsEvent.Publish

    let settingChangedEvent = Event<unit>()

    let mutable display : TextView = null

    member val PresetButtonAction : unit -> unit = ignore with get, set

    member this.SettingChanged = settingChangedEvent.Publish

    member this.UpdateEditors() = updateEditorsEvent.Trigger()

    member this.UpdatePresetName() =
        if not (isNull display) then
            display.Text <- CurrentPresetSelection.getAppearancePresetDisplayText()

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
        let allTypes = Materials.allTypes |> Array.append [| typeof<Materials.SameAsBackground> |]
        let editor = SettingsClassChooser<MaterialSettings>.Create(this.Context, allTypes)
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
        display <- view.FindViewById<TextView>(Resources.Id.preset_name_display)
        view.FindViewById<View>(Resources.Id.preset_control).Click.Add(fun args -> this.PresetButtonAction())
        view

    override this.OnCreate(bundle) =
        base.OnCreate(bundle)
        this.SettingChanged.Add(this.UpdatePresetName)

    override this.OnResume() =
        base.OnResume()
        this.UpdatePresetName()
        this.UpdateEditors()

    override this.OnDestroy() =
        base.OnDestroy()
        display <- null

