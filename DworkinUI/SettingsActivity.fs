namespace DworkinUI


open Android.App
open Android.Widget
open Android.Views
open Android.Content

open Settings
open Settings.Presets
open LibDworkin.DworkinSettings



[<AbstractClass>]
type ActivityWithBackButton() =
    inherit AndroidX.AppCompat.App.AppCompatActivity()
    
    member this.Toolbar = this.FindViewById<AndroidX.AppCompat.Widget.Toolbar>(Resources.Id.app_toolbar)

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


type private PresetContextMenuChoice =
    | Select = 0
    | Rename = 1
    | Delete = 2

[<AbstractClass>]
type PresetSelectionActivity<'TPreset when 'TPreset :> Preset>() =
    inherit ActivityWithBackButton()

    let allPresets = ResizeArray<'TPreset>()
    let previews = ResizeArray<PresetPreview>()

    let mutable fragment : Settings.Presets.PresetSelectionFragment = null

    abstract PresetsDirName : string
    abstract BuiltinPresets : 'TPreset[]
    abstract CurrentPresetSelection : 'TPreset option with get, set
    abstract GetCurrentPreset : unit -> 'TPreset
    abstract GetPreviewSize : unit -> (int*int)
    abstract UpdatePresetNameDisplay : unit -> unit
    abstract OnPresetSelected : 'TPreset -> unit

    override this.OnCreate(bundle) =
        // Passing null to base.OnCreate because when save preset dialog is shown, error occurs when recreating it for 2 reasons:
        // - SavePresetDialog is generic type
        // - SavePresetDialog doesn't have default constructor
        // It's a workaround that causes save dialog to vanish when screen orientation changes.
        base.OnCreate(null)

        let presetsDirPath = Presets.PresetManagement.getPresetsDirPath this this.PresetsDirName
        use presetsDir = new Java.IO.File(presetsDirPath)
        let builtinPresets = this.BuiltinPresets
        let presets =
            if presetsDir.Exists() then
                [| for file in presetsDir.ListFiles() do
                    let preset : 'TPreset = Presets.PresetManagement.loadPreset this file.AbsolutePath
                    yield preset |]
            else [||]
        allPresets.AddRange(builtinPresets)
        allPresets.AddRange(presets)
        let previewSize = this.GetPreviewSize()
        for preset in allPresets do
            previews.Add <| new Settings.Presets.PresetPreview(this, preset, ImageSize=previewSize)

        this.SetContentView(Resources.Layout.PresetSelectionScreen)
        this.SetupToolbar()

        fragment <- new Settings.Presets.PresetSelectionFragment(Previews=previews)
        fragment.PresetSelected.Add(fun preset -> this.OnPresetSelected(preset :?> 'TPreset) )
    
        this.FindViewById<Button>(Resources.Id.save_preset_button).Click.Add(fun args ->
            let currPreset = this.GetCurrentPreset() |> Serialization.copy
            match this.CurrentPresetSelection with
            | Some(preset) -> currPreset.Name <- preset.Name
            | None -> currPreset.Name <- "Custom"
            let dialog = new SavePresetDialog<'TPreset>(currPreset, previewSize, allPresets.ToArray())
            dialog.PresetSaved.Add(fun preset -> 
                this.CurrentPresetSelection <- Some(preset :?> 'TPreset)
                this.UpdatePresetNameDisplay()
                // update preset selection grid
                let newPreview = new Settings.Presets.PresetPreview(this, preset, ImageSize=previewSize)
                newPreview.Initialize()
                let i = allPresets.FindIndex(fun x -> x.Name = preset.Name)
                if i >= 0 then
                    allPresets.[i] <- preset :?> 'TPreset 
                    previews.[i] <- newPreview
                else
                    allPresets.Add(preset :?> 'TPreset) 
                    previews.Add(newPreview)
                fragment.Update() )
            dialog.Show(this.SupportFragmentManager, dialog.GetType().Name) )

        this.SupportFragmentManager.BeginTransaction().Replace(Resources.Id.content, fragment).Commit() |> ignore

    override this.OnResume() =
        base.OnResume()
        this.UpdatePresetNameDisplay()

    override this.OnCreateContextMenu(menu, v, menuInfo) =  
        match v with
        | :? GridView as gridView ->
            let menuInfo = menuInfo :?> AdapterView.AdapterContextMenuInfo
            let preview = gridView.GetChildAt(menuInfo.Position) :?> PresetPreview
            menu.SetHeaderTitle(preview.Preset.Name) |> ignore
            if preview.Preset.InAppPurchases.IsEmpty then
                menu.Add(menuInfo.Position, int PresetContextMenuChoice.Select, 0, "Select") |> ignore
            if not preview.Preset.IsBuiltin then
                menu.Add(menuInfo.Position, int PresetContextMenuChoice.Rename, 1, "Rename") |> ignore
                menu.Add(menuInfo.Position, int PresetContextMenuChoice.Delete, 2, "Delete") |> ignore
        | _ -> ()
        base.OnCreateContextMenu(menu, v, menuInfo)

    override this.OnContextItemSelected(item) =
        let pos = item.GroupId
        match LanguagePrimitives.EnumOfValue item.ItemId with
        | PresetContextMenuChoice.Select ->
            this.OnPresetSelected(allPresets.[pos])
            true
        | PresetContextMenuChoice.Rename ->
            let builtinPresetNames = [| for preset in allPresets do if preset.IsBuiltin then yield preset.Name |]
            let dialog = new RenamePresetDialog<'TPreset>(allPresets.[pos], allPresets.ToArray())
            dialog.PresetRenamed.Add(fun (oldName, newName) ->
                match this.CurrentPresetSelection with
                | Some(preset) when preset.Name = oldName -> 
                    preset.Name <- newName
                    this.UpdatePresetNameDisplay()
                | _ -> ()
                previews.[pos].InvalidateCache() // delete cached preview image under old name
                assert (previews.[pos].Preset.Name = newName)
                previews.[pos].UpdateName()
                fragment.Update() )
            dialog.Show(this.SupportFragmentManager, dialog.GetType().Name)
            true
        | PresetContextMenuChoice.Delete ->
            PresetManagement.deletePreset this allPresets.[pos]
            previews.[pos].InvalidateCache()
            allPresets.RemoveAt(pos)
            previews.RemoveAt(pos)
            fragment.Update()
            true
        | _ -> 
            base.OnContextItemSelected(item)
    

[<Activity(Label = "Appearance Preset")>] 
type AppearancePresetSelectionActivity() =
    inherit PresetSelectionActivity<AppearancePreset>()

    override this.PresetsDirName = AppearancePreset.DirectoryName

    override this.BuiltinPresets = BuiltinPresets.getAppearancePresets(this)

    override this.GetCurrentPreset() =
        let cs = CurrentSettings.get()
        AppearancePreset(cs.Effect, cs.ForegroundColor, cs.BackgroundColor, cs.Foreground, cs.Background, IsSaved=false)

    override this.CurrentPresetSelection 
        with get() = CurrentPresetSelection.appearance
        and set(value) = CurrentPresetSelection.appearance <- value

    override this.GetPreviewSize() = AppearancePreset.GetPreviewSize(this)
    
    override this.UpdatePresetNameDisplay() =
        let display = this.FindViewById<TextView>(Resources.Id.preset_name_display)
        display.Text <- CurrentPresetSelection.getAppearancePresetDisplayText()

    override this.OnPresetSelected(preset) =  
        let currSettings = CurrentSettings.get()
        let newSettings = 
            { currSettings with 
                Effect=Serialization.copy preset.Effect
                BackgroundColor=preset.BackgroundColor
                ForegroundColor=preset.ForegroundColor
                Background=Serialization.copy preset.Background
                Foreground=Serialization.copy preset.Foreground }
        CurrentSettings.set(newSettings)
        this.CurrentPresetSelection <- Some(preset)
        this.Finish()

    override this.OnStop() =
        PreviewRenderers.AppearancePresetRenderer.ResetInstance()
        base.OnStop()


[<Activity(Label = "Preset")>] 
type OverallPresetSelectionActivity() =
    inherit PresetSelectionActivity<OverallPreset>()

    override this.PresetsDirName = OverallPreset.DirectoryName

    override this.BuiltinPresets = BuiltinPresets.getOverallPresets(this)

    override this.GetCurrentPreset() = OverallPreset(CurrentSettings.get())
       
    override this.CurrentPresetSelection
        with get() = CurrentPresetSelection.overall
        and set(value) = CurrentPresetSelection.overall <- value

    override this.GetPreviewSize() = OverallPreset.GetPreviewSize(this)

    override this.UpdatePresetNameDisplay() =
        let display = this.FindViewById<TextView>(Resources.Id.preset_name_display)
        display.Text <- CurrentPresetSelection.getOverallPresetDisplayText()

    override this.OnPresetSelected(preset) =
        let oldSettings = CurrentSettings.get()
        let newSettings = Serialization.copy preset.Settings
        CurrentSettings.set(newSettings)
        this.CurrentPresetSelection <- Some(preset)
        this.Finish()
    
    override this.OnStop() =
        PreviewRenderers.OverallPresetRenderer.ResetInstance()
        base.OnStop()


[<Activity(Label = "Appearance Settings")>]
type AppearanceSettingsActivity() =
    inherit ActivityWithBackButton()
    
    member private this.UpdatePresetName() =
        let display = this.FindViewById<TextView>(Resources.Id.preset_name_display)
        if not (isNull display) then
            display.Text <- CurrentPresetSelection.getAppearancePresetDisplayText()

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


[<Activity(Name = "dworkinUI.SettingsActivity", Label = "Settings")>]
type SettingsActivity() =
    inherit ActivityWithBackButton()
    
    member private this.UpdatePresetName() =
        let display = this.FindViewById<TextView>(Resources.Id.preset_name_display)
        if not (isNull display) then
            display.Text <- CurrentPresetSelection.getOverallPresetDisplayText()

    override this.OnRestart() =
        base.OnRestart()
        this.UpdatePresetName()

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

        this.FindViewById<View>(Resources.Id.purchases_link).Click.Add(fun _ ->
            let skus = Settings.InAppPurchases.getAvailablePurchases()
            let dialog = InAppPurchases.getPurchasesDialog(this, skus)
            dialog.Show() )

        this.UpdatePresetName()
        // open presets manager
        this.FindViewById<View>(Resources.Id.preset_control).Click.Add(fun args ->
            this.StartActivity(typeof<OverallPresetSelectionActivity>))

    override this.OnResume() =
        base.OnResume()
        Settings.InAppPurchases.refreshPurchasesInfo(this.ApplicationContext)
        let availablePurchases = Settings.InAppPurchases.getAvailablePurchases()
        let purchasesLink = this.FindViewById<View>(Resources.Id.purchases_link)
        if Array.isEmpty availablePurchases then
            purchasesLink.Visibility <- ViewStates.Gone
        else
            purchasesLink.Visibility <- ViewStates.Visible

