namespace DworkinUI


open Android.App
open Android.Widget
open Android.Views
open Android.Content
open AndroidX.RecyclerView.Widget

open Settings
open Settings.Presets
open LibDworkin.DworkinSettings



type IMenuComponent = interface
    abstract OnCreateOptionsMenu : AndroidX.AppCompat.App.AppCompatActivity * IMenu -> bool
    abstract OnOptionsItemSelected : AndroidX.AppCompat.App.AppCompatActivity * IMenuItem -> bool
    abstract OnActivityResult : AndroidX.AppCompat.App.AppCompatActivity * int * Result -> unit
end


[<AbstractClass>]
type ActivityWithBackButton() =
    inherit AndroidX.AppCompat.App.AppCompatActivity()

    static member val MenuComponent : IMenuComponent = 
        { new IMenuComponent with
            override this.OnCreateOptionsMenu(_, _) = false
            override this.OnOptionsItemSelected(_, _) = false
            override this.OnActivityResult(_, _, _) = () } with get, set
    
    member this.Toolbar = this.FindViewById<AndroidX.AppCompat.Widget.Toolbar>(Resources.Id.app_toolbar)

    member this.SetupToolbar() =
        this.SetSupportActionBar(this.Toolbar)
        this.SupportActionBar.SetDisplayHomeAsUpEnabled(true)

    override this.OnOptionsItemSelected(item) =
        if ActivityWithBackButton.MenuComponent.OnOptionsItemSelected(this, item) then
            true
        elif item.ItemId = Android.Resource.Id.Home then
            this.Finish()
            true
        else
            base.OnOptionsItemSelected(item)

    override this.OnActivityResult(requestCode, resultCode, data) =
        base.OnActivityResult(requestCode, resultCode, data)
        ActivityWithBackButton.MenuComponent.OnActivityResult(this, requestCode, resultCode)

    override this.OnCreateOptionsMenu(menu) =
        ActivityWithBackButton.MenuComponent.OnCreateOptionsMenu(this, menu)


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
                |> Array.sortBy (fun each -> each.Name)
            else [||]
        allPresets.AddRange(presets)
        allPresets.AddRange(builtinPresets)
        let previewSize = this.GetPreviewSize()
        for preset in allPresets do
            previews.Add <| new Settings.Presets.PresetPreview(this, preset, ImageSize=previewSize)

        this.SetContentView(Resources.Layout.PresetSelectionScreen)
        this.SetupToolbar()

        fragment <- new Settings.Presets.PresetSelectionFragment(previews)
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
                this.RegisterForContextMenu(newPreview)
                let i = allPresets.FindIndex(fun x -> x.Name = preset.Name)
                if i >= 0 then
                    allPresets.[i] <- preset :?> 'TPreset 
                    previews.[i].InvalidateCache()
                    previews.[i] <- newPreview
                else
                    let j = allPresets.FindLastIndex(fun each -> not each.IsBuiltin && preset.Name > each.Name) + 1
                    allPresets.Insert(j, preset :?> 'TPreset) 
                    previews.Insert(j, newPreview)
                fragment.Update() )
            dialog.Show(this.SupportFragmentManager, dialog.GetType().Name) )

        this.SupportFragmentManager.BeginTransaction().Replace(Resources.Id.content, fragment).Commit() |> ignore

    override this.OnResume() =
        base.OnResume()
        this.UpdatePresetNameDisplay()

    override this.OnCreateContextMenu(menu, v, menuInfo) =  
        match v with
        | :? PresetPreview as preview ->
            let preset = preview.Preset
            let position = allPresets.FindIndex(fun x -> LanguagePrimitives.PhysicalEquality preset (x :> Preset))
            menu.SetHeaderTitle(preset.Name) |> ignore
            if preset.InAppPurchases.IsEmpty then
                menu.Add(position, int PresetContextMenuChoice.Select, 0, "Select") |> ignore
            if not preset.IsBuiltin then
                menu.Add(position, int PresetContextMenuChoice.Rename, 1, "Rename") |> ignore
                menu.Add(position, int PresetContextMenuChoice.Delete, 2, "Delete") |> ignore
        | _ -> ()
        base.OnCreateContextMenu(menu, v, menuInfo)

    override this.OnContextItemSelected(item) =
        let pos = item.GroupId
        match LanguagePrimitives.EnumOfValue item.ItemId with
        | PresetContextMenuChoice.Select ->
            this.OnPresetSelected(allPresets.[pos])
            true
        | PresetContextMenuChoice.Rename ->
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

    override this.OnCreateOptionsMenu(menu) = 
        // Don't show menu with "preview/set" button in preset selection screens
        true

    override this.OnDestroy() =
        base.OnDestroy()
        for each in previews do each.Dispose()
        previews.Clear()
        System.GC.Collect() // needed because otherwise will eventually get OutOfMemoryException
    

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
        PreviewRenderers.AppearancePresetRenderer.ResetInstance() |> Async.Start
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
        PreviewRenderers.OverallPresetRenderer.ResetInstance() |> Async.Start
        base.OnStop()


[<Activity(Label = "Appearance Settings")>]
type AppearanceSettingsActivity() =
    inherit ActivityWithBackButton()
    
    override this.OnCreate(bundle) =
        base.OnCreate(bundle)
        this.SetContentView(Resources.Layout.Empty)
        this.SetupToolbar()
        let fragment = new AppearanceSettingsFragment()
        fragment.PresetButtonAction <- fun () -> this.StartActivity(typeof<AppearancePresetSelectionActivity>)
        this.SupportFragmentManager.BeginTransaction().Replace(Resources.Id.content, fragment).Commit() |> ignore


[<Activity(Name = "dworkinUI.SettingsActivity", Label = "Settings")>]
type SettingsActivity() =
    inherit ActivityWithBackButton()
    
    member private this.UpdatePresetName() =
        let display = this.FindViewById<TextView>(Resources.Id.preset_name_display)
        if not (isNull display) then
            display.Text <- CurrentPresetSelection.getOverallPresetDisplayText()

    member this.IsTabletLayout : bool =
        this.Resources.GetBoolean(Resources.Boolean.isTablet)

    member private this.SecondaryPaneTitleView =
        this.FindViewById<TextView>(Resources.Id.settings_secondary_title)

    member private this.UpdatePurchasesLinkVisibility() =
        let purchasesLink = this.FindViewById<View>(Resources.Id.purchases_link)
        if not (isNull purchasesLink) then
            let availablePurchases = Settings.InAppPurchases.getAvailablePurchases()
            if Array.isEmpty availablePurchases then
                purchasesLink.Visibility <- ViewStates.Gone
            else
                purchasesLink.Visibility <- ViewStates.Visible
#if DEBUG
    static member val CREATE_FILE = 5
    
    member this.SavePresetsArchive(fs: System.IO.Stream) =
        let presetDirNames = [| OverallPreset.DirectoryName; AppearancePreset.DirectoryName |] 
        use zos = new Java.Util.Zip.ZipOutputStream(fs)
        for dirName in presetDirNames do
            Settings.Presets.PresetManagement.ensurePresetsDir this dirName
            let dirPath = Settings.Presets.PresetManagement.getPresetsDirPath this dirName
            let dir = System.IO.DirectoryInfo(dirPath)
            for file in dir.EnumerateFiles() do
                let zipEntry = new Java.Util.Zip.ZipEntry(dirName + "/" + file.Name)
                zos.PutNextEntry(zipEntry)
                let fileContent = System.IO.File.ReadAllBytes(file.FullName)
                zos.Write(fileContent)
                zos.CloseEntry()
        zos.Close()
        fs.Close()
#endif
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
        
        if this.IsTabletLayout then // two-pane tablet layout
            let secondPane = this.FindViewById<LinearLayout>(Resources.Id.second_pane)
            if not (isNull bundle) then
                let title = bundle.GetString("secondary_pane_title")
                if not (isNull title) then
                    this.SecondaryPaneTitleView.Text <- title
                    secondPane.ShowDividers <- ShowDividers.Middle
            this.FindViewById<View>(Resources.Id.pattern_link).Click.Add(fun _ -> 
                this.SecondaryPaneTitleView.Text <- "Pattern settings"
                secondPane.ShowDividers <- ShowDividers.Middle
                let fragment = new PatternSettingsFragment()
                this.SupportFragmentManager.BeginTransaction().Replace(Resources.Id.settings_secondary, fragment).Commit() |> ignore
            )
            this.FindViewById<View>(Resources.Id.appearance_link).Click.Add(fun _ -> 
                this.SecondaryPaneTitleView.Text <- "Appearance settings"
                secondPane.ShowDividers <- ShowDividers.Middle
                let fragment = new AppearanceSettingsFragment()
                fragment.PresetButtonAction <- fun () -> this.StartActivity(typeof<AppearancePresetSelectionActivity>)
                this.SupportFragmentManager.BeginTransaction().Replace(Resources.Id.settings_secondary, fragment).Commit() |> ignore
            )
        else // normal phone one-pane layout
            this.FindViewById<View>(Resources.Id.pattern_link).Click.Add(fun _ -> 
                this.StartActivity(typeof<PatternSettingsActivity>) )

            this.FindViewById<View>(Resources.Id.appearance_link).Click.Add(fun _ -> 
                this.StartActivity(typeof<AppearanceSettingsActivity>) )
        
        this.FindViewById<View>(Resources.Id.purchases_link).Click.Add(fun _ ->
            let skus = Settings.InAppPurchases.getAvailablePurchases()
            async {
                let! data = InAppPurchases.getPurchasesDataAsync(skus)
                this.RunOnUiThread(fun () -> InAppPurchases.getPurchasesDialog(this, data).Show())
            }
            |> Async.Start
        )

        this.UpdatePresetName()
        // open presets manager
        this.FindViewById<View>(Resources.Id.preset_control).Click.Add(fun args ->
            this.StartActivity(typeof<OverallPresetSelectionActivity>))
#if DEBUG
        let controlsGroup = this.FindViewById<ViewGroup>(Resources.Id.controls_group)
        let savePresetsArchiveButton = new Button(this, Text="Save presets")
        savePresetsArchiveButton.Click.Add(fun _ -> 
            let intent = new Intent(Intent.ActionCreateDocument)
            intent
                .AddCategory(Intent.CategoryOpenable)
                .SetType("application/zip")
                .PutExtra(Intent.ExtraTitle, "presets.zip") 
                |> ignore
            this.StartActivityForResult(intent, SettingsActivity.CREATE_FILE)
        )
        controlsGroup.AddView(savePresetsArchiveButton)
#endif
    
    override this.OnSaveInstanceState(bundle) =
        base.OnSaveInstanceState(bundle)
        if this.IsTabletLayout then
            bundle.PutString("secondary_pane_title", this.SecondaryPaneTitleView.Text)

    override this.OnResume() =
        base.OnResume()
        Settings.InAppPurchases.readFromStorage(this.ApplicationContext)
        this.UpdatePurchasesLinkVisibility() // based on purchaes info from storage
        async {
            // fetch purchases info from billing API and update "Premium content" element visibility again
            do! Settings.InAppPurchases.refreshPurchasesInfoAsync(this.ApplicationContext)
            this.RunOnUiThread(this.UpdatePurchasesLinkVisibility)
        }
        |> Async.Start
        
#if DEBUG
    override this.OnActivityResult(requestCode, resultCode, data) =
        base.OnActivityResult(requestCode, resultCode, data)
        if requestCode = SettingsActivity.CREATE_FILE && resultCode = Result.Ok && not (isNull data) then
            let uri = data.Data
            use fs = this.ContentResolver.OpenOutputStream(uri)
            this.SavePresetsArchive(fs)
#endif
