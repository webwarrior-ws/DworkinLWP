namespace DworkinUI


open Android.App
open Android.Widget
open Android.Views
open Android.Content

open Settings
open Settings.Presets



type internal PresetNameCollision =
    | NoCollision
    | UserPreset
    | BuiltinPreset
    with 
        static member Determine(candidateName : string, allPresets : #Preset[]) =
            let existingPreset = allPresets |> Array.tryFind (fun preset -> preset.Name = candidateName)
            match existingPreset with
            | None -> NoCollision
            | Some(preset) when preset.IsBuiltin -> BuiltinPreset
            | Some(_) -> UserPreset


type PresetNameCollisionWarning(context : Context, attrs : Android.Util.IAttributeSet, defStyleAttr : int) =
    inherit FrameLayout(context, attrs, defStyleAttr)

    let mutable view : View = null

    new(context, attrs) = new PresetNameCollisionWarning(context, attrs, 0)
    new(context) = new PresetNameCollisionWarning(context, null)
    
    member internal this.UpdateDisplay(collision : PresetNameCollision) =
        let warningBuiltin = view.FindViewById<View>(Resources.Id.warning_text_builtin)
        let warningUser = view.FindViewById<View>(Resources.Id.warning_text_user)
        warningBuiltin.Visibility <- 
            if collision = BuiltinPreset then ViewStates.Visible else ViewStates.Gone
        warningUser.Visibility <- 
            if collision = UserPreset then ViewStates.Visible else ViewStates.Gone

    override this.OnFinishInflate() =
        view <- FrameLayout.Inflate(context, Resources.Layout.PresetNameCollisionWarning, this)


type SavePresetDialog<'TPreset when 'TPreset :> Preset>(currentPreset : 'TPreset, previewSize : (int*int), allPresets : 'TPreset[]) =
    inherit AndroidX.Fragment.App.DialogFragment()

    let presetSavedEvent = Event<Preset>()
    member this.PresetSaved = presetSavedEvent.Publish

    member private this.ValidateName(dialog : AlertDialog, name : string) =
        let existingPreset = allPresets |> Array.tryFind (fun preset -> preset.Name = name)
        let collision = PresetNameCollision.Determine(name, allPresets)
        this.UpdateWarningsAndSaveButton(dialog, collision)

    member private this.UpdateWarningsAndSaveButton(dialog : AlertDialog, collision : PresetNameCollision) = 
        let saveButton = dialog.GetButton(int Android.Content.DialogButtonType.Positive)
        saveButton.Enabled <- collision <> BuiltinPreset
        let warningWidget = dialog.FindViewById<PresetNameCollisionWarning>(Resources.Id.preset_name_warning)
        warningWidget.UpdateDisplay(collision)

    override this.OnCreateDialog(bundle) =
        use builder = new AlertDialog.Builder(this.Context)
        let dialogView = this.Activity.LayoutInflater.Inflate(Resources.Layout.SavePresetDialog, null)
        let nameInput = dialogView.FindViewById<EditText>(Resources.Id.preset_name_input)
        
        let preview = new PresetPreview(this.Context, currentPreset, ImageSize=previewSize)
        preview.Initialize()
        preview.NameVisible <- false
        dialogView.FindViewById<ViewGroup>(Resources.Id.preview_display).AddView(preview)
        
        let dialog = 
            builder
                .SetView(dialogView)
                .SetPositiveButton("Save", 
                    fun _ args -> 
                        let newPreset = Serialization.copy currentPreset
                        newPreset.Name <- nameInput.Text
                        newPreset.IsSaved <- false
                        PresetManagement.savePreset this.Context newPreset
                        preview.SaveCurrentImageForAnotherPreset(newPreset)
                        presetSavedEvent.Trigger(newPreset) )
                .SetNegativeButton("Cancel", fun _ _ -> this.Dialog.Cancel())
                .Create()

        nameInput.Text <- currentPreset.Name
        nameInput.TextChanged.Add(fun args -> 
            let currText = args.Text.ToString()
            this.ValidateName(dialog, currText) )
        dialog.ShowEvent.Add(fun args -> this.ValidateName(dialog, currentPreset.Name))

        dialog :> Dialog
        

type RenamePresetDialog<'TPreset when 'TPreset :> Preset>(currentPreset : 'TPreset, allPresets : 'TPreset[]) =
    inherit AndroidX.Fragment.App.DialogFragment()
    
    let presetRenamedEvent = Event<string * string>()
    member this.PresetRenamed = presetRenamedEvent.Publish

    member private this.UpdateWarningsAndSaveButton(dialog : AlertDialog, collision : PresetNameCollision) = 
        let okButton = dialog.GetButton(int Android.Content.DialogButtonType.Positive)
        okButton.Enabled <- collision <> BuiltinPreset
        let warningWidget = dialog.FindViewById<PresetNameCollisionWarning>(Resources.Id.preset_name_warning)
        warningWidget.UpdateDisplay(collision)

    override this.OnCreateDialog(bundle) =
        use builder = new AlertDialog.Builder(this.Context)
        let dialogView = this.Activity.LayoutInflater.Inflate(Resources.Layout.RenamePresetDialog, null)
        let nameInput = dialogView.FindViewById<EditText>(Resources.Id.preset_name_input)

        let dialog = 
            builder
                .SetView(dialogView)
                .SetTitle("Rename preset")
                .SetPositiveButton("OK", fun _ args -> 
                    let oldName = currentPreset.Name
                    if PresetManagement.renamePreset this.Context currentPreset nameInput.Text then
                        presetRenamedEvent.Trigger(oldName, currentPreset.Name) )
                .SetNegativeButton("Cancel", fun _ _ -> this.Dialog.Cancel())
                .Create()
        
        nameInput.Text <- currentPreset.Name
        nameInput.TextChanged.Add(fun args -> 
            let currText = args.Text.ToString()
            let collision = PresetNameCollision.Determine(currText, allPresets)
            this.UpdateWarningsAndSaveButton(dialog, collision) )

        dialog :> Dialog
