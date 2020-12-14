namespace DworkinUI


open Android.App
open Android.Widget
open Android.Views
open Android.Content

open Settings
open Settings.Presets



type private PresetNameCollision =
    | NoCollision
    | UserPreset
    | BuiltinPreset

type SavePresetDialog<'TPreset when 'TPreset :> Preset>(currentPreset : 'TPreset, previewSize : (int*int), allPresets : 'TPreset[]) =
    inherit Android.Support.V4.App.DialogFragment()

    let presetSavedEvent = Event<Preset>()
    member this.PresetSaved = presetSavedEvent.Publish

    member private this.ValidateName(dialog : AlertDialog, name : string) =
        let existingPreset = allPresets |> Array.tryFind (fun preset -> preset.Name = name)
        let collision =
            match existingPreset with
            | None -> NoCollision
            | Some(preset) when preset.IsBuiltin -> BuiltinPreset
            | Some(_) -> UserPreset
        this.UpdateWarningsAndSaveButton(dialog, collision)

    member private this.UpdateWarningsAndSaveButton(dialog : AlertDialog, collision : PresetNameCollision) = 
        let saveButton = dialog.GetButton(int Android.Content.DialogButtonType.Positive)
        saveButton.Enabled <- collision <> BuiltinPreset
        let warningWidget = dialog.FindViewById<View>(Resources.Id.preset_name_warning)
        warningWidget.Visibility <- if collision <> NoCollision then ViewStates.Visible else ViewStates.Invisible
        let setWarningText s =
            dialog.FindViewById<TextView>(Resources.Id.preset_name_warning_text).Text <- s  
        match collision with
        | BuiltinPreset -> setWarningText "Built-in preset with this name already exists. Choose another name."
        | UserPreset -> setWarningText "Warning: preset with this name already exists and will be overwritten."
        | NoCollision -> ()

    override this.OnCreateDialog(bundle) =
        use builder = new AlertDialog.Builder(this.Context)
        let dialogView = this.Activity.LayoutInflater.Inflate(Resources.Layout.SavePresetDialog, null)
        let nameInput = dialogView.FindViewById<EditText>(Resources.Id.preset_name_input)
        let dialog = 
            builder
                .SetView(dialogView)
                .SetPositiveButton("Save", 
                    fun _ args -> 
                        let newPreset = Serialization.copy currentPreset
                        newPreset.Name <- nameInput.Text
                        newPreset.IsSaved <- false
                        PresetManagement.savePreset this.Context newPreset
                        presetSavedEvent.Trigger(newPreset) )
                .SetNegativeButton("Cancel", fun _ _ -> this.Dialog.Cancel())
                .Create()
        
        let preview = new PresetPreview(this.Context, currentPreset, ImageSize=previewSize)
        preview.InvalidateCache()
        preview.Initialize()
        dialogView.FindViewById<ViewGroup>(Resources.Id.preview_display).AddView(preview)
        
        nameInput.Text <- currentPreset.Name
        nameInput.TextChanged.Add(fun args -> 
            let currText = args.Text.ToString()
            this.ValidateName(dialog, currText) )
        dialog.ShowEvent.Add(fun args -> this.ValidateName(dialog, currentPreset.Name))

        dialog :> Dialog
        

type RenamePresetDialog<'TPreset when 'TPreset :> Preset>(currentPreset : 'TPreset, builtinPresetNames : string[]) =
    inherit Android.Support.V4.App.DialogFragment()
    
    let presetRenamedEvent = Event<string * string>()
    member this.PresetRenamed = presetRenamedEvent.Publish

    override this.OnCreateDialog(bundle) =
        use builder = new AlertDialog.Builder(this.Context)
        let dialogView = this.Activity.LayoutInflater.Inflate(Resources.Layout.RenamePresetDialog, null)
        let nameInput = dialogView.FindViewById<EditText>(Resources.Id.preset_name_input)
        nameInput.Text <- currentPreset.Name
        nameInput.TextChanged.Add(fun args -> 
            let currText = args.Text.ToString()
            let warning = dialogView.FindViewById<View>(Resources.Id.preset_name_warning)
            warning.Visibility <- 
                if builtinPresetNames |> Array.contains currText then 
                    ViewStates.Visible 
                else 
                    ViewStates.Invisible )

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

        dialog :> Dialog
