namespace UITestingApp

open Android.App
open Android.Content
open Android.Views
open Android.Widget
open Android.Util

open Settings
open Settings.Presets


type TestPreset(settingsObject : AllKindsOfSettings) =
    inherit Preset()
    
    override this.GeneratePreview(context, w, h) =
        let bmp = Android.Graphics.Bitmap.CreateBitmap(w, h, Android.Graphics.Bitmap.Config.Argb8888)
        let c = 
            match settingsObject.Color with
            | Defined(r, g, b) -> Android.Graphics.Color.Rgb(int(r * 255.0f), int(g * 255.0f), int(b * 255.0f))
            | Randomized -> Android.Graphics.Color.Transparent
        bmp.EraseColor(c.ToArgb())
        System.Threading.Thread.Sleep(1000) // simulate lengthy thumbnail generation
        bmp

    override this.DirName = "test"

    override this.HasRandomizedValues = settingsObject.HasRandomizedValues

    override this.UsedClasses = [||]
 

[<Activity>] 
type PresetSelectionActivity() =
    inherit AndroidX.Fragment.App.FragmentActivity()

    member this.OnPresetSelected(preset : Preset) =
        Toast.MakeText(this, sprintf "%s selected" preset.Name, ToastLength.Short).Show()

    override this.OnCreate(bundle) =
        base.OnCreate(bundle)

        let presets = 
            [| 
                TestPreset(AllKindsOfSettings(), Name="default", IsSaved=true); 
                TestPreset(AllKindsOfSettings(Color=Defined(1.0f, 0.0f, 0.0f)), Name="red", IsSaved=true); 
                TestPreset(AllKindsOfSettings(Color=Defined(0.5f, 0.5f, 1.0f)), Name="custom"); 
                TestPreset(AllKindsOfSettings(Color=Defined(1.0f, 1.0f, 1.0f)), Name="white", IsSaved=true); 
            |]
        let allPreviews =
            [| for preset in presets ->  new PresetPreview(this, preset, ImageSize=(512, 320)) |]
        let fragment = new PresetSelectionFragment(Previews=allPreviews)
        fragment.PresetSelected.Add(this.OnPresetSelected)

        this.SetContentView(Resources.Layout.PresetSelection)
        this.SupportFragmentManager.BeginTransaction().Replace(Resources.Id.previewSelectionRoot, fragment).Commit() |> ignore
