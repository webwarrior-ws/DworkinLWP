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
    
    override this.GeneratePreview(h : int, w : int) =
        let bmp = Android.Graphics.Bitmap.CreateBitmap(h, w, Android.Graphics.Bitmap.Config.Argb8888)
        let c = 
            match settingsObject.Color with
            | Defined(r, g, b) -> Android.Graphics.Color.Rgb(int(r * 255.0f), int(g * 255.0f), int(b * 255.0f))
            | Randomized -> Android.Graphics.Color.Transparent
        bmp.EraseColor(c.ToArgb())
        System.Threading.Thread.Sleep(1000) // simulate lengthy thumbnail generation
        bmp

    override this.DirName = "test"


type PresetsSelectionView(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit GridView(context, attrs, defStyle)
    
    let mutable previews : PresetPreview[] = [||]

    new(context, attrs) = new PresetsSelectionView(context, attrs, 0)
    new(context) = new PresetsSelectionView(context, null)

    member this.SetPreviews(newPreviews) = 
        previews <- newPreviews
        if previews.Length > 0 then
            this.SetColumnWidth(fst previews.[0].ImageSize)
        this.SetNumColumns(0xffffffff) // auto-fit
        let spacing = 8 // in pixels
        this.SetHorizontalSpacing(spacing)
        this.SetVerticalSpacing(spacing)
        this.StretchMode <- StretchMode.NoStretch
        this.Adapter <- new PresetPreviewAdapter(context, previews)
 

[<Activity>] 
type PresetSelectionActivity() =
    inherit Activity()

    let mutable allPreviews : PresetPreview[] = [||]

    member this.OnPresetSelected(preset : Preset) =
        Toast.MakeText(this, sprintf "%s selected" preset.Name, ToastLength.Short).Show()

    override this.OnCreate(bundle) =
        base.OnCreate(bundle)
        allPreviews <-
            [| 
                TestPreset(AllKindsOfSettings(), Name="default", IsSaved=true); 
                TestPreset(AllKindsOfSettings(Color=Defined(1.0f, 0.0f, 0.0f)), Name="red", IsSaved=true); 
                TestPreset(AllKindsOfSettings(Color=Defined(0.5f, 0.5f, 1.0f)), Name="custom"); 
                TestPreset(AllKindsOfSettings(Color=Defined(1.0f, 1.0f, 1.0f)), Name="white", IsSaved=true); 
            |]
            |> Array.map (fun preset -> new PresetPreview(this, preset, ImageSize=(512, 320)))
        let view = new PresetsSelectionView(this)
        view.SetGravity(GravityFlags.CenterHorizontal)
        let containerView = new FrameLayout(this)
        containerView.SetPadding(12, 12, 12, 12)
        containerView.AddView(view)
        for each in allPreviews do each.Initialize()
        view.SetPreviews(allPreviews)
        for each in allPreviews do
            each.Click.Add(fun _ -> this.OnPresetSelected(each.Preset))
        this.SetContentView(containerView)

    override this.OnStop() =
        base.OnStop()
        for each in allPreviews do
            each.CancelGeneration()
