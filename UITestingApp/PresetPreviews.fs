namespace UITestingApp

open Android.App
open Android.Content
open Android.Views
open Android.Widget
open Android.Util

open Settings
open Settings.Presets


type TestPresetPreview(context : Context, name : string, settingsObject : AllKindsOfSettings) =
    inherit PresetPreview(context, PresetName = name)

    let selectedEvent = Event<AllKindsOfSettings>()
    member val PresetSelected = selectedEvent.Publish

    override this.GeneratePreview() =
        let bmp = Android.Graphics.Bitmap.CreateBitmap(fst this.ImageSize, snd this.ImageSize, Android.Graphics.Bitmap.Config.Argb8888)
        let c = 
            match settingsObject.Color with
            | Defined(r, g, b) -> Android.Graphics.Color.Rgb(int(r * 255.0f), int(g * 255.0f), int(b * 255.0f))
            | Randomized -> Android.Graphics.Color.Transparent
        bmp.EraseColor(c.ToArgb())
        System.Threading.Thread.Sleep(1000) // simulate lengthy thumbnail generation
        bmp

    override this.CacheDirName = "test"

    override this.Selected() = 
        Toast.MakeText(context, sprintf "%s selected" name, ToastLength.Short).Show()
        selectedEvent.Trigger(settingsObject)


type PresetPreviewAdapter(context : Context, previews : #PresetPreview[]) =
    inherit BaseAdapter()

    override this.Count = previews.Length
    override this.GetItem(pos) = previews.[pos] :> Java.Lang.Object
    override this.GetItemId(pos) = int64 pos

    override this.GetView(pos, convertView, parent) =
        previews.[pos] :> View


type PresetsSelectionView(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit GridView(context, attrs, defStyle)
    
    let mutable previews : #PresetPreview[] = [||]

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

    let mutable allPreviews : TestPresetPreview[] = [||]

    member this.OnPresetSelected(object) =
        //...
        ()

    override this.OnCreate(bundle) =
        base.OnCreate(bundle)
        allPreviews <-
            [| 
                "default", AllKindsOfSettings(); 
                "red", AllKindsOfSettings(Color=Defined(1.0f, 0.0f, 0.0f)); 
                "custom", AllKindsOfSettings(Color=Defined(0.5f, 0.5f, 1.0f)); 
                "white", AllKindsOfSettings(Color=Defined(1.0f, 1.0f, 1.0f)); 
            |]
            |> Array.map (fun (name, o) -> new TestPresetPreview(this, name, o, ImageSize=(512, 320), IsPresetSaved=(name<>"custom")))
        let view = new PresetsSelectionView(this)
        view.SetGravity(GravityFlags.CenterHorizontal)
        let containerView = new FrameLayout(this)
        containerView.SetPadding(12, 12, 12, 12)
        containerView.AddView(view)
        for each in allPreviews do each.Initialize()
        view.SetPreviews(allPreviews)
        for each in allPreviews do
            each.PresetSelected.Add(this.OnPresetSelected)
        this.SetContentView(containerView)

    override this.OnStop() =
        base.OnStop()
        for each in allPreviews do
            each.CancelGeneration()

