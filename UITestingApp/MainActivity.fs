namespace UITestingApp

open System

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
open Android.Util

open Settings
open Settings.Views

type Resources = UITestingApp.Resource


type Endpoint = 
    | Hollow = 0
    | Filled = 1


type AllKindsOfSettings() =
    inherit Settings()
    static do 
        Settings.RegisterClass<AllKindsOfSettings>()
        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.PCBEndpoints, 
            Views.RandomizedSpinnerWithImages<Endpoint>.GetFactoryForMapping(
                function 
                | Endpoint.Hollow -> { ImageID=Resources.Drawable.endpoint_hollow; Label="hollow" } 
                | Endpoint.Filled -> { ImageID=Resources.Drawable.endpoint_filled; Label="filled" }) )
    
    [<Enum([| Endpoint.Hollow; Endpoint.Filled |], ItemType=typeof<Endpoint>, Name="Custom Spinner", 
      Representation=EnumRepresentation.PCBEndpoints)>]
    member val CustomEnum = Defined(Endpoint.Hollow) with get, set

    [<Enum([| 1; 2; 3; 4; |], Name="Integer Spinner")>]
    member val IntEnum = Defined(1) with get, set

    [<IntRange(2, 5)>]
    member val IntRange = Defined(3) with get, set

    [<FloatRange(1.0f, 2.0f, Step=0.1f)>]
    member val FloatRange = Defined(1.2f) with get, set

    [<Color>]
    member val Color = Defined(0.1f, 0.5f, 0.5f) with get, set

    [<Bool>]
    member val Bool = Defined(false) with get, set

    static member AllRandom = AllKindsOfSettings(CustomEnum=Randomized, IntEnum=Randomized, IntRange=Randomized, FloatRange=Randomized, Color=Randomized, Bool=Randomized)



type TestPresetPreview(context : Context, name : string, settingsObject : AllKindsOfSettings) =
    inherit PresetPreview(context, PresetName = name)

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

    override this.Selected() = ()


type PresetPreviewAdapter(context : Context, previews : #Views.PresetPreview[]) =
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

    override this.OnCreate(bundle) =
        base.OnCreate(bundle)
        let allPreviews = 
            [| 
                "default", AllKindsOfSettings(); 
                "red", AllKindsOfSettings(Color=Defined(1.0f, 0.0f, 0.0f)); 
                "third", AllKindsOfSettings(Color=Defined(0.5f, 0.5f, 1.0f)); 
                "white", AllKindsOfSettings(Color=Defined(1.0f, 1.0f, 1.0f)) 
            |]
            |> Array.map (fun (name, o) -> new TestPresetPreview(this, name, o, ImageSize=(512, 320)))
        let view = new PresetsSelectionView(this)
        view.SetGravity(GravityFlags.CenterHorizontal)
        let containerView = new FrameLayout(this)
        containerView.SetPadding(12, 12, 12, 12)
        containerView.AddView(view)
        for each in allPreviews do each.Initialize()
        view.SetPreviews(allPreviews)
        this.SetContentView(containerView)



type SettingsFragment() =
    inherit Android.Support.V4.App.Fragment()

    override this.OnCreateView(inflater, container, bundle) = 
        let view = inflater.Inflate(Resources.Layout.SettingsFragment, container)
        view


[<Activity (Label = "UITestingApp", MainLauncher = true, Icon = "@mipmap/icon")>]
type MainActivity () =
    inherit Android.Support.V4.App.FragmentActivity()

    let mutable settings = AllKindsOfSettings()

    member this.UpdateDisplay() =
        let display = this.FindViewById<TextView>(Resources.Id.textDisplay)
        display.Text <- settings.ToString()

    override this.OnCreate(bundle) =

        base.OnCreate(bundle)

        // Set our view from the "main" layout resource
        this.SetContentView(Resources.Layout.Main)

        // TODO: see if this is no longer needed
        XamarinCrutch.updateLibraryResourceIds typeof<Resources> typeof<Settings.Resources>
        for each in Settings.AdditionalResourceValues.resourceTypesToBeUpdated do
            XamarinCrutch.updateLibraryResourceIds typeof<Resources> each

        let settingsView = settings.CreateView(this)
        settings.UseView(settingsView)
        this.FindViewById<ViewGroup>(Resources.Id.editors).AddView(settingsView)
        this.FindViewById<ViewGroup>(Resources.Id.editors).SetPadding(20, 0 , 20, 0)

        settingsView.Changed.Add(fun _ -> 
            this.UpdateDisplay() )

        this.FindViewById<Button>(Resources.Id.reloadButton).Click.Add (fun args -> 
            settings <- AllKindsOfSettings()
            settings.UseView(settingsView)
            this.UpdateDisplay()
        )

        this.FindViewById<Button>(Resources.Id.setRandomizedButton).Click.Add (fun args -> 
            settings <- AllKindsOfSettings.AllRandom
            settings.UseView(settingsView)
            this.UpdateDisplay()
        )

        this.FindViewById<Button>(Resources.Id.presetsButton).Click.Add (fun args -> 
            //let intent = new Intent(this, typeof<PresetSelectionActivity>)
            this.StartActivity(typeof<PresetSelectionActivity>)
        )
