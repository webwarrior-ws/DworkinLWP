module Settings.Presets


open System.Collections.Generic

open Android.Content
open Android.Util
open Android.Widget
open Android.Views



[<AbstractClass>]
type Preset() =
    member val Name = "" with get, set
    /// Indicates whether preset is saved to disk
    member val IsSaved = false with get, set 
    /// True if preset is pre-installed with application
    member val IsBuiltin = false with get, set

    // to be overriden in subclasses:
    abstract GeneratePreview : Context * int * int -> Android.Graphics.Bitmap
    /// Name used to define sub-directories for storage of presets and preview cache. 
    /// Has to be unique for each type of preset.
    abstract DirName : string
    
    abstract HasRandomizedValues : bool

    abstract UsedClasses : System.Type[]

    member this.InAppPurchases : Set<string> =
        this.UsedClasses
        |> Array.choose InAppPurchases.getSKUForClass
        |> Array.filter (fun sku -> not <| InAppPurchases.isPurchased sku)
        |> Set.ofArray
        


/// Functions and classes used to manage presets (save/load/etc.)
module PresetManagement =
    let getPresetsDirPath (context : Context) (dirName : string) =
        context.FilesDir.Path + "/presets/" + dirName

    let ensurePresetsDir (context : Context) (dirName : string) =
        System.IO.Directory.CreateDirectory(getPresetsDirPath context dirName) |> ignore

    let getFilePath (context : Context) (dirName : string) (name : string) =
        let filename = Java.Net.URLEncoder.Encode(name, "UTF-8") + ".preset"
        (getPresetsDirPath context dirName) + "/" + filename

    let savePreset<'TPreset when 'TPreset :> Preset>(context : Context) (preset : 'TPreset) =
        if preset.IsBuiltin then
            failwith "Built-in presets cannot be saved"
        else
            ensurePresetsDir context preset.DirName
            let presetFilename = getFilePath context preset.DirName preset.Name
            use ostream = new System.IO.FileStream(presetFilename, System.IO.FileMode.Create)
            let json = Serialization.toJsonValue(preset)
            json.Save(ostream)
            preset.IsSaved <- true

    let renamePreset (context : Context) (preset : Preset) (newName : string) =
        if preset.IsBuiltin then
            failwith "Built-in presets cannot be renamed"
        else
            use file = new Java.IO.File(getFilePath context preset.DirName preset.Name)
            if file.Exists() then
                use newFile = new Java.IO.File(getFilePath context preset.DirName newName)
                if file.RenameTo(newFile) then
                    preset.Name <- newName
                    true
                else
                    false
            else 
                false
    
    let deletePreset (context : Context) (preset : Preset) =
        if preset.IsBuiltin then
            failwith "Built-in presets cannot be deleted"
        else
            use file = new Java.IO.File(getFilePath context preset.DirName preset.Name)
            if file.Exists() then
                file.Delete() |> ignore
            preset.IsSaved <- false

    let loadPreset<'TPreset when 'TPreset :> Preset> (context : Context) (filePath : string) : 'TPreset =
        if not (filePath.EndsWith ".preset") then
            failwith "preset files must end with .preset"
        else
            use file = new Java.IO.File(filePath)
            let encodedName = file.Name.Remove(file.Name.Length - ".preset".Length)
            let name = Java.Net.URLDecoder.Decode(encodedName, "UTF-8")
            use stream = new System.IO.FileStream(file.AbsolutePath, System.IO.FileMode.Open)
            let json = System.Json.JsonValue.Load(stream)
            let preset : 'TPreset = Serialization.fromJsonValue(json)
            preset.Name <- name
            preset.IsSaved <- true
            preset


type PresetPreviewState =
    | Pending
    | Generating
    | Complete
    | Error

/// Preset preview widget 
type PresetPreview(context : Context, preset : Preset) =
    inherit FrameLayout(context, null, 0)
    
    let mutable currentBitmap : Android.Graphics.Bitmap = null
    let mutable state = PresetPreviewState.Pending
    let cancellationSource = new System.Threading.CancellationTokenSource()

    member this.Preset = preset
    /// Size of preview in pixels
    member val ImageSize = (480, 320) with get, set

    member this.State = state
    
    member internal this.StatusDisplay = this.FindViewById<TextView>(Resources.Id.statusDisplay)
    member private this.NameDisplay = this.FindViewById<TextView>(Resources.Id.nameDisplay)

    member this.NameVisible
        with get() : bool = this.NameDisplay.Visibility = ViewStates.Visible
        and set(isVisible : bool) = 
            this.NameDisplay.Visibility <- if isVisible then ViewStates.Visible else ViewStates.Invisible

    override this.Dispose(disposing) =
        cancellationSource.Dispose()
        base.Dispose(disposing) 
    
    override this.OnAttachedToWindow() =
        base.OnAttachedToWindow()
        this.SetInitialState() // if called earlier, may result in preview not being set

    member this.Initialize() =
        FrameLayout.Inflate(context, Resources.Layout.PresetPreview, this) |> ignore
        this.LayoutParameters <- new ViewGroup.LayoutParams(fst this.ImageSize, snd this.ImageSize)
        this.UpdateName()
        if preset.HasRandomizedValues then 
            this.FindViewById<ImageView>(Resources.Id.random_indicator).Visibility <- ViewStates.Visible
        if not preset.InAppPurchases.IsEmpty then
            this.FindViewById<View>(Resources.Id.premium_indicator).Visibility <- ViewStates.Visible
    
    member this.UpdateName() =
        this.NameDisplay.Text <- preset.Name

    // previews cache management
    member this.CacheFilePath =
        let filename = "img-" + Java.Net.URLEncoder.Encode(preset.Name, "UTF-8") + ".png"
        context.CacheDir.Path + "/" + preset.DirName + "/" + filename
    
    member private this.EnsureCacheDir() =
        System.IO.Directory.CreateDirectory(context.CacheDir.Path + "/" + preset.DirName) |> ignore

    member this.SavePreview(bitmap : Android.Graphics.Bitmap) =
        let saveFileTask = 
            async {
                try
                    this.EnsureCacheDir()
                    use ostream = new System.IO.FileStream(this.CacheFilePath, System.IO.FileMode.Create)
                    do! bitmap.CompressAsync(Android.Graphics.Bitmap.CompressFormat.Png, 1, ostream) |> Async.AwaitTask |> Async.Ignore
                with
                | e -> this.Post(this.ShowError) |> ignore
            }
        Async.Start(saveFileTask, cancellationSource.Token)
        
    member this.GetCachedPreview() : Android.Graphics.Bitmap option =
        use file = new Java.IO.File(this.CacheFilePath)
        if preset.IsSaved && file.Exists() then
            let bitmap = Android.Graphics.BitmapFactory.DecodeFile(this.CacheFilePath)
            if not (isNull bitmap) && (bitmap.Width, bitmap.Height) = this.ImageSize then
                Some(bitmap)
            else
                None // force re-generation of thumbnail with correct dimensions
        else
            None
    
    member this.InvalidateCache() =
        use file = new Java.IO.File(this.CacheFilePath)
        if file.Exists() then
            file.Delete() |> ignore

    member this.SaveCurrentImage() =
        if not (isNull currentBitmap) then
            try
                this.EnsureCacheDir()
                use ostream = new System.IO.FileStream(this.CacheFilePath, System.IO.FileMode.Create)
                currentBitmap.Compress(Android.Graphics.Bitmap.CompressFormat.Png, 1, ostream) |> ignore
            with
            | _ -> ()

    // state changes
    member this.SetInitialState() =
        if state = PresetPreviewState.Pending then
            match this.GetCachedPreview() with
            | Some(bitmap) -> this.SetImage(bitmap)
            | None -> this.RequestGeneration()

    /// Pending | Generating -> Complete
    member this.SetImage(bitmap : Android.Graphics.Bitmap) =
        let imageView = this.FindViewById<ImageView>(Resources.Id.image)
        imageView.SetImageBitmap(bitmap)
        this.StatusDisplay.Visibility <- ViewStates.Invisible
        state <- PresetPreviewState.Complete

    /// Generating -> Error
    member this.ShowError() =
        let statusDisplay = this.StatusDisplay
        statusDisplay.Text <- "error"
        statusDisplay.SetTextColor(Android.Graphics.Color.Red)
        state <- PresetPreviewState.Error

    /// Pending -> Generating
    abstract RequestGeneration : unit -> unit
    default this.RequestGeneration() =
        let generationTask = async {
            try
                let w, h = this.ImageSize
                currentBitmap <- preset.GeneratePreview(context, w, h)
                // using Post method because one must only change UI state from UI thread
                this.Post(fun () -> this.SetImage(currentBitmap)) |> ignore
                if preset.IsSaved then
                    this.SavePreview(currentBitmap)
            with
            | e -> 
                Android.Util.Log.Debug("previews", e.Message) |> ignore
                this.Post(this.ShowError) |> ignore
        } 
        state <- PresetPreviewState.Generating
        Android.Util.Log.Debug("preview-render", sprintf "Request render preset %A in %d" this.Preset.Name (this.GetHashCode())) |> ignore
        Async.Start(generationTask, cancellationSource.Token)
        this.StatusDisplay.Text <- "Generating preview..."
    
    /// Generating -> Pending
    abstract CancelGeneration : unit -> unit
    default this.CancelGeneration() =
        cancellationSource.Cancel()
        if state = PresetPreviewState.Generating then
            this.StatusDisplay.Text <- "Loading preview..."
            state <- PresetPreviewState.Pending


type PresetPreviewAdapter(previews : System.Collections.Generic.IList<PresetPreview>) =
    inherit BaseAdapter()

    member val Previews = previews with get, set

    override this.Count = this.Previews.Count
    override this.GetItem(pos) = this.Previews.[pos] :> Java.Lang.Object
    override this.GetItemId(pos) = int64 pos

    override this.GetView(pos, convertView, parent) =
        let view = this.Previews.[pos]
        view.LongClickable <- true
        view :> View


[<AllowNullLiteral>]
type PresetSelectionFragment() =
    inherit AndroidX.Fragment.App.Fragment()
    
    let adapter = new PresetPreviewAdapter([||])

    let presetSelectedEvent = Event<Preset>()
    member this.PresetSelected = presetSelectedEvent.Publish

    member this.Previews
        with get() : IList<PresetPreview> = adapter.Previews
        and set(newValue : IList<PresetPreview>) = 
            adapter.Previews <- newValue
            this.Update()
    
    member this.Update() =
        adapter.NotifyDataSetChanged()

    member private this.OnPresetClicked(eventArgs, preview : PresetPreview) =
        let skus = preview.Preset.InAppPurchases
        if not skus.IsEmpty then
            // inform user that he needs to purchase items
            use builder = new Android.App.AlertDialog.Builder(this.Context)
            builder
                .SetTitle("Preset with premium content")
                .SetView(Resources.Layout.PremiumPresetDialog)
                .SetNegativeButton("Cancel", (fun _ _ -> ()))
                .SetPositiveButton(
                    "Buy", 
                    (fun _ _ ->
                        async {
                            let! data = InAppPurchases.getPurchasesDataAsync(skus |> Set.toArray)
                            this.Activity.RunOnUiThread(fun () -> InAppPurchases.getPurchasesDialog(this.Activity, data).Show())
                        }
                        |> Async.Start
                    )) |> ignore
            builder.Show() |> ignore
        else
            presetSelectedEvent.Trigger(preview.Preset)

    override this.OnCreateView(inflater, container, bundle) = 
         // third parameter to Inflate is used to avoid error when adding fragment using fragment manager
        let view = inflater.Inflate(Resources.Layout.PresetSelectionFragment, container, false)
        let gridView = view.FindViewById<GridView>(Resources.Id.previewGrid)
        if this.Previews.Count > 0 then
            gridView.SetColumnWidth(fst this.Previews.[0].ImageSize)
        for each in this.Previews do
            each.Initialize()
            each.Click.Add(fun args -> this.OnPresetClicked(args, each))
        gridView.Adapter <- adapter
        this.Activity.RegisterForContextMenu(gridView)
        view         

    override this.OnStop() =
        base.OnStop()
        for each in this.Previews do
            each.CancelGeneration()
