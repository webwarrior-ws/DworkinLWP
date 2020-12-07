module Settings.Presets

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
    abstract GeneratePreview : int * int -> Android.Graphics.Bitmap
    /// Name used to define sub-directories for storage of presets and preview cache. 
    /// Has to be unique for each type of preset.
    abstract DirName : string



type PresetPreviewState =
    | Pending
    | Generating
    | Complete
    | Error

/// Preset preview widget 
type PresetPreview(context : Context, preset : Preset) =
    inherit FrameLayout(context, null, 0)
    
    let mutable state = PresetPreviewState.Pending
    let cancellationSource = new System.Threading.CancellationTokenSource()

    member this.Preset = preset
    /// Size of preview in pixels
    member val ImageSize = (480, 320) with get, set

    member this.State = state
    
    member internal this.StatusDisplay = this.FindViewById<TextView>(Resources.Id.statusDisplay)

    override this.Dispose(disposing) =
        cancellationSource.Dispose()
        base.Dispose(disposing)
        base.OnFinishInflate() 
        
    member this.Initialize() =
        FrameLayout.Inflate(context, Resources.Layout.PresetPreview, this) |> ignore
        this.LayoutParameters <- new ViewGroup.LayoutParams(fst this.ImageSize, snd this.ImageSize)
        this.FindViewById<TextView>(Resources.Id.nameDisplay).Text <- preset.Name
        this.SetInitialState()
    
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
                | e -> this.ShowError()
            }
        Async.Start(saveFileTask, cancellationSource.Token)
        
    member this.GetCachedPreview() : Android.Graphics.Bitmap option =
        use file = new Java.IO.File(this.CacheFilePath)
        if preset.IsSaved && file.Exists() then
            let bitmap = Android.Graphics.BitmapFactory.DecodeFile(this.CacheFilePath)
            if (bitmap.Width, bitmap.Height) = this.ImageSize then
                Some(bitmap)
            else
                None // force re-generation of thumbnail with correct dimensions
        else
            None

    // state changes
    member this.SetInitialState() =
        state <- PresetPreviewState.Pending
        match this.GetCachedPreview() with
        | Some(bitmap) -> this.SetImage(bitmap)
        | None -> this.RequestGeneration()

    /// Pending | Generating -> Complete
    member this.SetImage(bitmap : Android.Graphics.Bitmap) =
        let imageView = this.FindViewById<ImageView>(Resources.Id.image)
        // using Post method because one must only change UI state from UI thread
        imageView.Post(fun () ->
            imageView.SetImageBitmap(bitmap)
            this.StatusDisplay.Visibility <- ViewStates.Invisible
            state <- PresetPreviewState.Complete
        ) |> ignore

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
                let bitmap = preset.GeneratePreview(this.ImageSize)
                this.SetImage(bitmap)
                if preset.IsSaved then
                    this.SavePreview(bitmap)
            with
            | e -> this.ShowError()
        } 
        Async.Start(generationTask, cancellationSource.Token)
        this.StatusDisplay.Text <- "Generating preview..."
        state <- PresetPreviewState.Generating
    
    /// Generating -> Pending
    abstract CancelGeneration : unit -> unit
    default this.CancelGeneration() =
        cancellationSource.Cancel()
        if state = PresetPreviewState.Generating then
            this.StatusDisplay.Text <- "Loading preview..."
            state <- PresetPreviewState.Pending


type PresetPreviewAdapter(previews : PresetPreview[]) =
    inherit BaseAdapter()

    override this.Count = previews.Length
    override this.GetItem(pos) = previews.[pos] :> Java.Lang.Object
    override this.GetItemId(pos) = int64 pos

    override this.GetView(pos, convertView, parent) =
        previews.[pos] :> View


type PresetSelectionFragment() =
    inherit Android.Support.V4.App.Fragment()
    
    let presetSelectedEvent = Event<Preset>()

    member this.PresetSelected = presetSelectedEvent.Publish

    member val Previews : PresetPreview[] = [||] with get, set

    override this.OnCreateView(inflater, container, bundle) = 
         // third parameter to Inflate is used to avoid error when adding fragment using fragment manager
        let view = inflater.Inflate(Resources.Layout.PresetSelectionFragment, container, false)
        let gridView = view.FindViewById<GridView>(Resources.Id.previewGrid)
        if this.Previews.Length > 0 then
            gridView.SetColumnWidth(fst this.Previews.[0].ImageSize)
        for each in this.Previews do
            each.Click.Add(fun _ -> presetSelectedEvent.Trigger(each.Preset))
        gridView.Adapter <- new PresetPreviewAdapter(this.Previews)
        view

    override this.OnStop() =
        base.OnStop()
        for each in this.Previews do
            each.CancelGeneration()
