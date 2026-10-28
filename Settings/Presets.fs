module Settings.Presets


open System.Collections.Generic

open Android.Content
open Android.Util
open Android.Widget
open Android.Views
open AndroidX.RecyclerView.Widget


[<AbstractClass>]
type Preset() =
    member val Name = "" with get, set
    /// Indicates whether preset is saved to disk
    member val IsSaved = false with get, set 
    /// True if preset is pre-installed with application
    member val IsBuiltin = false with get, set

    // to be overriden in subclasses:
    abstract GeneratePreview : Context * int * int -> Async<Android.Graphics.Bitmap>
    /// Name used to define sub-directories for storage of presets and preview cache. 
    /// Has to be unique for each type of preset.
    abstract DirName : string
    
    abstract HasRandomizedValues : bool

    abstract UsedClasses : System.Type[]


/// Functions and classes used to manage presets (save/load/etc.)
module PresetManagement =
    let private fileExtension = ".preset"

    let getPresetsDirPath (context : Context) (dirName : string) =
        context.FilesDir.Path + "/presets/" + dirName

    let ensurePresetsDir (context : Context) (dirName : string) =
        System.IO.Directory.CreateDirectory(getPresetsDirPath context dirName) |> ignore

    let getFilePath (context : Context) (dirName : string) (name : string) =
        let filename = Java.Net.URLEncoder.Encode(name, "UTF-8") + fileExtension
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
        if not (filePath.EndsWith fileExtension) then
            failwithf "preset files must end with %s" fileExtension
        else
            use file = new Java.IO.File(filePath)
            let encodedName = file.Name.Remove(file.Name.Length - fileExtension.Length)
            let name = Java.Net.URLDecoder.Decode(encodedName, "UTF-8")
            use stream = new System.IO.FileStream(file.AbsolutePath, System.IO.FileMode.Open)
            let json = System.Json.JsonValue.Load(stream)
            let preset : 'TPreset = Serialization.fromJsonValue(json)
            preset.Name <- name
            preset.IsSaved <- true
            preset

    let loadPresetsFromAssets<'TPreset when 'TPreset :> Preset> (context : Context) (dirName : string) : 'TPreset[] =
        let dirPath = "presets/" + dirName
        [|
            for fileName in context.Assets.List(dirPath) do
                if fileName.EndsWith(fileExtension) then
                    let encodedName = fileName.Remove(fileName.Length - fileExtension.Length)
                    let name = Java.Net.URLDecoder.Decode(encodedName, "UTF-8")
                    use stream = context.Assets.Open(dirPath + "/" + fileName)
                    let json = System.Json.JsonValue.Load(stream)
                    let preset : 'TPreset = Serialization.fromJsonValue(json)
                    preset.Name <- name
                    preset.IsSaved <- true
                    preset.IsBuiltin <- true
                    yield preset
        |]



type PresetPreviewState =
    | Pending
    | Generating
    | Complete
    | Error
    | Disposed

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
    member private this.NameDisplayContainer = this.FindViewById<View>(Resources.Id.nameDisplayContainer)

    member this.NameVisible
        with get() : bool = this.NameDisplayContainer.Visibility = ViewStates.Visible
        and set(isVisible : bool) = 
            this.NameDisplayContainer.Visibility <- if isVisible then ViewStates.Visible else ViewStates.Invisible

    override this.Dispose(disposing) =
        cancellationSource.Cancel()
        cancellationSource.Dispose()
        //if not (isNull currentBitmap) then
        //    currentBitmap.Dispose()
        currentBitmap <- null
        state <- Disposed
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

    member this.UpdateName() =
        this.NameDisplay.Text <- preset.Name

    // previews cache management
    member this.GetCacheFilePathFor(preset: Preset) =
        let filename = "img-" + Java.Net.URLEncoder.Encode(preset.Name, "UTF-8") + ".png"
        context.CacheDir.Path + "/" + preset.DirName + "/" + filename

    member this.CacheFilePath =
        this.GetCacheFilePathFor(preset)
    
    member private this.EnsureCacheDir() =
        System.IO.Directory.CreateDirectory(context.CacheDir.Path + "/" + preset.DirName) |> ignore

    member this.SavePreview(bitmap : Android.Graphics.Bitmap) =
        let saveFileTask = 
            async {
                try
                    this.EnsureCacheDir()
#if DEBUG
                    Android.Util.Log.Debug("presetPreview", sprintf "Save bitmap for %s to cache: %s" this.Preset.Name this.CacheFilePath) |> ignore
#endif
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
#if DEBUG
                Android.Util.Log.Debug("presetPreview", sprintf "Load bitmap for %s from cache: %s" this.Preset.Name this.CacheFilePath) |> ignore
#endif
                Some(bitmap)
            else
                None // force re-generation of thumbnail with correct dimensions
        else
            None
    
    member this.InvalidateCache() =
        use file = new Java.IO.File(this.CacheFilePath)
        if file.Exists() then
#if DEBUG
            Android.Util.Log.Debug("presetPreview", sprintf "Delete cached image: %s" this.CacheFilePath) |> ignore
#endif
            file.Delete() |> ignore

    member this.SaveCurrentImageForAnotherPreset(anotherPreset: Preset) =
        if not (isNull currentBitmap) then
            try
                this.EnsureCacheDir()
                let cacheFilePath = this.GetCacheFilePathFor(anotherPreset)
                use ostream = new System.IO.FileStream(cacheFilePath, System.IO.FileMode.Create)
                let result = currentBitmap.Compress(Android.Graphics.Bitmap.CompressFormat.Png, 1, ostream) 
#if DEBUG
                Android.Util.Log.Debug("presetPreview", sprintf "Save bitmap for %s to cache: %s" anotherPreset.Name cacheFilePath) |> ignore
                if not result then
                    Android.Util.Log.Debug("presetPreview", sprintf "Could not save bitmap for %s to cache" anotherPreset.Name) |> ignore
#else
                ()
#endif
            with
            | exn -> 
                Android.Util.Log.Error("presetPreview", exn.Message) |> ignore

    // state changes
    member this.SetInitialState() =
        if state = PresetPreviewState.Pending then
            try
                match this.GetCachedPreview() with
                | Some(bitmap) -> this.SetImage(bitmap)
                | None -> this.RequestGeneration()
            with
            | :? Java.Lang.OutOfMemoryError ->
                this.ShowError()

    /// Pending | Generating -> Complete
    member this.SetImage(bitmap : Android.Graphics.Bitmap) =
        if state <> Disposed then
            let imageView = this.FindViewById<ImageView>(Resources.Id.image)
            imageView.SetImageBitmap(bitmap)
            this.StatusDisplay.Visibility <- ViewStates.Invisible
            state <- PresetPreviewState.Complete

    /// Generating -> Error
    member this.ShowError() =
        if state <> Disposed then
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
                let! bitmap = preset.GeneratePreview(context, w, h)
                // using Post method because one must only change UI state from UI thread
                this.Post(fun () -> this.SetImage(bitmap)) |> ignore
                if preset.IsSaved then
                    this.SavePreview(bitmap)
                currentBitmap <- bitmap
            with
            | e -> 
                Android.Util.Log.Debug("presetPreview", e.Message) |> ignore
                this.Post(this.ShowError) |> ignore
        } 
        state <- PresetPreviewState.Generating
#if DEBUG
        Android.Util.Log.Debug("preview-render", sprintf "Request render preset %A in %d" this.Preset.Name (this.GetHashCode())) |> ignore
#endif
        Async.Start(generationTask, cancellationSource.Token)
        this.StatusDisplay.Text <- "Generating preview..."
    
    /// Generating -> Pending
    abstract CancelGeneration : unit -> unit
    default this.CancelGeneration() =
        cancellationSource.Cancel()
        if state = PresetPreviewState.Generating then
            this.StatusDisplay.Text <- "Loading preview..."
            state <- PresetPreviewState.Pending


type SpacingItemDecoration(context: Context) =
    inherit RecyclerView.ItemDecoration()

    member val Spacing = int (context.Resources.GetDimension(Resources.Dimension.preset_preview_spacing))

    override this.GetItemOffsets(outRect, view, parent, state) =
        outRect.Set(this.Spacing, this.Spacing, this.Spacing, this.Spacing)
        

type PresetPreviewViewHolder(context) =
    inherit RecyclerView.ViewHolder(new FrameLayout(context) :> View)

type PresetPreviewAdapter(previews : System.Collections.Generic.IList<PresetPreview>) =
    inherit RecyclerView.Adapter()

    member val ParentFragment : PresetSelectionFragment = null with get, set
    
    override this.ItemCount = previews.Count

    override this.OnCreateViewHolder(parent, viewType) =
        new PresetPreviewViewHolder(parent.Context) :> RecyclerView.ViewHolder

    override this.OnBindViewHolder(holder, pos) =
        let view = previews.[pos]
        view.Clickable <- true
        view.LongClickable <- true
        view.DescendantFocusability <- DescendantFocusability.BlockDescendants
        view.Click.Add(fun args -> this.ParentFragment.OnPresetClicked(view))
        holder.ItemView <- view


and [<AllowNullLiteral>]
     PresetSelectionFragment(previews: IList<PresetPreview>) =
    inherit AndroidX.Fragment.App.Fragment()
    
    let adapter = new PresetPreviewAdapter(previews)

    let presetSelectedEvent = Event<Preset>()
    member this.PresetSelected = presetSelectedEvent.Publish
        
    member this.Update() =
        adapter.NotifyDataSetChanged()

    member this.OnPresetClicked(preview : PresetPreview) =
        presetSelectedEvent.Trigger(preview.Preset)

    override this.OnCreateView(inflater, container, bundle) = 
        // third parameter to Inflate is used to avoid error when adding fragment using fragment manager
        let view = inflater.Inflate(Resources.Layout.PresetSelectionFragment, container, false)
        let recyclerView = view.FindViewById<RecyclerView>(Resources.Id.previewGrid)
        for each in previews do
            each.Initialize()
            this.Activity.RegisterForContextMenu(each)
        adapter.ParentFragment <- this
        let itemDecoration = new SpacingItemDecoration(this.Context)
        let numColumns = 
            if previews.Count > 0 then
                let previewWidth, _ = previews.[0].ImageSize
                let screenWidth = this.Context.Resources.DisplayMetrics.WidthPixels
                if screenWidth > 0 then 
                    (screenWidth - itemDecoration.Spacing) / (previewWidth + itemDecoration.Spacing) 
                else 2
            else 2
        recyclerView.SetLayoutManager(new GridLayoutManager(this.Context, numColumns))
        recyclerView.SetAdapter(adapter)
        recyclerView.AddItemDecoration(itemDecoration)
        view         

    override this.OnStop() =
        base.OnStop()
        for each in previews do
            each.CancelGeneration()
