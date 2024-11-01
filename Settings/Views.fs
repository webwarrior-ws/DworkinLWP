module Settings.Views

open Android.Content
open Android.Util
open Android.Widget
open Android.Views
open AndroidX.AppCompat.App

open Cheesebaron.ColorPickers


// interfaces for setting-editing viwes
type Accessor = 
    | Empty
    | Accessor of getter: (unit -> obj) * setter : (obj -> unit)
    with
        member this.Get() = 
            match this with 
            | Empty -> null
            | Accessor(getter=f) -> f()
        
        member this.Set(value) =
            match this with
            | Empty -> ()
            | Accessor(setter=f) -> f value


type ISettingEditor =
    /// Update current value and bind value change events to use setter in new accessor
    abstract member Accessor : Accessor with set
    abstract member Title : string with set
    abstract member Changed : IEvent<unit>


type ISettingsView =
    abstract member Editors : System.Collections.Generic.IList<ISettingEditor>
    abstract member Changed : IEvent<unit>


/// View that contains setting editors. Doesn't include title.
type SettingsView(context : Context, attrs : IAttributeSet, defStyle : int) as this =
    inherit LinearLayout(context, attrs, defStyle)
    let dpToPixels(dp : float32) : int =
        TypedValue.ApplyDimension(ComplexUnitType.Dip, dp, this.Resources.DisplayMetrics) |> round |> int |> max 1

    do
        this.LayoutParameters <- new ViewGroup.MarginLayoutParams(ViewGroup.LayoutParams.MatchParent, ViewGroup.LayoutParams.WrapContent)
        this.Orientation <- Orientation.Vertical
        // dividers
        this.DividerPadding <- dpToPixels 2.0f
        this.ShowDividers <- ShowDividers.Middle
        let dividerDrawable = new Android.Graphics.Drawables.ShapeDrawable(new Android.Graphics.Drawables.Shapes.RectShape())
        dividerDrawable.SetIntrinsicHeight(dpToPixels 1.0f)
        dividerDrawable.Paint.Color <- Android.Graphics.Color.DarkGray
        this.SetDividerDrawable(dividerDrawable)

    let mutable editors : ISettingEditor[] = [||]
    let mutable settingsClass : System.Type = null
    let changedEvent = Event<unit>()

    new(context, attrs) = new SettingsView(context, attrs, 0)
    new(context) = new SettingsView(context, null)

    member this.SettingsClass 
        with get() = settingsClass
        and internal set(value) = settingsClass <- value

    member val Changed = changedEvent.Publish

    member this.Editors 
        with get() = editors
        and set(newEditors) =
            if editors.Length > 0 then
                failwith "Editors should only be set once"
            editors <- newEditors
            for each in editors do 
                this.AddView(each :?> View)
                each.Changed.Add(fun () -> changedEvent.Trigger())

    interface ISettingsView with
        member this.Editors = this.Editors :> System.Collections.Generic.IList<ISettingEditor>
        member this.Changed = this.Changed


type IRandomizedEditor<'TItem> =
    abstract CurrentValue : 'TItem with get, set
    abstract IsEnabled : bool with get, set


type RandomizedEditorComponent<'TEditor, 'TItem when 'TEditor :> View and 'TEditor :> IRandomizedEditor<'TItem>> (editor : 'TEditor) =
    let changedEvent = Event<unit>()
    let mutable accessor = Empty
    let mutable setValueOnChange = true
    
    member this.Initialize() =
        this.RandomButton.OnCheckedChangeListener <- 
            fun (button, isChecked) ->
                if editor.IsEnabled <> not isChecked then
                    editor.IsEnabled <- not isChecked
                    changedEvent.Trigger()
        this.Changed.Add(fun () -> 
            if setValueOnChange then
                let value = 
                    if this.RandomButton.Checked then
                        Randomized
                    else
                        Defined(editor.CurrentValue)
                accessor.Set(value))

    member val Changed : IEvent<unit> = changedEvent.Publish

    member this.FireChangedEvent() = changedEvent.Trigger()

    member this.RandomButton : ToggleImageButton = editor.FindViewById<ToggleImageButton>(Resource.Id.dice)

    member this.Accessor
        with set(newAccessor : Accessor) =
            match newAccessor with
            | Empty -> ()
            | Accessor(getter, _) ->
                let currItemUntyped = getter()
                let currItem = currItemUntyped :?> Randomizeable<'TItem>
                setValueOnChange <- false
                match currItem with
                | Randomized -> 
                    this.RandomButton.Checked <- true
                | Defined(value) -> 
                    this.RandomButton.Checked <- false
                    editor.CurrentValue <- value
                setValueOnChange <- true
            accessor <- newAccessor


type RandomizedSpinner<'TItem when 'TItem : equality>(context : Context, attrs : IAttributeSet, defStyle : int) as this =
    inherit LinearLayout(context, attrs, defStyle)

    let randomizedEditorComponent = RandomizedEditorComponent<RandomizedSpinner<'TItem>, 'TItem>(this)
    let mutable items : 'TItem[] = [||]

    new(context, attrs) = new RandomizedSpinner<'TItem>(context, attrs, 0)
    new(context) = new RandomizedSpinner<'TItem>(context, null)

    member val LabelingFunction : 'TItem -> string = fun x -> x.ToString() with get, set

    interface IRandomizedEditor<'TItem> with
        member this.CurrentValue 
            with get() = items.[this.Spinner.SelectedItemPosition]
            and set(value) = 
                let index = items |> Array.findIndex (fun x -> x = value)
                this.Spinner.SetSelection(index)
        
        member this.IsEnabled
            with get() = this.Spinner.Enabled
            and set(value) = this.Spinner.Enabled <- value

    interface ISettingEditor with
        member this.Changed = randomizedEditorComponent.Changed
        member this.Title 
            with set(value) = this.FindViewById<TextView>(Resource.Id.title).Text <- value
        member this.Accessor 
            with set(newAccessor) = randomizedEditorComponent.Accessor <- newAccessor

    member private this.Spinner = this.FindViewById<Spinner>(Resource.Id.spinner1)

    abstract GetAdapterForSpinner : 'TItem[] -> ArrayAdapter
    default this.GetAdapterForSpinner(itemsArray) =
        let labels = itemsArray |> Array.map this.LabelingFunction
        new ArrayAdapter(context, Android.Resource.Layout.SimpleSpinnerDropDownItem, labels)

    member this.SetItems(newItems : 'TItem[]) =
        items <- newItems
        this.Spinner.Adapter <- this.GetAdapterForSpinner(items)

    member this.Initialize() =
        let view = LinearLayout.Inflate(context, Resource.Layout.RandomizedSpinner, this)
        randomizedEditorComponent.Initialize()
        this.Spinner.ItemSelected.Add(fun args -> randomizedEditorComponent.FireChangedEvent())
    
    override this.OnFinishInflate() =
        this.Initialize()
        base.OnFinishInflate()

    static member GetFactoryForLabelingFunction(labelingFunction : 'TItem -> string) =
        fun context options ->
            let view = new RandomizedSpinner<'TItem>(context)
            view.LabelingFunction <- labelingFunction
            view.Initialize()
            view.SetItems(options |> Array.map unbox<'TItem>)
            view :> ISettingEditor


[<AbstractClass>]
type RandomizedSeekBar<'TValue>(context : Context, attrs : IAttributeSet, defStyle : int) as this =
    inherit LinearLayout(context, attrs, defStyle)

    let randomizedEditorComponent = RandomizedEditorComponent<RandomizedSeekBar<'TValue>, 'TValue>(this)

    new(context, attrs) = new RandomizedSeekBar<'TValue>(context, attrs, 0)
    new(context) = new RandomizedSeekBar<'TValue>(context, null)

    abstract ValueToProgress : 'TValue -> int
    abstract ProgressToValue : int -> 'TValue
    abstract ValueToString : 'TValue -> string
    
    member internal this.SeekBar : AndroidX.AppCompat.Widget.AppCompatSeekBar = this.FindViewById<_>(Resource.Id.seek_bar)
    member private this.NumberDisplay = this.FindViewById<TextView>(Resource.Id.number_display)

    interface IRandomizedEditor<'TValue> with
        member this.CurrentValue
            with get() = this.ProgressToValue(this.SeekBar.Progress)
            and set(value) = 
                this.SeekBar.Progress <- this.ValueToProgress(value)
                this.NumberDisplay.Text <- this.ValueToString(value)
        
        member this.IsEnabled 
            with get() = this.SeekBar.Enabled
            and set(value) = this.SeekBar.Enabled <- value

    interface ISettingEditor with
        member this.Changed = randomizedEditorComponent.Changed
        member this.Title 
            with set(value) = this.FindViewById<TextView>(Resource.Id.title).Text <- value
        member this.Accessor 
            with set(newAccessor) = randomizedEditorComponent.Accessor <- newAccessor

    member this.Initialize() =
        let view = LinearLayout.Inflate(context, Resource.Layout.RandomizedSeekbar, this)
        randomizedEditorComponent.Initialize()
        let seekbar = this.SeekBar
        seekbar.ProgressChanged.Add(fun args -> 
            let value = this.ProgressToValue(args.Progress)
            this.NumberDisplay.Text <- this.ValueToString(value)
            randomizedEditorComponent.FireChangedEvent())
    
    override this.OnFinishInflate() =
        this.Initialize()
        base.OnFinishInflate()


type RandomizedIntSeekBar(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit RandomizedSeekBar<int>(context, attrs, defStyle)

    let mutable minVal, maxVal = 0, 1

    new(context, attrs) = new RandomizedIntSeekBar(context, attrs, 0)
    new(context) = new RandomizedIntSeekBar(context, null)

    member this.SetRange(low : int, high : int) =
        assert (low < high)
        this.SeekBar.Max <- high - low
        minVal <- low
        maxVal <- high

    override this.ValueToProgress(value) = value - minVal
    override this.ProgressToValue(progress) = progress + minVal
    override this.ValueToString(value) = string value


type RandomizedFloatSeekBar(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit RandomizedSeekBar<float32>(context, attrs, defStyle)

    let mutable minVal, maxVal = 0.0f, 1.0f
    let mutable steps = 1

    new(context, attrs) = new RandomizedFloatSeekBar(context, attrs, 0)
    new(context) = new RandomizedFloatSeekBar(context, null)

    member this.SetRange(low : float32, high : float32, numSteps : int) =
        assert (low < high)
        assert (numSteps > 0)
        this.SeekBar.Max <- numSteps
        steps <- numSteps
        minVal <- low
        maxVal <- high
    
    member private this.Extent = maxVal - minVal

    override this.ValueToProgress(value) = (value - minVal) / this.Extent * float32(steps) |> int
    override this.ProgressToValue(progress) = float32(progress) / float32(steps) * this.Extent + minVal
    override this.ValueToString(value) = sprintf "%2.4g" value // TODO: use info about range


type RandomizedColorPicker(context : Context, attrs : IAttributeSet, defStyle : int) as this =
    inherit LinearLayout(context, attrs, defStyle)

    let randomizedEditorComponent = RandomizedEditorComponent<RandomizedColorPicker, RGBTriple>(this)
    let mutable color : RGBTriple = (0.0f, 0.0f, 0.0f)

    new(context, attrs) = new RandomizedColorPicker(context, attrs, 0)
    new(context) = new RandomizedColorPicker(context, null)

    interface IRandomizedEditor<RGBTriple> with
        member this.CurrentValue
            with get() = color
            and set(value) = 
                color <- value
                let r, g, b = color
                this.SetSwatchColor(Android.Graphics.Color(byte(r*255.0f), byte(g*255.0f), byte(b*255.0f)))
        
        member this.IsEnabled 
            with get() = this.Swatch.Enabled
            and set(enabled) = 
                this.Swatch.Text <- if enabled then "" else "🔒"
                this.Swatch.Enabled <- enabled

    interface ISettingEditor with
        member this.Changed = randomizedEditorComponent.Changed
        member this.Title 
            with set(value) = this.FindViewById<TextView>(Resource.Id.title).Text <- value
        member this.Accessor 
            with set(newAccessor) = randomizedEditorComponent.Accessor <- newAccessor

    member private this.Swatch = this.FindViewById<Button>(Resource.Id.swatch)

    member private this.SetSwatchColor(color) =
        this.Swatch.SetBackgroundColor(color)
    
    member private this.AndroidColor 
        with get() = 
            let r, g, b = color
            Android.Graphics.Color(byte(r*255.0f), byte(g*255.0f), byte(b*255.0f))
        and set(value : Android.Graphics.Color) = 
            color <- (float32 value.R) / 255.0f, (float32 value.G) / 255.0f, (float32 value.B) / 255.0f
            this.SetSwatchColor(value)

    member this.Initialize() =
        let view = LinearLayout.Inflate(context, Resource.Layout.RandomizedColorPicker, this)
        randomizedEditorComponent.Initialize()
        this.Click.Add(fun args ->
            if (this :> IRandomizedEditor<RGBTriple>).IsEnabled then
                let dialog = new ColorPickerDialog(context, this.AndroidColor)
                dialog.SetTitle("Choose color")
                dialog.AlphaSliderVisible <- false
                dialog.ColorChanged.Add(fun args -> 
                    this.AndroidColor <- args.Color
                    randomizedEditorComponent.FireChangedEvent() )
                dialog.Show() )
        this.Swatch.Clickable <- false
    
    override this.OnFinishInflate() =
        this.Initialize()
        base.OnFinishInflate()


type RandomizedCheckbox(context : Context, attrs : IAttributeSet, defStyle : int) as this =
    inherit LinearLayout(context, attrs, defStyle)

    let randomizedEditorComponent = RandomizedEditorComponent<RandomizedCheckbox, bool>(this)

    new(context, attrs) = new RandomizedCheckbox(context, attrs, 0)
    new(context) = new RandomizedCheckbox(context, null)

    interface IRandomizedEditor<bool> with
        member this.CurrentValue 
            with get() = this.Checkbox.Checked
            and set(value) = this.Checkbox.Checked <- value
        
        member this.IsEnabled
            with get() = this.Checkbox.Enabled
            and set(value) = 
                this.Enabled <- value
                this.Checkbox.Enabled <- value

    interface ISettingEditor with
        member this.Changed = randomizedEditorComponent.Changed
        member this.Title 
            with set(value) = this.FindViewById<TextView>(Resource.Id.title).Text <- value
        member this.Accessor 
            with set(newAccessor) = randomizedEditorComponent.Accessor <- newAccessor

    member private this.Checkbox = this.FindViewById<CheckBox>(Resource.Id.checkbox)

    member this.Initialize() =
        let view = LinearLayout.Inflate(context, Resource.Layout.RandomizedCheckbox, this)
        randomizedEditorComponent.Initialize()
        this.Click.Add(fun args ->
            this.Checkbox.Checked <- not this.Checkbox.Checked
            randomizedEditorComponent.FireChangedEvent())
        this.Checkbox.Clickable <- false

    override this.OnFinishInflate() =
        this.Initialize()
        base.OnFinishInflate()    


// customized spinner editors
type SpinnerItem = 
    {
        ImageID : int
        Label : string
    }

type ImageSpinnerAdapter(context : Context, itemsArray : SpinnerItem[]) =
    inherit ArrayAdapter<SpinnerItem>(context, Resource.Layout.SpinnerItemWithImage, itemsArray)

    let inflater : LayoutInflater = context.GetSystemService(Context.LayoutInflaterService) :?> LayoutInflater

    override this.GetView(position, convertView, parent) = 
        let itemView = inflater.Inflate(Resource.Layout.SpinnerItemWithImage, parent, false)
        let imageView = itemView.FindViewById<ImageView>(Resource.Id.image)
        imageView.SetImageResource(itemsArray.[position].ImageID)
        let textView = itemView.FindViewById<TextView>(Resource.Id.text)
        textView.Text <- itemsArray.[position].Label
        itemView

    override this.GetDropDownView(position, convertView, parent) =
        this.GetView(position, convertView, parent)


type RandomizedSpinnerWithImages<'TItem when 'TItem : equality>(context : Context, mapping : 'TItem -> SpinnerItem) =
    inherit RandomizedSpinner<'TItem>(context)
    
    override this.GetAdapterForSpinner(items) =
        new ImageSpinnerAdapter(context, items |> Array.map mapping) :> ArrayAdapter

    static member GetFactoryForMapping(mapping : 'TItem -> SpinnerItem) =
        fun context options ->
            let view = new RandomizedSpinnerWithImages<'TItem>(context, mapping)
            view.Initialize()
            view.SetItems(options |> Array.map unbox<'TItem>)
            view :> ISettingEditor


type GridImageAdapter(context : Context, itemsArray : SpinnerItem[]) =
    inherit ArrayAdapter<SpinnerItem>(context, Resource.Layout.SpinnerItemWithImage, itemsArray)

    override this.GetView(position, convertView, parent) = 
        let view = new ImageView(context)
        view.SetImageResource(itemsArray.[position].ImageID)
        let wrapContent = int ViewGroup.LayoutParams.WrapContent
        view.LayoutParameters <- new ViewGroup.LayoutParams(wrapContent, wrapContent)
        view :> View


/// Editor for choosing items from list of options. If NumColumns = 1 (default), then show list of options (picture & label), 
/// otherwise - grid of pictures
type RandomizedItemChooser<'TItem when 'TItem : equality>(context : Context, mapping : 'TItem -> SpinnerItem) as this =
    inherit LinearLayout(context, null, 0)

    let randomizedEditorComponent = RandomizedEditorComponent<RandomizedItemChooser<'TItem>, 'TItem>(this)
    let mutable items : 'TItem[] = [||]
    let mutable selectedIndex = 0

    member val NumColumns = 1 with get, set

    interface IRandomizedEditor<'TItem> with
        member this.CurrentValue 
            with get() = items.[selectedIndex]
            and set(value) = 
                selectedIndex <- Array.findIndex ((=) value) items
                this.UpdateDisplay(value)
        
        member this.IsEnabled
            with get() = this.Enabled
            and set(value) = 
                this.Enabled <- value
                if not value then
                    this.ImageView.SetColorFilter(Android.Graphics.Color.Argb(127, 127, 127, 127))
                else
                    this.ImageView.SetColorFilter(null)

    interface ISettingEditor with
        member this.Changed = randomizedEditorComponent.Changed
        member this.Title 
            with set(value) = this.FindViewById<TextView>(Resource.Id.title).Text <- value
        member this.Accessor 
            with set(newAccessor) = randomizedEditorComponent.Accessor <- newAccessor

    member private this.ImageView = this.FindViewById<ImageView>(Resource.Id.imageView)

    member private this.UpdateDisplay(value : 'TItem) =
        let mappedValue = mapping value
        this.ImageView.SetImageResource(mappedValue.ImageID)
        this.FindViewById<TextView>(Resource.Id.item_name).Text <- mappedValue.Label

    member this.SetItems(newItems : 'TItem[]) =
        items <- newItems

    member this.Initialize() =
        let view = LinearLayout.Inflate(context, Resource.Layout.RandomizedItemChooser, this)
        randomizedEditorComponent.Initialize()
        this.Click.Add(fun args ->
            use builder = new AlertDialog.Builder(context, Resource.Style.AlertDialogTheme)
            let titleView = this.FindViewById<TextView>(Resource.Id.title)
            
            builder.SetTitle("Select " + titleView.Text.ToLower()) |> ignore
            let dialog : AlertDialog = 
                if this.NumColumns > 1 then
                    let grid = GridView.Inflate(context, Resource.Layout.ItemChooserGrid, null) :?> GridView
                    grid.NumColumns <- this.NumColumns
                    builder.SetView(grid) |> ignore
                    grid.Adapter <- new GridImageAdapter(context, items |> Array.map mapping)
                    let dialog = builder.Create()

                    let callback (args : AdapterView.ItemClickEventArgs) =
                        selectedIndex <- args.Position
                        this.UpdateDisplay(items.[selectedIndex])
                        randomizedEditorComponent.FireChangedEvent()
                        dialog.Dismiss()

                    grid.ItemClick.Add(callback)
                    dialog
                else
                    let imageAdapter = new ImageSpinnerAdapter(context, items |> Array.map mapping)
                    let callback _ (args : DialogClickEventArgs) =
                        selectedIndex <- args.Which
                        this.UpdateDisplay(items.[selectedIndex])
                        randomizedEditorComponent.FireChangedEvent()
                    builder.SetAdapter(imageAdapter, callback) |> ignore
                    builder.Create()
            dialog.Show() |> ignore)

    override this.OnFinishInflate() =
        this.Initialize()
        base.OnFinishInflate()    

    static member GetFactoryForMapping(mapping : 'TItem -> SpinnerItem, ?numColumns : int) =
        fun context options ->
            let view = new RandomizedItemChooser<'TItem>(context, mapping, NumColumns=defaultArg numColumns 1)
            view.Initialize()
            view.SetItems(options |> Array.map unbox<'TItem>)
            view :> ISettingEditor