module Settings.Views

open Android.Content
open Android.Util
open Android.Widget
open Android.Views

open Top.Defaults.ColorPickerLib


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
    do
        this.LayoutParameters <- new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MatchParent, ViewGroup.LayoutParams.WrapContent)
        this.Orientation <- Orientation.Vertical

    let mutable editors : ISettingEditor[] = [||]
    let changedEvent = Event<unit>()

    new(context, attrs) = new SettingsView(context, attrs, 0)
    new(context) = new SettingsView(context, null)

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
    

type RandomizedSpinner<'TItem when 'TItem : equality>(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit LinearLayout(context, attrs, defStyle)

    let changedEvent = Event<unit>()
    let mutable items : 'TItem[] = [||]
    let mutable accessor = Empty

    new(context, attrs) = new RandomizedSpinner<'TItem>(context, attrs, 0)
    new(context) = new RandomizedSpinner<'TItem>(context, null)

    interface ISettingEditor with
        member this.Changed = this.Changed
        member this.Title 
            with set(value) = this.FindViewById<TextView>(Resources.Id.title).Text <- value
        member this.Accessor 
            with set(newAccessor) =
                match newAccessor with
                | Empty -> ()
                | Accessor(getter, _) ->
                    let currItem = getter() :?> Randomizeable<'TItem>
                    match currItem with
                    | Randomized -> this.Button.Checked <- true
                    | Defined(value) -> 
                        this.Button.Checked <- false
                        let index = items |> Array.findIndex (fun x -> x = value)
                        this.Spinner.SetSelection(index)
                accessor <- newAccessor
    
    member val Changed = changedEvent.Publish

    member private this.Button = this.FindViewById<ToggleImageButton>(Resources.Id.dice)
    member private this.Spinner = this.FindViewById<Spinner>(Resources.Id.spinner1)

    member this.SetItems(itemLabelPairs : ('TItem * string)[]) =
        items <- itemLabelPairs |> Array.map fst
        let labels = itemLabelPairs |> Array.map snd
        this.Spinner.Adapter <- new ArrayAdapter(context, Android.Resource.Layout.SimpleSpinnerDropDownItem, labels)

    member this.SelectedItem : Randomizeable<'TItem> = 
        if this.Button.Checked then
            Randomized
        else
            Defined(items.[this.Spinner.SelectedItemPosition])

    member this.Initialize() =
        let view = LinearLayout.Inflate(context, Resources.Layout.RandomizedSpinner, this)
        view.LayoutParameters <- new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MatchParent, ViewGroup.LayoutParams.WrapContent)
        (view :?> LinearLayout).Orientation <- Orientation.Vertical
        let button = this.Button
        let spinner = this.Spinner
        button.OnCheckedChangeListener <- 
            fun (button, isChecked) ->
                if spinner.Enabled <> not isChecked then
                    spinner.Enabled <- not isChecked
                    changedEvent.Trigger()
        spinner.ItemSelected.Add(fun args -> changedEvent.Trigger())
        this.Changed.Add(fun () -> accessor.Set(this.SelectedItem))
    
    override this.OnFinishInflate() =
        this.Initialize()
        base.OnFinishInflate()


[<AbstractClass>]
type RandomizedSeekBar<'TValue>(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit LinearLayout(context, attrs, defStyle)

    let changedEvent = Event<unit>()
    let mutable accessor = Empty

    new(context, attrs) = new RandomizedSeekBar<'TValue>(context, attrs, 0)
    new(context) = new RandomizedSeekBar<'TValue>(context, null)

    abstract ValueToProgress : 'TValue -> int
    abstract ProgressToValue : int -> 'TValue
    abstract ValueToString : 'TValue -> string
    
    member private this.Button = this.FindViewById<ToggleImageButton>(Resources.Id.dice)
    member internal this.SeekBar : SeekBar = this.FindViewById<SeekBar>(Resources.Id.seek_bar)
    member private this.NumberDisplay = this.FindViewById<TextView>(Resources.Id.number_display)

    interface ISettingEditor with
        member this.Changed = this.Changed
        member this.Title 
            with set(value) = this.FindViewById<TextView>(Resources.Id.title).Text <- value
        member this.Accessor 
            with set(newAccessor) =
                match newAccessor with
                | Empty -> ()
                | Accessor(getter, _) ->
                    let currValue = getter() :?> Randomizeable<'TValue>
                    match currValue with
                    | Randomized -> this.Button.Checked <- true
                    | Defined(value) -> 
                        this.Button.Checked <- false
                        this.SeekBar.Progress <- this.ValueToProgress(value)
                        this.NumberDisplay.Text <- this.ValueToString(value)
                accessor <- newAccessor
    
    member val Changed = changedEvent.Publish

    member this.Initialize() =
        let view = LinearLayout.Inflate(context, Resources.Layout.RandomizedSeekbar, this)
        view.LayoutParameters <- new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MatchParent, ViewGroup.LayoutParams.WrapContent)
        (view :?> LinearLayout).Orientation <- Orientation.Vertical
        let button = this.Button
        let seekbar = this.SeekBar
        button.OnCheckedChangeListener <- 
            fun (button, isChecked) ->
                if seekbar.Enabled <> not isChecked then
                    seekbar.Enabled <- not isChecked
                    changedEvent.Trigger()
        seekbar.ProgressChanged.Add(fun args -> changedEvent.Trigger())
        this.Changed.Add(
            fun () -> 
                let newVal = 
                    if this.Button.Checked then 
                        Randomized 
                    else 
                        let value = this.ProgressToValue(seekbar.Progress)
                        this.NumberDisplay.Text <- this.ValueToString(value)
                        Defined(value)
                accessor.Set(newVal) )
    
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
        this.SeekBar.Min <- 0
        this.SeekBar.Max <- numSteps
        steps <- numSteps
        minVal <- low
        maxVal <- high

    override this.ValueToProgress(value) = (value / (maxVal - minVal) - minVal) * float32(steps) |> int
    override this.ProgressToValue(progress) = float32(progress) / float32(steps) * (maxVal - minVal) + minVal
    override this.ValueToString(value) = sprintf "%2.4g" value // TODO: use info about range


type RandomizedColorPicker(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit LinearLayout(context, attrs, defStyle)

    let changedEvent = Event<unit>()
    let mutable accessor = Empty
    let mutable color : RGBTriple = (0.0f, 0.0f, 0.0f)

    new(context, attrs) = new RandomizedColorPicker(context, attrs, 0)
    new(context) = new RandomizedColorPicker(context, null)

    interface ISettingEditor with
        member this.Changed = this.Changed
        member this.Title 
            with set(value) = this.FindViewById<TextView>(Resources.Id.title).Text <- value
        member this.Accessor 
            with set(newAccessor) =
                match newAccessor with
                | Empty -> ()
                | Accessor(getter, _) ->
                    let currItem = getter() :?> Randomizeable<RGBTriple>
                    match currItem with
                    | Randomized -> this.Button.Checked <- true
                    | Defined(r, g, b) -> 
                        this.Button.Checked <- false
                        this.AndroidColor <- Android.Graphics.Color(byte(r*255.0f), byte(g*255.0f), byte(b*255.0f))
                accessor <- newAccessor
    
    member val Changed = changedEvent.Publish

    member private this.Button = this.FindViewById<ToggleImageButton>(Resources.Id.dice)
    member private this.Swatch = this.FindViewById<Button>(Resources.Id.swatch)

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
        let view = LinearLayout.Inflate(context, Resources.Layout.RandomizedColorPicker, this)
        view.LayoutParameters <- new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MatchParent, ViewGroup.LayoutParams.WrapContent)
        (view :?> LinearLayout).Orientation <- Orientation.Horizontal
        let button = this.Button
        let swatch = this.Swatch
        button.OnCheckedChangeListener <- 
            fun (button, isChecked) ->
                if swatch.Enabled <> not isChecked then
                    swatch.Enabled <- not isChecked
                    changedEvent.Trigger()
        this.Swatch.Click.Add(fun args ->
            let dialog = 
                ColorPickerPopup.Builder(context)
                    .InitialColor(this.AndroidColor.ToArgb())
                    .OkTitle("Choose")
                    .CancelTitle("Cancel")
                    .ShowValue(true)
                    .Build()
            let handler = 
                { new ColorPickerPopup.IColorPickerObserver with 
                    override popup.OnColorPicked(c) = 
                        this.AndroidColor <- Android.Graphics.Color(c)
                        changedEvent.Trigger()
                    override popup.OnColor(c, fromUser) = () }
            dialog.Show(this, handler) )
        this.Changed.Add(fun () -> 
            let newVal = 
                if this.Button.Checked then Randomized 
                else Defined(color)
            accessor.Set(newVal))
    
    override this.OnFinishInflate() =
        this.Initialize()
        base.OnFinishInflate()