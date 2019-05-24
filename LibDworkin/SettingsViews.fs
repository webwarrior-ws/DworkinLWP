module LibDworkin.SettingsViews

open Android.Content
open Android.Util
open Android.Widget
open Android.Views

open SpecializedCollections

type private Resources = LibDworkin.Resource


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
type SettingsView(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit LinearLayout(context, attrs, defStyle)

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