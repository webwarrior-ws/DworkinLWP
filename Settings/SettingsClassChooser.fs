namespace Settings

open Android.Content
open Android.Util
open Android.Views
open Android.Widget

open Views


[<AllowNullLiteral>]
type SettingsClassChooser<'T when 'T :> Settings>(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit LinearLayout(context, attrs, defStyle)
    
    let mutable classes : System.Type[] = [||]
    let mutable itemView : SettingsView option = None
    let mutable accessor : Accessor = Empty
    let changedEvent = Event<unit>()

    new(context, attrs) = new SettingsClassChooser<'T>(context, attrs, 0)
    new(context) = new SettingsClassChooser<'T>(context, null)

    static member Create(context : Context, classLabelPairs : (System.Type * string)[]) =
        let res = new SettingsClassChooser<'T>(context)
        res.Initialize()
        res.SetClasses(classLabelPairs)
        res

    member private this.Spinner = this.FindViewById<Spinner>(Resources.Id.spinner)

    member private this.SetItemView(view : SettingsView) =
        match itemView with
        | Some(oldView) -> this.RemoveView(oldView)
        | None -> ()
        this.AddView(view)
        view.Changed.Add(fun () -> changedEvent.Trigger())
        itemView <- Some(view)

    member private this.SetValue(value : 'T) =
        match itemView with
        | Some(view) when value.GetType() = view.SettingsClass -> ()
        | _ -> this.SetItemView(value.CreateView(context))
        value.UseView(itemView.Value)
        let index = classes |> Array.findIndex (fun x -> x = value.GetType())
        if index <> this.Spinner.SelectedItemPosition then
            this.Spinner.SetSelection(index)
        changedEvent.Trigger()
    
    member this.UpdateValue() =
        match accessor with
        | Empty -> ()
        | Accessor(getter, setter) -> 
            let value = getter() :?> 'T
            this.SetValue(value)

    member this.Title 
        with set(value) = this.FindViewById<TextView>(Resources.Id.title).Text <- value
    
    member this.Accessor
        with get() = accessor
        and set(newValue) =
            accessor <- newValue
            this.UpdateValue()
    
    member val Changed = changedEvent.Publish

    member this.SetClasses(classLabelPairs : (System.Type * string)[]) = 
        if not (classLabelPairs |> Array.forall (fun (cls, _) -> cls.IsSubclassOf(typeof<'T>))) then
            failwithf "all classes must be subclass of %A" (typeof<'T>)
        classes <- classLabelPairs |> Array.map fst
        let labels = classLabelPairs |> Array.map snd
        this.Spinner.Adapter <- new ArrayAdapter(context, Android.Resource.Layout.SimpleSpinnerDropDownItem, labels)

    member this.Initialize() =
        let view = LinearLayout.Inflate(context, Resources.Layout.SettingsClassChooser, this)
        view.LayoutParameters <- new ViewGroup.LayoutParams(ViewGroup.LayoutParams.MatchParent, ViewGroup.LayoutParams.WrapContent)
        (view :?> LinearLayout).Orientation <- Orientation.Vertical
        this.Spinner.ItemSelected.Add(fun args ->
            let newClass = classes.[this.Spinner.SelectedItemPosition]
            if newClass <> itemView.Value.SettingsClass then
                let newValue = newClass.GetConstructor([||]).Invoke([||]) :?> 'T
                match accessor with
                | Empty -> ()
                | Accessor(_, setter) -> 
                    this.SetValue(newValue)
                    setter newValue )
    
    override this.OnFinishInflate() =
        this.Initialize()
        base.OnFinishInflate()
