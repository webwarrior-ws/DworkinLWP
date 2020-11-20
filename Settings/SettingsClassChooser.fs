namespace Settings

open Android.Content
open Android.Util
open Android.Views
open Android.Widget

open Views



type AndroidSettingsUIBuilder(context : Android.Content.Context, property : System.Reflection.PropertyInfo) =
    interface ISettingsUIBuilder<ISettingEditor> with
        override this.BuildEnumControl(options, typ, representation) = this.BuildEnumControl(options, typ, representation)
        override this.BuildIntRangeControl(minVal, maxVal) = this.BuildIntRangeControl(minVal, maxVal) 
        override this.BuildFloatRangeControl(minVal, maxVal, step) = this.BuildFloatRangeControl(minVal, maxVal, step)
        override this.BuildBoolControl() = this.BuildBoolControl()
        override this.BuildColorControl() = this.BuildColorControl()

    member this.CreateRandomizeableEnumEditor<'T when 'T : equality>(options : obj[]) : ISettingEditor = 
        let view = new RandomizedSpinner<'T>(context)
        view.Initialize()
        let typedOptions = options |> Array.map (unbox<'T>)
        view.SetItems([| for item in typedOptions -> item, (item.ToString()) |])
        view :> ISettingEditor

    member this.BuildEnumControl(options, itemType, representation) = 
        if Randomizeable.isRandomizableType property.PropertyType then
            match representation with
            | EnumRepresentation.Simple ->
                let method = this.GetType().GetMethod("CreateRandomizeableEnumEditor").MakeGenericMethod([| itemType |])
                method.Invoke(this, [| options |]) :?> ISettingEditor
        else
            failwith "Non-randomizeable version of enum editor is not implemented"

    member this.BuildIntRangeControl(minVal, maxVal) = 
        if Randomizeable.isRandomizableType property.PropertyType then
            let view = new RandomizedIntSeekBar(context)
            view.Initialize()
            view.SetRange(minVal, maxVal)
            view :> ISettingEditor
        else
            failwith "Non-randomizeable version of int range editor is not implemented"

    member this.BuildFloatRangeControl(minVal, maxVal, step) = 
        if Randomizeable.isRandomizableType property.PropertyType then
            let view = new RandomizedFloatSeekBar(context)
            view.Initialize()
            view.SetRange(minVal, maxVal, int((maxVal - minVal) / step))
            view :> ISettingEditor
        else
            failwith "Non-randomizeable version of int range editor is not implemented"

    member this.BuildBoolControl() =
        if Randomizeable.isRandomizableType property.PropertyType then
            let view = new RandomizedCheckbox(context)
            view.Initialize()
            view :> ISettingEditor
        else
            failwith "Non-randomizeable version of bool editor is not implemented"

    member this.BuildColorControl() = 
        if Randomizeable.isRandomizableType property.PropertyType then
            let view = new RandomizedColorPicker(context)
            view.Initialize()
            view :> ISettingEditor
        else
            failwith "Non-randomizeable version of color picker editor is not implemented"


[<AutoOpen>]
module SettingsExtensions =
    type Settings with
        member this.CreateView(context : Android.Content.Context) =
            let view = new SettingsView(context, SettingsClass=this.GetType())
            view.Editors <- 
                this.SettingProperties 
                |> Array.map (fun prop -> 
                    let builder = AndroidSettingsUIBuilder(context, prop)
                    let attr = prop.GetCustomAttributes(typeof<SettingAttribute>, true).[0] :?> SettingAttribute
                    attr.BuildUIControl(builder) )
            view

        member this.UseView(view : ISettingsView) =
            let useAttributeView (editor : ISettingEditor) (prop : System.Reflection.PropertyInfo) =
                let attr = prop.GetCustomAttributes(typeof<SettingAttribute>, true).[0] :?> SettingAttribute
                editor.Accessor <- Accessor((fun () -> prop.GetValue(this)), (fun value -> prop.SetValue(this, value)))
                editor.Title <- if System.String.IsNullOrEmpty attr.Name then prop.Name else attr.Name
            Seq.iter2 
                useAttributeView
                view.Editors 
                this.SettingProperties



[<AllowNullLiteral>]
type SettingsClassChooser<'T when 'T :> Settings>(context : Context, attrs : IAttributeSet, defStyle : int) =
    inherit LinearLayout(context, attrs, defStyle)
    
    let mutable classes : System.Type[] = [||]
    let mutable itemView : SettingsView option = None
    let mutable accessor : Accessor = Empty
    let changedEvent = Event<unit>()

    new(context, attrs) = new SettingsClassChooser<'T>(context, attrs, 0)
    new(context) = new SettingsClassChooser<'T>(context, null)

    static member Create(context : Context, classes : System.Type[]) =
        let res = new SettingsClassChooser<'T>(context)
        res.Initialize()
        res.SetClasses(classes)
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

    member this.SetClasses(newClasses : System.Type[]) = 
        if not (newClasses |> Array.forall (fun cls -> cls.IsSubclassOf(typeof<'T>))) then
            failwithf "all classes must be subclass of %A" (typeof<'T>)
        let labels = 
            newClasses 
            |> Array.map (fun cls -> 
                let name = 
                    cls.GetCustomAttributes(false)
                    |> Array.tryPick (fun attr -> match attr with | :? NameAttribute as na -> Some(na.Name) | _ -> None)
                match name with
                | Some(x) -> x
                | None -> cls.Name )
        this.Spinner.Adapter <- new ArrayAdapter(context, Android.Resource.Layout.SimpleSpinnerDropDownItem, labels)
        classes <- newClasses

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
