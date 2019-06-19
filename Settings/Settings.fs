namespace Settings

#if __ANDROID__
open Android.Views
open Views
#endif


[<AbstractClass>]
type SettingAttribute() =
    inherit System.Attribute()

    member val Name = "" with get, set

    abstract member RandomValue : System.Random -> obj
#if __ANDROID__
    abstract member CreateView : Android.Content.Context * System.Reflection.PropertyInfo -> ISettingEditor
    abstract member UseView : ISettingEditor * Settings * System.Reflection.PropertyInfo -> unit
    default this.UseView(view, object, property) =
        view.Accessor <- Accessor((fun () -> property.GetValue(object)), (fun value -> property.SetValue(object, value)))
        view.Title <- if System.String.IsNullOrEmpty this.Name then property.Name else this.Name
#endif

and [<AbstractClass>] 
    Settings() = 
    member this.SettingProperties =
        this.GetType().GetProperties()
        |> Array.filter (fun prop -> prop.CanRead && prop.CanWrite && prop.GetCustomAttributes(typeof<SettingAttribute>, true).Length > 0)
    
    /// Return true if any setting has Randomized value
    member this.HasRandomizedValues : bool =
        this.SettingProperties
        |> Array.exists (fun prop -> 
            let value = prop.GetValue(this)
            let typ = value.GetType()
            if Randomizeable<_>.IsRandomizableType typ then
                let case, _ = Microsoft.FSharp.Reflection.FSharpValue.GetUnionFields(value, typ)
                case.Name = "Randomized"
            else 
                false)

    override this.Equals(other : obj) =
        if LanguagePrimitives.PhysicalEquality (this :> obj) other then
            true
        elif this.GetType() <> other.GetType() then
            false
        else
            this.SettingProperties
            |> Array.forall (fun prop -> prop.GetValue(this) = prop.GetValue(other) )
    
    override this.GetHashCode() = 
        this.SettingProperties 
        |> Array.fold (fun acc prop -> prop.GetValue(this).GetHashCode() ^^^ acc) 0
    
    override this.ToString() =
        let propertyStrings = this.SettingProperties |> Array.map(fun prop -> sprintf "%s=%A" prop.Name (prop.GetValue(this)))
        sprintf "%s(%s)" (this.GetType().Name) (System.String.Join(", ", propertyStrings))

#if __ANDROID__
    abstract member CreateView : Android.Content.Context -> SettingsView
    default this.CreateView(context) =
        let view = new SettingsView(context)
        view.Editors <- 
            this.SettingProperties 
            |> Array.map (fun prop -> 
                let attr = prop.GetCustomAttributes(typeof<SettingAttribute>, true).[0] :?> SettingAttribute
                attr.CreateView(context, prop) )
        view
    
    abstract member UseView : ISettingsView -> unit
    default this.UseView(view) =
        let editors = view.Editors
        this.SettingProperties |> Array.iteri (fun i prop ->
            let editor = editors.[i]
            let attr = prop.GetCustomAttributes(typeof<SettingAttribute>, true).[0] :?> SettingAttribute
            attr.UseView(editor, this, prop) )
#endif


module Randomizeable =
    let isRandomizableType (typ : System.Type) =
        Randomizeable<_>.IsRandomizableType typ

    let getValue (random : System.Random) (propertyExpr : Quotations.Expr<Randomizeable<'T>>) : 'T =
        match propertyExpr with
        | Quotations.Patterns.PropertyGet(Some(innerExpr), prop, _) -> 
            match innerExpr with
            | Quotations.Patterns.Value(object, _) ->
                match prop.GetValue(object) :?> Randomizeable<'T> with
                | Randomized -> 
                    let attr = prop.GetCustomAttributes(typeof<SettingAttribute>, true).[0] :?> SettingAttribute
                    (attr.RandomValue random) :?> 'T
                | Defined(value) -> value
            | _ -> failwith "Property must be accessed on a value (e.g. this.Foo)"
        | _ -> failwith "Must be property access"


// concrete attributes

type EnumAttribute(options: obj[]) = 
    inherit SettingAttribute()

    let itemType = options |> Seq.map (fun each -> each.GetType()) |> Seq.distinct |> Seq.exactlyOne

    override this.RandomValue(random) =
        options.[random.Next(options.Length)]
#if __ANDROID__
    member this.CreateRandomizeableView<'T when 'T : equality>(context, options : obj[]) : ISettingEditor = 
        let view = new RandomizedSpinner<'T>(context)
        view.Initialize()
        let typedOptions = options |> Array.map (unbox<'T>)
        view.SetItems([| for item in typedOptions -> item, (item.ToString()) |])
        view :> ISettingEditor
    
    override this.CreateView(context, property) = 
        if Randomizeable.isRandomizableType property.PropertyType then
            let method = this.GetType().GetMethod("CreateRandomizeableView").MakeGenericMethod([| itemType |])
            method.Invoke(this, [| context; options |]) :?> ISettingEditor
        else
            failwith "Non-randomizeable version of enum editor is not implemented"
#endif

/// Inclusive integer range
type IntRangeAttribute(minVal: int, maxVal: int) =
    inherit SettingAttribute()
    do assert(maxVal >= minVal)

    override this.RandomValue(random) =
        random.Next(minVal, maxVal+1) |> box
#if __ANDROID__
    override this.CreateView(context, property) = 
        if Randomizeable.isRandomizableType property.PropertyType then
            let view = new RandomizedIntSeekBar(context)
            view.Initialize()
            view.SetRange(minVal, maxVal)
            view :> ISettingEditor
        else
            failwith "Non-randomizeable version of int range editor is not implemented"
#endif

/// Float range (single precision)
type FloatRangeAttribute(minVal: float32, maxVal: float32) =
    inherit SettingAttribute()
    do assert(maxVal > minVal)

    member val Step = 0.0f with get, set

    member private this.NumSteps =
        let extent = maxVal - minVal
        if this.Step > 0.0f then
            int(extent / this.Step)
        else
            20

    override this.RandomValue(random) =
        (float32(random.NextDouble()) * (maxVal - minVal) + minVal) |> box
#if __ANDROID__
    override this.CreateView(context, property) = 
        if Randomizeable.isRandomizableType property.PropertyType then
            let view = new RandomizedFloatSeekBar(context)
            view.Initialize()
            view.SetRange(minVal, maxVal, this.NumSteps)
            view :> ISettingEditor
        else
            failwith "Non-randomizeable version of int range editor is not implemented"
#endif

/// Color (RGB float32 triple). Each component is in range [1; 0].
/// float32*float32*float32 is chosen to be independent of different color implementations
type ColorAttribute() =
    inherit SettingAttribute()

    override this.RandomValue(random) =
        let r = float32(random.NextDouble())
        let g = float32(random.NextDouble())
        let b = float32(random.NextDouble())
        (r, g, b) |> box
#if __ANDROID__
    override this.CreateView(context, property) = 
        if Randomizeable.isRandomizableType property.PropertyType then
            let view = new RandomizedColorPicker(context)
            view.Initialize()
            view :> ISettingEditor
        else
            failwith "Non-randomizeable version of color picker editor is not implemented"
#endif


type BoolAttribute() =
    inherit SettingAttribute()

    override this.RandomValue(random) =
        (random.NextDouble() < 0.5)  |> box
#if __ANDROID__
    override this.CreateView(context, property) = 
        if Randomizeable.isRandomizableType property.PropertyType then
            let view = new RandomizedCheckbox(context)
            view.Initialize()
            view :> ISettingEditor
        else
            failwith "Non-randomizeable version of bool editor is not implemented"
#endif
