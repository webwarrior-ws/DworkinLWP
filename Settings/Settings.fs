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
        Microsoft.FSharp.Reflection.FSharpType.IsUnion typ && 
        (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases typ |> Array.map (fun x -> x.Name) = [| "Random"; "Defined" |])

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
        failwith "notimplemented"
#endif

/// Float range (single precision)
type FloatRangeAttribute(minVal: float32, maxVal: float32) =
    inherit SettingAttribute()
    do assert(maxVal > minVal)

    override this.RandomValue(random) =
        (float32(random.NextDouble()) * (maxVal - minVal) + minVal) |> box
#if __ANDROID__
    override this.CreateView(context, property) = 
        failwith "notimplemented"
#endif

type ColorAttribute() =
    inherit SettingAttribute()

    override this.RandomValue(random) =
        let r = float32(random.NextDouble())
        let g = float32(random.NextDouble())
        let b = float32(random.NextDouble())
        OpenTK.Graphics.Color4(r, g, b, 1.0f) |> box
#if __ANDROID__
    override this.CreateView(context, property) = 
        failwith "notimplemented"
#endif
