namespace Settings



/// Determines which UI control to use for EnumAttribute.
/// To register custom editor for an EnumRepresentation, call AndroidSettingsUIBuilder.RegisterEnumEditorFactory method.
type EnumRepresentation =
   | Simple = 0 // dropdown with text entries
   | PCBEndpoints = 1


type ISettingsUIBuilder<'TControl> =
    abstract BuildEnumControl : obj[] * System.Type * EnumRepresentation -> 'TControl
    abstract BuildIntRangeControl : int * int -> 'TControl
    abstract BuildFloatRangeControl : float32 * float32 * float32 -> 'TControl
    abstract BuildBoolControl : unit -> 'TControl
    abstract BuildColorControl : unit -> 'TControl


/// Attribute used to give custom names to classes (specifically concrete Settings subclasses) to be used in UI etc.
type NameAttribute(name : string) =
    inherit System.Attribute()
    member this.Name = name


[<AbstractClass>]
type SettingAttribute() =
    inherit System.Attribute()

    member val Name = "" with get, set

    abstract member RandomValue : System.Random -> obj
    
    abstract member BuildUIControl : ISettingsUIBuilder<'TControl> -> 'TControl

and [<AbstractClass>] 
    Settings() = 
    member this.SettingProperties =
        let thisType = this.GetType()
        let rec distance (fromType : System.Type) (toType : System.Type) =
            if fromType = typeof<obj> then 999
            elif fromType = toType then 0
            else 1 + (distance fromType.BaseType toType)
        thisType.GetProperties()
        |> Array.filter (fun prop -> prop.CanRead && prop.CanWrite && prop.GetCustomAttributes(typeof<SettingAttribute>, true).Length > 0)
        |> Array.sortByDescending (fun prop -> distance thisType prop.DeclaringType)
    
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

    // Serialization
    static member val private NameToClassMap = System.Collections.Concurrent.ConcurrentDictionary<string, System.Type>()
    static member val private ClassToNameMap = System.Collections.Concurrent.ConcurrentDictionary<System.Type, string>()

    /// Map class to provided name.
    /// This static method should be called by all concrete subclasses.
    static member RegisterClass<'TClass when 'TClass :> Settings>(name : string) =
        Settings.NameToClassMap.[name] <- typeof<'TClass>
        Settings.ClassToNameMap.[typeof<'TClass>] <- name

    static member RegisterClass<'TClass when 'TClass :> Settings>() =
        Settings.RegisterClass<'TClass>(typeof<'TClass>.Name)

    static member GetClassForName(name : string) : System.Type =
        match Settings.NameToClassMap.TryGetValue(name) with
        | true, typ -> typ
        | false, _ -> failwithf "Type %s is not registered" name

    static member GetNameForClass(typ : System.Type) : string =
        match Settings.ClassToNameMap.TryGetValue(typ) with
        | true, name -> name
        | false, _ -> failwithf "Type %s is not registered" typ.Name



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



module Serialization =
    open System.Json

    // implicit conversion operator
    let inline (!>) (x:^a) : ^b = 
        ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)

    type IConverter =
        abstract member ToJsonValue : obj -> JsonValue
        abstract member FromJsonValue : JsonValue -> obj

    type Converter<'T> = | Converter of ('T ->  JsonValue) * (JsonValue -> 'T)
        with
            member this.ToJsonValue(object) = match this with | Converter(f, _) -> f object
            member this.FromJsonValue(value) = match this with | Converter(_, f) -> f value
            
            interface IConverter with
                member this.ToJsonValue(object) = this.ToJsonValue(object |> unbox<'T>)
                member this.FromJsonValue(value) = this.FromJsonValue(value) |> box
    
    module Converters =
        let converters = System.Collections.Concurrent.ConcurrentDictionary<System.Type, IConverter>()

        let inline makePrimitiveConverter< ^T when (JsonValue or  ^T) : (static member op_Implicit : JsonValue ->  ^T) and ( ^T or JsonValue) : (static member op_Implicit :  ^T -> JsonValue) >() : Converter< ^T > = 
            Converter( (fun (x : ^T) -> !> x) ,  (!>) )
    
        type RandomizeableConverterMaker<'T> =
            static member Make(valueConverter : IConverter) : Converter<Randomizeable<'T>> =
                let randomizedMarker = "<<<Randomized>>>"
                Converter(
                    (function | Randomized -> !> randomizedMarker | Defined(object) -> valueConverter.ToJsonValue(object)),
                    (fun value -> 
                        match value.JsonType with
                        | JsonType.String when !> value = randomizedMarker -> Randomized
                        | _ -> Defined(valueConverter.FromJsonValue(value) |> unbox<'T>) ) )
        
        let inline makeAndRegisterPrimitiveConverter< ^T when (JsonValue or  ^T) : (static member op_Implicit : JsonValue ->  ^T) and ( ^T or JsonValue) : (static member op_Implicit :  ^T -> JsonValue) >() =
            converters.[typeof< ^T >] <- makePrimitiveConverter< ^T >()
    
        do
            makeAndRegisterPrimitiveConverter<int>()
            makeAndRegisterPrimitiveConverter<float32>()
            makeAndRegisterPrimitiveConverter<bool>()
            makeAndRegisterPrimitiveConverter<string>()
            // Color
            converters.[typeof<float32 * float32 * float32>] <- 
                Converter(
                    (fun (r : float32, g : float32, b : float32) -> JsonArray([| r; g; b |] |> Array.map (!>)) :> JsonValue) ,
                    (fun value -> !> value.[0], !> value.[1], !> value.[2] ) )

        let getOrSetIfAbsent (typ : System.Type) (valueGenerator : unit -> IConverter) : IConverter =
            match converters.TryGetValue(typ) with
            | true, conv -> conv
            | false, _ ->
                let conv = valueGenerator()
                converters.[typ] <- conv
                conv

        let rec resolver (typ : System.Type) () : IConverter =
            if typ.IsSubclassOf(typeof<Settings>) then
                let typeMarker = "<<<Type>>>"
                let toJson (s : Settings) =
                    let result = JsonObject()
                    let typeName : JsonValue = !> Settings.GetNameForClass(typ)
                    result.[typeMarker] <- typeName
                    for prop in s.SettingProperties do
                        let propConverter = getOrSetIfAbsent prop.PropertyType (resolver prop.PropertyType)
                        result.[prop.Name] <- propConverter.ToJsonValue(prop.GetValue(s))
                    result :> JsonValue
                let fromJson (value : JsonValue) =
                    let typeName : string = !> value.[typeMarker]
                    let typ = Settings.GetClassForName(typeName)
                    let instance = typ.GetConstructor([||]).Invoke([||]) :?> Settings
                    for prop in instance.SettingProperties do
                        let jsonPropValue = value.[prop.Name]
                        let propConverter = getOrSetIfAbsent prop.PropertyType (resolver prop.PropertyType)
                        let propValue = propConverter.FromJsonValue jsonPropValue
                        prop.SetValue(instance, propValue)
                    instance
                Converter(toJson, fromJson) :> IConverter
            elif Randomizeable.isRandomizableType typ then
                let valueType = typ.GenericTypeArguments.[0]
                let valueConverter = getOrSetIfAbsent valueType (resolver valueType)
                let makerMethod = typedefof<RandomizeableConverterMaker<_>>.MakeGenericType([| valueType |]).GetMethod("Make")
                makerMethod.Invoke(null, [| valueConverter |]) :?> IConverter
            elif typ.IsEnum then
                // encode enum values as strings
                let toJson (value : obj) =
                    let name = typ.GetEnumName(value)
                    if isNull name then // illegal enum value
                        raise (System.Exception(sprintf "Illegal enum value: %A" value))
                    JsonPrimitive(name) :> JsonValue
                let fromJson (value : JsonValue) =
                    let name : string = !> value
                    System.Enum.Parse(typ, name)
                Converter(toJson, fromJson) :> IConverter
            else
                failwithf "Unsupported type: %s" typ.Name

    
    let toJsonValue(object : 'T) : JsonValue =
        let typ = object.GetType()
        let converter = Converters.getOrSetIfAbsent typ (Converters.resolver typ)
        converter.ToJsonValue(box object)

    let fromJsonValue<'T>(value : JsonValue) : 'T =
        let typ = typeof<'T>
        let converter = Converters.getOrSetIfAbsent typ (Converters.resolver typ)
        converter.FromJsonValue(value) |> unbox<'T>

    let copy<'T> : 'T -> 'T = toJsonValue >> fromJsonValue



// concrete attributes

type EnumAttribute(options: obj[]) = 
    inherit SettingAttribute()

    member val Representation = EnumRepresentation.Simple with get, set

    member val ItemType = options |> Seq.map (fun each -> each.GetType()) |> Seq.distinct |> Seq.exactlyOne with get, set

    override this.RandomValue(random) =
        options.[random.Next(options.Length)]

    override this.BuildUIControl(builder) = 
        builder.BuildEnumControl(options, this.ItemType, this.Representation)


/// Inclusive integer range
type IntRangeAttribute(minVal: int, maxVal: int) =
    inherit SettingAttribute()
    do assert(maxVal >= minVal)

    override this.RandomValue(random) =
        random.Next(minVal, maxVal+1) |> box

    override this.BuildUIControl(builder) = builder.BuildIntRangeControl(minVal, maxVal)

/// Float range (single precision)
type FloatRangeAttribute(minVal: float32, maxVal: float32) =
    inherit SettingAttribute()
    do assert(maxVal > minVal)
    let extent = maxVal - minVal

    member val Step = 0.0f with get, set
    
    member private this.NumSteps =
        if this.Step > 0.0f then
            int(extent / this.Step)
        else
            20

    override this.RandomValue(random) =
        if this.Step <= 0.0f then
            (float32(random.NextDouble()) * (maxVal - minVal) + minVal) |> box
        else
            minVal + float32(random.Next(this.NumSteps+1)) / float32(this.NumSteps) * extent |> box
    
    override this.BuildUIControl(builder) = builder.BuildFloatRangeControl(minVal, maxVal, this.Step)


/// Color (RGB float32 triple). Each component is in range [1; 0].
/// float32*float32*float32 is chosen to be independent of different color implementations
type ColorAttribute() =
    inherit SettingAttribute()

    override this.RandomValue(random) =
        let r = float32(random.NextDouble())
        let g = float32(random.NextDouble())
        let b = float32(random.NextDouble())
        (r, g, b) |> box

    override this.BuildUIControl(builder) = builder.BuildColorControl()



type BoolAttribute() =
    inherit SettingAttribute()

    override this.RandomValue(random) =
        (random.NextDouble() < 0.5)  |> box

    override this.BuildUIControl(builder) = builder.BuildBoolControl()

