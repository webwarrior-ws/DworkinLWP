namespace Settings

type private RandomizeableTypeAttribute() = 
    inherit System.Attribute()


[<RandomizeableType>]
type Randomizeable<'T> =
    | Randomized
    | Defined of 'T
    with
        static member IsRandomizableType (typ : System.Type) =
            typ.GetCustomAttributes(typeof<RandomizeableTypeAttribute>, true) |> Array.isEmpty |> not