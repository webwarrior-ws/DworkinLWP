namespace Settings

type private RandomizeableTypeAttribute() = 
    inherit System.Attribute()


[<RandomizeableType>]
[<DefaultAugmentation(false)>]
type Randomizeable<'T> =
    | Randomized
    | Defined of 'T
    with
        member this.IsRandomized =
            match this with
            | Randomized -> true
            | _ -> false

        static member IsRandomizableType (typ : System.Type) =
            typ.GetCustomAttributes(typeof<RandomizeableTypeAttribute>, true) |> Array.isEmpty |> not