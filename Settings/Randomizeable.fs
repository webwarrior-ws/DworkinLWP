module Settings.Randomizeable


type Randomizeable<'T> =
    | Randomized
    | Defined of 'T

let isRandomizableType (typ : System.Type) =
    Microsoft.FSharp.Reflection.FSharpType.IsUnion typ && 
    (Microsoft.FSharp.Reflection.FSharpType.GetUnionCases typ |> Array.map (fun x -> x.Name) = [| "Random"; "Defined" |])
