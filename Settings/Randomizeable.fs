namespace Settings


type Randomizeable<'T> =
    | Randomized
    | Defined of 'T
    with
        static member IsRandomizableType (typ : System.Type) =
            let getCases = Microsoft.FSharp.Reflection.FSharpType.GetUnionCases
            let getCaseNames = getCases >> Array.map (fun x -> x.Name)
            Microsoft.FSharp.Reflection.FSharpType.IsUnion typ && (getCaseNames typ = (getCaseNames typeof<Randomizeable<_>>))