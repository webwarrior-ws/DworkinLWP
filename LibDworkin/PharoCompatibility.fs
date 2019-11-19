/// Extensions and functions that mimic behaviour of some Pharo methods
module LibDworkin.PharoCompatibility

open System.Collections.Generic
open Geometry


// extensions to match some Smalltalk dictionary methods
type IDictionary<'K, 'V> with
    /// Equivaluent of Smalltalk's #at:ifAbsentPut:
    member this.GetOrSetIfAbsent (key : 'K, value : unit -> 'V) : 'V =
        match this.TryGetValue(key) with
        | (true, v) -> v
        | (false, _) -> 
            let computedValue = value()
            this.[key] <- computedValue
            computedValue
    
    /// Equivaluent of Smalltalk's #at:ifPresent:ifAbsentPut:
    member this.GetTransformedOrSetIfAbsent (key : 'K, transform: 'V -> 'V, value : unit -> 'V) : 'V =
        match this.TryGetValue(key) with
        | (true, v) -> transform v
        | (false, _) -> 
            let computedValue = value()
            this.[key] <- computedValue
            computedValue
    
    /// Equivaluent of Smalltalk's #at:ifAbsent:
    member this.Get(key : 'K, value : unit -> 'V) : 'V = 
        match this.TryGetValue(key) with
        | (true, v) -> v
        | (false, _) -> value()
    
    /// Equivaluent of Smalltalk's #at:ifPresent:ifAbsent:
    member this.GetTransformed(key : 'K, transform: 'V -> 'X, value : unit -> 'X) : 'X = 
        match this.TryGetValue(key) with
        | (true, v) -> transform v
        | (false, _) -> value()
    
    /// Equivaluent of Smalltalk's #at:ifPresent:
    member this.GetTransformed(key : 'K, transform: 'V -> 'X) : 'X = 
        this.GetTransformed(key, transform, fun () -> raise (KeyNotFoundException(key.ToString())) )


/// inline version of IDictionary.GetOrSetIfAbsent()
let inline getOrSetIfAbsent (dict : System.Collections.Generic.IDictionary<'K, 'V>) (key : 'K) (value : unit -> 'V) =
    match dict.TryGetValue(key) with
    | (true, v) -> v
    | (false, _) -> 
        let computedValue = value()
        dict.[key] <- computedValue
        computedValue


/// Equivalent of Smalltalk's #ifEmpty:ifNotEmpty:
let (|Empty|NotEmpty|) (sequence : #seq<'T>) =
    if Seq.isEmpty sequence then
        Empty 
    else
        NotEmpty(sequence)


// uniform access to size of collections
type ICollection<'T> with
    member inline this.Length = this.Count


// extensions to match some Smalltalk's SequenceableCollection methods
type IList<'T> with
    member inline this.First = this.[0]
    member inline this.Second = this.[1]
    member inline this.Third = this.[2]
    member inline this.Fourth = this.[3]
    member inline this.Fifth = this.[4]

    member inline this.Last = this.[this.Count-1]

    // note: uses 1-based indices
    member this.AddAfterIndex(object : 'T, index : int) =
        this.Insert(index, object)
        
    // note: uses 1-based indices
    member this.AddBeforeIndex(object : 'T, index : int) =
        this.Insert(index-1, object)
    
    member this.RemoveLast() : 'T =
        let result = this.Last
        this.RemoveAt(this.Length-1)
        result

    member this.AtRandom(prng : System.Random) =
        this.[prng.Next(this.Count)]
        
    member this.ShuffleBy(random : System.Random) : unit =
        for i in this.Length-1 .. -1 .. 1 do
            // swap elemets at i and random.Next(i)
            let i2 = random.Next(i)
            let e = this.[i]
            let e2 = this.[i2]
            this.[i] <- e2
            this.[i2] <- e


type 'T ``[]`` with
    member this.AtRandom(prng : System.Random) =
        this.[prng.Next(this.Length)]
    
    /// take n last elemets of an array
    member this.TakeLast(n : int) : 'T[] =
        this.[this.Length-n..this.Length-1]
    
    member this.ShuffleBy(random : System.Random) : unit =
        for i in this.Length-1 .. -1 .. 1 do
            // swap elemets at i and random.Next(i)
            let i2 = random.Next(i)
            let e = this.[i]
            let e2 = this.[i2]
            this.[i] <- e2
            this.[i2] <- e

type System.Random with
    member inline this.NextFloat() =
        this.NextDouble() |> float32

    member this.NextInRange(a : float32, b : float32) =
        lerp a b (this.NextFloat())
    
    member this.NextInRange(arr : float32[]) =
        match arr with
        | [| a; b |] -> this.NextInRange(a, b)
        | _ -> this.NextInRange(Array.min arr, Array.max arr)
    
    member this.NextInRange(range : SpecializedCollections.NumberRange) =
        this.NextInRange(range.Min, range.Max)
    
    member this.NextGaussian() =
        let mutable w = 1.0f
        let mutable x1 = 0.0f
        while w >= 1.0f do
            x1 <- (2.0f * this.NextFloat()) - 1.0f
            let x2 = (2.0f * this.NextFloat()) - 1.0f
            w <- (x1 * x1) + (x2 * x2)
        w <- sqrt ((-2.0f * log w) / w)
        x1 * w
    
    member this.NextGaussian(mu : float32, sigma : float32) =
         this.NextGaussian() * sigma + mu
        
    member this.NextInRectangle(aRectangle : Rectangle) =
        Point(lerp aRectangle.Left aRectangle.Right (this.NextFloat()), lerp aRectangle.Top aRectangle.Bottom (this.NextFloat()))



/// tests if x lies between a and b
let inline between a b x =
    x >= a && x <= b

/// Square of x
let inline squared x = x*x 

/// Answer the nearest number that is a multiple of quantum
let inline roundTo quantum number =
    round (number / quantum) * quantum

/// Equivalent to Smalltalk's #withIndexDo:
let withIndexDo func sequence =
    // in Smalltalk, indices are 1-based, so add 1 to i
    Seq.iteri (fun i x -> func x (i+1)) sequence

/// Power curve function from http://iquilezles.org/www/articles/functions/functions.htm
let pcurve x a b =
    let k = (a+b) ** (a+b) / (a**a * b**b)
    k * x ** a  * (1.0f-x) ** b

let inline isOdd n = n &&& 1 = 1

let inline isEven n = n &&& 1 = 0

/// Equivalent to Smalltalk's \\ operator (and different from % when dealing with negative numbers)
let inline (%%) a b =
    let result = a % b
    if sign b = sign result then
        result
    else
        result + b