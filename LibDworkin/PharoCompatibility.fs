/// Extensions and functions that mimic behaviour of some Pharo methods
module PharoCompatibility

open System.Collections.Generic


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


/// Equivalent of Smalltalk's #ifEmpty:ifNotEmpty:
let (|Empty|NotEmpty|) (sequence : #seq<'T>) =
    if Seq.isEmpty sequence then
        Empty 
    else
        NotEmpty(sequence)


// unified access to size of collections
type ICollection<'T> with
    member inline this.Length = this.Count


/// tests if x lies between a and b
let inline between a b x =
    x >= a && x <= b

/// Square of x
let inline squared x = x*x 