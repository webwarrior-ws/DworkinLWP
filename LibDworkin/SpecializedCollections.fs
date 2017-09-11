module SpecializedCollections

open System.Collections.Generic


/// Low-overhead dynamic array that provides direct access to undelying array containing actual data.
/// Store in mutable variables! Always pass initialCapacity to constructor!
type Buffer<'T> = struct
    val mutable Array : 'T[] 
    val mutable Size : int 
    
    new(initialCapacity : int) =
        { Array = Array.zeroCreate initialCapacity; Size = 0 }
        
    member this.Capacity = this.Array.Length

    member this.Add(item : 'T) =
        if this.Size >= this.Array.Length then
            System.Array.Resize(&this.Array, this.Array.Length * 2)
        this.Array.[this.Size] <- item
        this.Size <- this.Size + 1
    
    member this.Reset() =
        this.Size <- 0
    
    member this.RemoveAll(predicate : 'T -> bool) =
        let mutable n = 0
        for i=0 to this.Size-1 do
            if not (predicate this.Array.[i]) then
                this.Array.[n] <- this.Array.[i]
                n <- n + 1
        this.Size <- n
    
    /// reset and fill values with defaults so that old values can be garbage collected
    member this.Clear() =
        Array.fill this.Array 0 this.Size Unchecked.defaultof<'T>
        this.Size <- 0
end


type SmallDictionary<'K, 'V when 'K : equality> = ResizeArray<KeyValuePair<'K, 'V>>

module SmallDictionary =
    let create() = SmallDictionary(2)

    let add (dict : SmallDictionary<'K, 'V>) (item : KeyValuePair<'K, 'V>) =
        dict.Add item
    
    let get (dict : SmallDictionary<'K, 'V>) (key : 'K) =
        match dict.FindIndex(fun x -> x.Key = key) with
        | i when i >= 0 -> Some(dict.[i].Value)
        | _ -> None

    let set (dict : SmallDictionary<'K, 'V>) (key : 'K) (value : 'V) =
        match dict.FindIndex(fun x -> x.Key = key) with
        | i when i >= 0 -> dict.[i] <- KeyValuePair(key, value)
        | _ -> dict.Add(KeyValuePair(key, value))
    
    /// like at:ifAbsentPut: in Smalltalk
    let getOrSetIfAbsent (dict : SmallDictionary<'K, 'V>) (key : 'K) (value : unit -> 'V) : 'V =
        match get dict key with
        | Some(value) -> value
        | None -> 
            let computedValue = value()
            dict.Add(KeyValuePair(key, computedValue))
            computedValue


type LayersList<'T when 'T : equality>(items : 'T[], isComplement : bool) = struct
    member this.Contains(item : 'T) = 
        (items |> Array.contains item) <> isComplement
end
