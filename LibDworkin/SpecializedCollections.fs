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


type SmallDictionary<'K, 'V when 'K : equality> = struct
    val private list : ResizeArray<KeyValuePair<'K, 'V>>

    private new(initialCapacity : int) = { list = ResizeArray(initialCapacity) }

    static member Create<'K, 'V>(initialCapacity : int) : IDictionary<'K, 'V> = 
        SmallDictionary<'K, 'V>(initialCapacity) :> IDictionary<'K, 'V>
    
    static member Create<'K, 'V>() : IDictionary<'K, 'V> = SmallDictionary.Create(2)

    interface IDictionary<'K, 'V> with
        member this.Add(item: KeyValuePair<'K,'V>): unit = 
            this.list.Add item
        member this.Add(key, value) = 
            (this :> IDictionary<'K, 'V>).Add(KeyValuePair<'K, 'V>(key, value))
        
        member this.Clear(): unit = this.list.Clear()
        
        member this.Contains(item: KeyValuePair<'K,'V>): bool = 
            this.list.Contains item
        
        member this.ContainsKey(key: 'K): bool = 
            match this.list.FindIndex(fun x -> x.Key = key) with
            | i when i >= 0 -> true
            | _ -> false
        
        member this.CopyTo(array: KeyValuePair<'K,'V> [], arrayIndex: int): unit = 
            raise (System.NotImplementedException())
        
        member this.Count: int = 
            this.list.Count
        
        member this.GetEnumerator(): IEnumerator<KeyValuePair<'K,'V>> = 
            this.list.GetEnumerator() :> IEnumerator<KeyValuePair<'K,'V>>
        
        member this.GetEnumerator(): System.Collections.IEnumerator = 
            this.list.GetEnumerator() :> System.Collections.IEnumerator
        
        member this.IsReadOnly: bool = false
        
        member this.Item
            with get (key: 'K): 'V = 
                match this.list.FindIndex(fun x -> x.Key = key) with
                | i when i >= 0 -> this.list.[i].Value
                | _ -> raise (KeyNotFoundException(key.ToString()))
            and set (key: 'K) (v: 'V): unit = 
                match this.list.FindIndex(fun x -> x.Key = key) with
                | i when i >= 0 -> this.list.[i] <- KeyValuePair(key, v)
                | _ -> raise (KeyNotFoundException(key.ToString()))
        
        member this.Keys: ICollection<'K> = 
            this.list.ConvertAll(fun item -> item.Key) :> ICollection<'K>
        
        member this.Remove(key: 'K): bool = 
            match this.list.FindIndex(fun x -> x.Key = key) with
            | i when i >= 0 -> 
                this.list.RemoveAt i
                true
            | _ -> false
        
        member this.Remove(item: KeyValuePair<'K,'V>): bool = 
            this.list.Remove item
        
        member this.TryGetValue(key: 'K, value: byref<'V>): bool = 
            match this.list.FindIndex(fun x -> x.Key = key) with
            | i when i >= 0 -> 
                value <- this.list.[i].Value
                true
            | _ -> false
        
        member this.Values: ICollection<'V> = 
            this.list.ConvertAll(fun item -> item.Value) :> ICollection<'V>
end
    

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


type LayersList<'T when 'T : equality>(items : 'T[], isComplement : bool) = struct
    member this.Contains(item : 'T) = 
        (items |> Array.contains item) <> isComplement
end
