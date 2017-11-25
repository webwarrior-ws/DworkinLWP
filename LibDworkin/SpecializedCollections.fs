module LibDworkin.SpecializedCollections

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

    member inline private this.IndexOf(key : 'K) : int =
        let mutable i = 0
        let mutable found = false
        
        while not found && i < this.list.Count do
            if this.list.[i].Key = key then
                found <- true
            else
                i <- i + 1
        
        if found then i else -1

    interface IDictionary<'K, 'V> with
        member this.Add(item: KeyValuePair<'K,'V>): unit = 
            this.list.Add item
        member this.Add(key, value) = 
            (this :> IDictionary<'K, 'V>).Add(KeyValuePair<'K, 'V>(key, value))
        
        member this.Clear(): unit = this.list.Clear()
        
        member this.Contains(item: KeyValuePair<'K,'V>): bool = 
            this.list.Contains item
        
        member this.ContainsKey(key: 'K): bool = 
            match this.IndexOf(key) with
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
                match this.IndexOf(key) with
                | i when i >= 0 -> this.list.[i].Value
                | _ -> raise (KeyNotFoundException(key.ToString()))
            and set (key: 'K) (v: 'V): unit = 
                match this.IndexOf(key) with
                | i when i >= 0 -> this.list.[i] <- KeyValuePair(key, v)
                | _ -> this.list.Add(KeyValuePair(key, v))
        
        member this.Keys: ICollection<'K> = 
            this.list.ConvertAll(fun item -> item.Key) :> ICollection<'K>
        
        member this.Remove(key: 'K): bool = 
            match this.IndexOf(key) with
            | i when i >= 0 -> 
                this.list.RemoveAt i
                true
            | _ -> false
        
        member this.Remove(item: KeyValuePair<'K,'V>): bool = 
            this.list.Remove item
        
        member this.TryGetValue(key: 'K, value: byref<'V>): bool = 
            match this.IndexOf(key) with
            | i when i >= 0 -> 
                value <- this.list.[i].Value
                true
            | _ -> false
        
        member this.Values: ICollection<'V> = 
            this.list.ConvertAll(fun item -> item.Value) :> ICollection<'V>
end


type LayersList<'T when 'T : equality>(items : 'T[], isComplement : bool) = struct
    member this.Contains(item : 'T) = 
        (items |> Array.contains item) <> isComplement
end


type NumberRange = struct
    val Min : float32
    val Max : float32
    
    new(min : float32, max: float32) =
        assert (min < max)
        { Min=min; Max=max }
    
    new(arr : float32[]) =
        match arr with
        | [| a; b |] -> NumberRange(a, b)
        | _ -> NumberRange(Array.min arr, Array.max arr)

    member this.Average =
        (this.Min + this.Max) * 0.5f
end