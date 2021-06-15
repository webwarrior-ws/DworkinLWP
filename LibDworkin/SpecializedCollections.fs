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


let inline fastEquals<'a when 'a :> System.IEquatable<'a>> (x:'a) (y:'a) = x.Equals y


type SmallDictionary<'K, 'V when 'K : equality and 'K :> System.IEquatable<'K>> = struct
    val private list : ResizeArray<KeyValuePair<'K, 'V>>

    private new(initialCapacity : int) = { list = ResizeArray(initialCapacity) }

    static member Create<'K, 'V>(initialCapacity : int) : IDictionary<'K, 'V> = 
        SmallDictionary<'K, 'V>(initialCapacity) :> IDictionary<'K, 'V>
    
    static member Create<'K, 'V>() : IDictionary<'K, 'V> = SmallDictionary.Create(2)

    member inline private this.IndexOf(key : 'K) : int =
        let mutable i = 0
        let mutable found = false
        
        while not found && i < this.list.Count do
            if fastEquals this.list.[i].Key key then
                found <- true
            else
                i <- i + 1
        
        if found then i else -1
    
    member this.Exists(predicate : 'K -> 'V -> bool) : bool =
        let mutable i = 0
        let mutable found = false
        
        while not found && i < this.list.Count do
            let currElem = this.list.[i]
            if predicate currElem.Key currElem.Value then
                found <- true
            else
                i <- i + 1
        
        found

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


type NumberRange = struct
    val Min : float32
    val Max : float32
    
    new(min : float32, max: float32) =
        assert (min <= max)
        { Min=min; Max=max }
    
    new(arr : float32[]) =
        match arr with
        | [| a; b |] when a <= b -> NumberRange(a, b)
        | _ -> NumberRange(Array.min arr, Array.max arr)

    member this.Average =
        (this.Min + this.Max) * 0.5f
    
    member this.Extent =
        this.Max - this.Min
    
    member this.Contains(aNumber : float32) =
        aNumber >= this.Min && aNumber <= this.Max
    
    member this.By(step : float32) =
        if this.Min = this.Max then 
            Seq.singleton this.Min
        else
            seq { this.Min .. step .. this.Max }
    
    static member (*) (range : NumberRange, x : float32) =
        NumberRange(range.Min * x, range.Max * x)
end


/// Interval that can be compared and value of which can be accessed
[<CustomEquality; NoComparison>]
type Interval = struct
    val Start: float32
    val Stop: float32
    val Step: float32

    val private Sequence : seq<float32>

    new (start, stop, step) = { Start=start; Stop=stop; Step=step; Sequence=seq { start .. step .. stop } }

    override this.Equals(other) =
        match other with
        | :? Interval as otherInterval -> (this :> System.IEquatable<Interval>).Equals(otherInterval)
        | _ -> false
    
    override this.GetHashCode() =
        this.Start.GetHashCode() ^^^ this.Stop.GetHashCode() ^^^ this.Step.GetHashCode()
    
    interface IEnumerable<float32> with
        member this.GetEnumerator() : IEnumerator<float32> = 
            this.Sequence.GetEnumerator()
        member this.GetEnumerator() : System.Collections.IEnumerator = 
            this.Sequence.GetEnumerator() :> System.Collections.IEnumerator
    
    interface System.IEquatable<Interval> with
        member this.Equals(other : Interval) =
            this.Start = other.Start && this.Stop = other.Stop && this.Step = other.Step
end


/// Data structure that contains either on or two objects of certain type
[<Struct>]
type OneOrTwo<'T> =
    | One of one: 'T
    | Two of two: struct ('T * 'T)


module OneOrTwo =
    let inline map (mapper : 'T -> 'U) (container : OneOrTwo<'T>) =
        match container with
        | One(x) -> One(mapper x)
        | Two(struct(x, y)) -> Two(struct(mapper x, mapper y))
    
    let inline exists2 (predicate : 'T1 ->'T2 -> bool) (source1 : OneOrTwo<'T1>) (source2 : OneOrTwo<'T2>) : bool =
        match struct(source1, source2) with
        | struct(One(x), One(y)) -> predicate x y
        | struct(Two(struct(x1, x2)), Two(struct(y1, y2))) -> predicate x1 y1 || predicate x2 y2
        | _ -> failwith "Size mismatch"


// Backport of ValueOption for F# 4.0 (used in Xamarin Android)
#if __ANDROID__
#nowarn "42"
#nowarn "62"

let inline private anyToString nullStr x = 
    match box x with 
    | null -> nullStr
    | :? System.IFormattable as f -> f.ToString(null,System.Globalization.CultureInfo.InvariantCulture)
    | obj ->  obj.ToString()

let private anyToStringShowingNull x = anyToString "null" x

[<StructuralEquality; StructuralComparison>]
[<Struct>]
[<CompiledName("FSharpValueOption`1")>]
[<System.Diagnostics.DebuggerDisplay("ValueSome({Value})")>]
type ValueOption<'T> =
    | ValueNone : 'T voption
    | ValueSome : 'T -> 'T voption

    member x.Value = match x with ValueSome x -> x | ValueNone -> raise (new System.InvalidOperationException("ValueOption.Value"))

    [<System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)>]
    static member None : 'T voption = ValueNone

    static member Some (value) : 'T voption = ValueSome(value)

    [<System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)>]
    member x.IsNone = match x with ValueNone -> true | _ -> false

    [<System.Diagnostics.DebuggerBrowsable(System.Diagnostics.DebuggerBrowsableState.Never)>]
    member x.IsSome = match x with ValueSome _ -> true | _ -> false

    static member op_Implicit (value) : 'T option = Some(value)

    override x.ToString() = 
        // x is non-null, hence ValueSome
        "ValueSome("^anyToStringShowingNull x.Value^")"

and 'T voption = ValueOption<'T>
#endif