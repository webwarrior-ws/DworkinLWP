module LibDworkin.SpatialIndex

open Geometry


type IHasBounds = interface
    abstract member Bounds : Rectangle
end


type SpatialIndex<'T when 'T :> IHasBounds> = interface
    abstract member Insert: 'T -> unit
    abstract member Query: Rectangle * ('T -> bool) -> bool
    abstract member Iterate: Rectangle * ('T -> unit) -> unit
    abstract member TotalItems: int
end


type AABB(rect : Rectangle) = struct
    interface IHasBounds with 
        member this.Bounds = rect
    member this.Bounds = rect
end


type OpenTK.Box2 with
    member this.BoundingBox =
       AABB(this)


exception OutOfBounds of objBounds:Rectangle * treeBounds:Rectangle


type QuadTree<'T when 'T :> IHasBounds>(bounds: Rectangle, maxObjects: int) =
    let mutable objects : 'T[] = Array.zeroCreate(min maxObjects 4)
    let mutable objectBounds : Rectangle[] = Array.zeroCreate(min maxObjects 4)
    let mutable objectsCount = 0
    let mutable allObjectsBounds : Rectangle = Rectangle()
    let mutable nodes : QuadTree<'T>[] option = None
    let center = bounds.Center

    interface SpatialIndex<'T> with
        member this.Insert(obj) = this.Insert(obj)
        member this.Query(rectangle, predicate) = this.Query(rectangle, predicate)
        member this.Iterate(rectangle, action) = this.Iterate(rectangle, action)
        member this.TotalItems = this.TotalItems

    member this.Insert(obj : 'T) =
        let objBounds = obj.Bounds
          
        if not (bounds.Contains objBounds) then
            raise (OutOfBounds(objBounds, bounds))

        this.Insert(obj, objBounds)
    
    // proceed only if predicate returns true
    member this.Query(rectangle: Rectangle, predicate: 'T -> bool) : bool =
        let rec processItems i proceed =
            if not proceed then false
            elif i < objectsCount then
                let candidate = &objectBounds.[i]
                if rectangle.IntersectsWith &candidate then
                    processItems (i + 1) (predicate objects.[i])
                else 
                    processItems (i + 1) true
             else true
        
        (objectsCount < 1 || (allObjectsBounds.IntersectsWith rectangle |> not) || processItems 0 true) && 
            match nodes with
            | Some(treeNodes) ->
                match center.QuadrantOf(rectangle.Left, rectangle.Top), 
                        center.QuadrantOf(rectangle.Right, rectangle.Bottom) with
                | q1, q2 when q1 = q2 -> 
                    treeNodes.[q1].Query(rectangle, predicate)
                | 2,  0 -> 
                    treeNodes |> Array.forall (fun node -> node.Query(rectangle, predicate))
                | q1, q2 -> 
                    treeNodes.[q1].Query(rectangle, predicate) && treeNodes.[q2].Query(rectangle, predicate)
            | None -> false

    
    member this.Iterate(rectangle: Rectangle, action: 'T -> unit) : unit =
        this.Query(rectangle, fun x -> action x; true) |> ignore
    
    /// return true if object satisfying given predicate exists
    member this.Exists(rectangle: Rectangle, predicate: 'T -> bool) : bool =
        let mutable res = false
        this.Query(rectangle, fun x -> res <- predicate x; not res) |> ignore
        res

    /// Determine which node the object belongs to. 
    /// -1 means object cannot completely fit within a child node and is part of the parent node.
    member private this.GetIndex(rect : Rectangle) : int =
        let q1 = center.QuadrantOf(rect.Left, rect.Top)
        let q2 = center.QuadrantOf(rect.Right, rect.Bottom)
        if q1 = q2 then q1 else -1

    /// Get array of 4 quadrants, which form current QuadTree bounds.
    /// Order is clockwise: 
    /// 0 is downRight, 1=downLeft, 2=upLeft, 3=upRight
    member private this.Quadrants : Rectangle[] = 
        [|
            Rectangle(center, Point(bounds.Right, bounds.Bottom));
            Rectangle(Point(bounds.Left, center.Y), Point(center.X, bounds.Bottom));
            Rectangle(bounds.Origin, center);
            Rectangle(Point(center.X, bounds.Top), Point(bounds.Right, center.Y));
        |]
    
    member private this.Insert(obj : 'T, objBounds: Rectangle) =
        if nodes.IsNone && objectsCount >= maxObjects then 
            this.Split()

        let addToThis = 
            match nodes with
            | Some(treeNodes) -> 
                match this.GetIndex objBounds with
                | index when index >= 0 -> 
                    treeNodes.[index].Insert(obj, objBounds)
                    false
                | _ -> true
            | None -> true

        if addToThis then
            if objectsCount >= objects.Length then
                System.Array.Resize(&objects, objects.Length * 2)
                System.Array.Resize(&objectBounds, objectBounds.Length * 2)
            objects.[objectsCount] <- obj
            objectBounds.[objectsCount] <- objBounds
            objectsCount <- objectsCount + 1

            allObjectsBounds <- 
                match objectsCount with 
                | 1 -> objBounds
                | _ -> allObjectsBounds.Merge objBounds

    member private this.Split() =
        let treeNodes = this.Quadrants |> Array.map (fun quadrant -> QuadTree(quadrant, maxObjects))
        
        // move objects [that fit in quadrants] to children nodes
        let mutable n = 0
        for i=0 to objectsCount-1 do
            let objBounds = objectBounds.[i]
            let delete = 
                match this.GetIndex objBounds with
                | index when index >= 0 -> 
                    treeNodes.[index].Insert(objects.[i], objBounds)
                    true
                | _ -> false
            if not delete then
                objects.[n] <- objects.[i]
                objectBounds.[n] <- objectBounds.[i]
                n <- n + 1
        objectsCount <- n

        nodes <- Some(treeNodes)
        
        if objectsCount > 0 then
            allObjectsBounds <- objectBounds.[0] 
            for i=1 to objectsCount-1 do
                allObjectsBounds <- allObjectsBounds.Merge objectBounds.[i]
    
    member this.TotalItems =
        objectsCount + (match nodes with | Some(trees) -> trees |> Array.sumBy (fun x -> x.TotalItems) | None -> 0 )
    
    member this.Depth =
        1 + (match nodes with | Some(trees) -> trees |> Array.map (fun x -> x.Depth) |> Array.max | None -> 0 )


// TODO: move to tests project
module Tests =
    let testQuerying() =
        let bounds = Rectangle(0.0f, 0.0f, 200.0f, 200.0f)
        let inst : SpatialIndex<AABB> = QuadTree(bounds, 20) :> SpatialIndex<AABB>

        inst.Iterate(bounds, fun _ -> failwith "")

        let rect = Rectangle(10.0f, 10.0f, 30.0f, 30.0f).BoundingBox
        
        inst.Insert(rect)

        inst.Iterate(Rectangle(0.0f, 0.0f, 5.0f, 25.0f), fun _ -> failwith "")
        inst.Iterate(Rectangle(0.0f, 0.0f, 5.0f, 25.0f), fun x -> if not (x = rect) then failwith "")

        inst.Insert(Rectangle(100.0f, 100.0f, 120.0f, 120.0f).BoundingBox)
        inst.Insert(Rectangle(120.0f, 100.0f, 140.0f, 120.0f).BoundingBox)
        inst.Insert(Rectangle(100.0f, 120.0f, 120.0f, 140.0f).BoundingBox)

        inst.Iterate(Rectangle(125.0f, 125.0f, 130.0f, 130.0f), fun _ -> failwith "")

        let rect = Rectangle(100.0f, 100.0f, 125.0f, 125.0f)
        let mutable count = 0
        inst.Iterate(rect, fun x -> assert(rect.IntersectsWith((x :> IHasBounds).Bounds)); count <- count + 1)
        if not (count = 3) then failwithf "%d <> 3" count
        if not (inst.TotalItems = 4) then failwithf "%d <> 4" inst.TotalItems

    let n = 150
    let gap = 20

    let rects_and_regions = lazy(
        let rand = System.Random(42)
        let shifts = Array.init n (fun i -> Point(float32(rand.Next(gap) / 2), float32(rand.Next(gap) / 2)))
        let rects = Array.init (n*n) (fun i ->
            let origin = Point(float32(i/n * gap), float32(i%n * gap)) + shifts.[i % shifts.Length]
            let corner = origin + Point(float32(gap - 2), float32(gap - 2))
            Rectangle(origin, corner).BoundingBox)
        
        let regions = 
            Array.init (n*10) (fun i ->
                let origin = Point(float32(rand.Next((n-2) * gap)), float32(rand.Next((n-2) * gap)))
                let corner = origin + Point(float32(rand.Next(gap*2)), float32(rand.Next(gap*2)))
                Rectangle(origin, corner))
        
        (rects, regions))
    
    let testPerformance() =
        let rects, regions = rects_and_regions.Value
        let inst : SpatialIndex<AABB> = QuadTree(Rectangle(0.0f, 0.0f, float32((n+2)*gap), float32((n+2)*gap)), 16) :> SpatialIndex<AABB>

        let mutable regIndex = 0

        for rect in rects do
            inst.Insert rect

            for i=1 to 6 do
                regIndex <- (regIndex + 1) % regions.Length
                let region = regions.[regIndex]
                inst.Query(region, fun _ -> false) |> ignore
        
        if not (inst.TotalItems = rects.Length) then failwithf "%d <> %d" inst.TotalItems rects.Length