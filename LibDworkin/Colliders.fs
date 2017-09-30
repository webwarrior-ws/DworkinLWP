module LibDworkin.Colliders


open Geometry
open AgentAbstractions

open OpenTK



type CircleCollider(parent: Agent option, center: Point, radius: float32) =
    inherit Collider(parent)

    let mutable center = center
    let mutable radius = radius

    member this.Center 
        with get() : Point = center
        and set(p : Point) =
            center <- p
            this.FlushCache()
    
    member this.Radius
        with get() : float32 = radius
        and set(num: float32) =
            radius <- num
            this.FlushCache()
    
    override this.CalculateBoundingBox() =
        Rectangle(center.X - radius, center.Y - radius, center.X + radius, center.Y + radius)

 
type CircleSectorCollider(parent: Agent option, center: Point, radius: float32, bearing: float32, centralAngle: float32) =
    inherit CircleCollider(parent, center, radius)

    static let rightAngles = [| 0.0f; 1.5707963267948966f; 3.141592653589793f; 4.71238898038469f |]

    let mutable bearing = bearing
    let mutable centralAngle = centralAngle
    let mutable r1 : Point option = None
    let mutable r2 : Point option = None

    member this.Bearing
        with get() : float32 = bearing
        and set(num: float32) = 
            bearing <- num
            this.FlushCache()
    
    member this.CentralAngle
        with get() : float32 = centralAngle
        and set(num: float32) =
            if num > Single.TwoPi || num < 0.0f then
                failwith "Expected number between 0 and 2*pi"
            centralAngle <- num
            this.FlushCache()
    
    member this.R1 = 
        match r1 with
        | Some(p) -> p
        | None ->
            let theta = bearing - centralAngle
            let p = center + Point(radius * cos theta, radius * sin theta)
            r1 <- Some(p)
            p
    
    member this.R2 = 
        match r2 with
        | Some(p) -> p
        | None ->
            let theta = bearing + centralAngle
            let p = center + Point(radius * cos theta, radius * sin theta)
            r2 <- Some(p)
            p

    member this.SetParameters(center: Point, radius: float32, bearing: float32) =
        if bearing = this.Bearing && radius = this.Radius then
            let shift = center - this.Center
            this.Center <- center
            if not this.BoundingBox.IsEmpty then
                this.BoundingBox <- this.BoundingBox.TranslateBy(shift)
            r1 <- r1 |> Option.map ((+) shift)
            r2 <- r2 |> Option.map ((+) shift)
        else
            this.Center <- center
            this.Radius <- radius
            this.Bearing <- bearing
            this.FlushCache()
     
    override this.FlushCache() =
        base.FlushCache();
        r1 <- None
        r2 <- None
         
    override this.CalculateBoundingBox() =
        let points = ResizeArray<Point>(5)
        let intersections =  [| 
            Point(center.X + radius, center.Y); Point(center.X, center.Y + radius);
            Point(center.X - radius, center.Y); Point(center.X, center.Y - radius) |]
        
        rightAngles |> (Array.iteri (fun i angle -> 
            let mutable alpha = angle - bearing + (centralAngle * 0.5f)
            if alpha < 0.0f then
                alpha <- alpha + Single.TwoPi
            elif alpha > Single.TwoPi then
                alpha <- alpha - Single.TwoPi
            if alpha >= 0.0f && alpha <= centralAngle then
                points.Add intersections.[i]))
        
        if points.Count <= 1 then points.Add center
        if points.Count < 4 then 
            points.Add this.R1
            points.Add this.R2
        
        Rectangle.Encompassing(points)


type SegmentCollider(parent: Agent option, p1: Point, p2: Point) =
    inherit Collider(parent)

    member val Start = p1
    member val End = p2

    override this.CalculateBoundingBox() =
        Rectangle(min p1.X p2.X, min p1.Y p2.Y, max p1.X p2.X, max p1.Y p2.Y)


// collision detection functions
[<AutoOpen>]
module CollisionTests =
    let inline circleCircle (o1 : Point) (r1 : float32) (o2 : Point) (r2 : float32) =
        let rsum = r1+r2
        o1.SquaredDistanceTo(o2) <= rsum*rsum

    let circleSegment (o1 : Point) (r1 : float32) (p1 : Point) (p2 : Point) =
        o1.NearestPointOnLineFromTo(p1, p2).SquaredDistanceTo(o1) <= (r1 * r1)

    let segmentSegment (start1 : Point) (end1 : Point) (start2 : Point) (end2 : Point) =
        if (start1 = start2) || (end1 = end2) || (start1 = end2) || (start2 = end1) then
            true
        else
            let inline sideOf (p0 : Point) (p1 : Point) (p2 : Point) =
                sign ((p1-p0).Cross(p2-p0))
            let sideStart = sideOf start1 end1 start2
            let sideEnd = sideOf start1 end1 end2
            if sideStart = sideEnd then
                false
            else
                let sideStart = sideOf start2 end2 start1
                let sideEnd = sideOf start2 end2 end1
                sideStart <> sideEnd
    
    let circlesectorCircle (sector : CircleSectorCollider) (center : Point) (radius : float32) =
        if circleCircle sector.Center sector.Radius center radius then
            let gamma = ((center - sector.Center).Theta - sector.Bearing + (sector.CentralAngle * 0.5f)) % Single.TwoPi
            (gamma >= 0.0f && gamma <= sector.CentralAngle) 
            || (circleSegment center radius sector.Center sector.R1) 
            || (circleSegment center radius sector.Center sector.R2)
        else false
    
    let circlesectorSegment (sector : CircleSectorCollider) (p1 : Point) (p2 : Point) =
        let a = p1 - sector.Center
        let b = p2 - sector.Center
        let p = Point(0.0f, 0.0f).NearestPointOnLineFromTo(a, b)
        if Point.Dot(p, p) > (sector.Radius * sector.Radius) then
            false
        else
            let angle = ((p.Theta - sector.Bearing) + (sector.CentralAngle / 2.0f)) % Single.TwoPi
            (angle >= 0.0f && angle <= sector.CentralAngle) ||
            (segmentSegment sector.Center sector.R1 p1 p2) || 
            (segmentSegment sector.Center sector.R2 p1 p2)


/// Check two colliders for collision
let collide (first : Collider) (second : Collider) : bool =
    match first, second with
    // most used first
    | :? CircleSectorCollider as a, (:? CircleCollider as b) -> circlesectorCircle a b.Center b.Radius
    | :? CircleSectorCollider as a, (:? SegmentCollider as b) -> circlesectorSegment a b.Start b.End
    | :? CircleCollider as a, (:? CircleSectorCollider as b) -> circlesectorCircle b a.Center a.Radius
    | :? CircleCollider as a, (:? CircleCollider as b) -> circleCircle a.Center a.Radius b.Center b.Radius
    // then rest
    | :? SegmentCollider as a, (:? CircleSectorCollider as b) -> circlesectorSegment b a.Start a.End
    | :? CircleCollider as a, (:? SegmentCollider as b) -> circleSegment a.Center a.Radius b.Start b.End
    | :? SegmentCollider as a, (:? CircleCollider as b) -> circleSegment b.Center b.Radius a.Start a.End
    | :? SegmentCollider as a, (:? SegmentCollider as b) -> segmentSegment a.Start a.End b.Start b.End
    | _ -> failwith "notimplemented"
