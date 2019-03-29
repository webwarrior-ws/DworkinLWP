module DworkinTestingApp.TestRenderers

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.ES20
open Javax.Microedition.Khronos

open LibDworkin.Geometry
open LibDworkin.Renderer
open LibDworkin.RendererComponents
open LibDworkin.AgentAbstractions
open LibDworkin.SpatialIndex
open LibDworkin.Colliders
open LibDworkin.SpecializedCollections



type ShapesTestRenderer(context) =
    inherit BaseRenderer(context)                
    
    override this.OnSurfaceChanged(w, h) =
        base.OnSurfaceChanged( w, h)
        let ratio = float32 w / float32 h
        if ratio < 1.0f then
            this.SetScale(0.5f, 0.5f * ratio)
        else
            this.SetScale(0.5f / ratio, 0.5f)
        GL.ClearColor(0.5f, 0.0f, 0.5f, 1.0f)
        GL.Clear(ClearBufferMask.ColorBufferBit)

    override this.OnDrawFrame() =
        //this.DrawQuad(Point(1.0f, 1.0f), Point(1.0f, 2.0f), Point(2.0f, 2.0f), Point(2.0f, 1.0f))
        //this.DrawQuad(Point(0.0f, 0.0f), Point(0.0f, 1.0f), Point(1.0f, 1.0f), Point(1.0f, 0.0f))
        this.BuildCircle(Point(1.0f, 1.0f), 0.5f)
        
        this.RenderShapes([||])
        this.DrawImage()
 

 type AdvancedShapesTestRenderer(context) =
    inherit ShapesTestRenderer(context)                
    override this.OnDrawFrame() =
        
        //this.DrawCircleContour(Point(1.0f, 1.0f), 0.95f, 0.1f)
        //this.DrawCircleContour(Point(0.5f, 1.0f), 0.25f, 0.025f)
        let points = [| Point(0.5f, 0.5f); Point(0.5f, 1.5f); Point(1.5f, 1.5f); Point(1.0f, 1.0f); Point(1.5f, 0.5f) |]
        //this.DrawPolygon(points)
        this.BuildPolygonContour(points, 0.1f)
        
        this.RenderShapes([||])
        this.DrawImage()



type CollidingAgentsEnvironment(bounds) =
    inherit Environment(bounds)

    let random = System.Random(42)
    let quadTree = QuadTree<Collider>(bounds, 8)
    let shapes = ResizeArray<ShapeAndPaint>()

    override this.Paint = Color4.White
    override this.Random = random
    override this.Delay(action, numSteps) = ()

    member this.AddCollider(collider : Collider) =
        try 
            quadTree.Insert(collider)
        with
            | :? OutOfBounds -> ()
    
    member this.Collides(collider : Collider, parent : Agent) =
        let criterion (c : Collider) = 
            (match c.Parent with | Some(p) -> (p :> IUnique) <> (parent :> IUnique) | None -> true) && collide collider c
        quadTree.Query((collider :> IHasBounds).Bounds, criterion)
    
    member this.AddShape(shape : Shape, fillType: ShapePaint) =
        shapes.Add(struct (shape, fillType))
    
    override this.Step() =
        this.StepAgents()
        let result = shapes.ToArray()
        shapes.Clear()
        result
 

type CollidingAgent() =
    inherit MovingAgent()

    let mutable lastStop = 0.0f
    
    override this.Step() =
        let prevPos = this.Position
        let prevDir = this.Direction

        this.Direction <- this.Direction + Point(
            float32((this.Environment.Value.Random.NextDouble() - 0.5) * 0.1), 
            float32((this.Environment.Value.Random.NextDouble() - 0.5) * 0.1))

        base.Step()
        
        let r = 1.0f
        let collider = CircleCollider(Some(this.GetFootprint()), this.Position + this.Direction * r, r-0.1f)
        let env = this.Environment.Value :?> CollidingAgentsEnvironment
        
        if env.Random.NextDouble() < 0.1 then
            let nextDir = if env.Random.Next(2) = 0 then this.Direction.LeftRotated else -this.Direction.LeftRotated
            env.AddAgent(CollidingAgent(Position=this.Position, Direction=nextDir))
        
        if env.Collides(collider, this) then
            this.Delete()
        elif this.DistanceCovered - lastStop > r then
           env.AddCollider(collider)
           lastStop <- this.DistanceCovered

        let prevPerp = prevDir.LeftRotated * r //* (this.Thickness * 0.5f)
        let currPerp = this.Direction.LeftRotated * r //* (this.Thickness * 0.5f)
        
        (this.Environment.Value :?> CollidingAgentsEnvironment).AddShape(
            Shape.Polygon([| prevPos + prevPerp; prevPos - prevPerp; this.Position - currPerp; this.Position + currPerp |]), 
            ShapePaint.Default)

    member this.GetFootprint() =
        // stub
        { new IDworkinAgentState with
              member this.Direction = raise (System.NotImplementedException())
              member this.DistanceCovered = raise (System.NotImplementedException())
              member this.GetFootprint() = raise (System.NotImplementedException())
              member this.Id = raise (System.NotImplementedException())
              member this.LengthFactor = raise (System.NotImplementedException())
              member this.Position = raise (System.NotImplementedException())
              member this.Thickness = raise (System.NotImplementedException())
              member this.ThicknessFactor = raise (System.NotImplementedException())
              member this.Parent = raise (System.NotImplementedException()) }


type CollidingAgentsTestRenderer(context) =
    inherit BaseRenderer(context)

    let mutable environment : CollidingAgentsEnvironment option = None

    override this.OnSurfaceChanged(width, height) =
        base.OnSurfaceChanged(width, height)

        let bounds = Rectangle.FromTLRB(0.0f, 0.0f, float32 width, float32 height)
        let env = CollidingAgentsEnvironment(bounds)
        environment <- Some(env)

        for i=1 to 1000 do
            let agent = CollidingAgent()
            agent.Position <- Point(float32(env.Random.NextDouble()) * bounds.Width, float32(env.Random.NextDouble()) * bounds.Height)
            agent.Direction <- Point(float32(env.Random.NextDouble() - 0.5), float32(env.Random.NextDouble() - 0.5))
            env.AddAgent(agent)
        
        this.SetScale(1.0f / float32 width, 1.0f / float32 height)

    override this.OnDrawFrame() =
        match environment with
        | Some(env) -> 
            this.RenderShapes(env.Step())
            this.DrawImage()
        | None -> ()


/// renderer that shows all colliders in environment
type CollidersTestRenderer(context, environmentConfig : LibDworkin.Agents.IDworkinEnvironmentConfig) =
    inherit BaseRenderer(context)

    let mutable environment : LibDworkin.Agents.DworkinEnvironment option = None
    let drawnColliders = System.Collections.Generic.HashSet<Collider>(
        { new System.Collections.Generic.IEqualityComparer<Collider> with
            member this.Equals(a, b) = 
                match a, b with
                | :? SegmentCollider as s1, (:? SegmentCollider as s2) -> s1.End = s2.End && s1.Start = s2.Start
                | :? CircleCollider as s1, (:? CircleCollider as s2) -> s1.Center = s2.Center && s1.Radius = s2.Radius
                | _ -> false
            member this.GetHashCode(a) = LanguagePrimitives.PhysicalHash a })

    override this.OnSurfaceChanged(width, height) =
        base.OnSurfaceChanged(width, height)
        environment <- Some(environmentConfig.GetEnvironment(Point(float32 width, float32 height)))        
        this.SetScale(1.0f / float32 width, 1.0f / float32 height)

    member this.ShapeForCollider(collider : Collider) : ShapeAndPaint option =
        if drawnColliders.Add collider then
            match collider with
            | :? CircleCollider as circle -> 
                Some(struct (Shape.Circle(circle.Center, circle.Radius), ShapePaint.Default))
            | :? SegmentCollider as segment -> 
                let mutable perp = (segment.End - segment.Start).PerpendicularLeft
                perp.Normalize()
                perp <- perp * 0.5f
                Some(struct (Shape.Polygon([| segment.Start + perp; segment.Start - perp; segment.End - perp; segment.End + perp |]), ShapePaint.Default))
            | _ -> None
        else
            None

    override this.OnDrawFrame() =
        let shapes = ResizeArray()
        match environment with
        | Some(env) -> 
            shapes.AddRange(env.Step())
            env.QueryColliders(env.Bounds, CollisionLayers.All, fun _ collider -> this.ShapeForCollider(collider) |> Option.iter shapes.Add; false) 
                |> ignore
        | None -> ()
        
        this.RenderShapes(shapes.ToArray())
        this.DrawImage()


type BenchmarkRenderer(context, environmentConfig : LibDworkin.Agents.IDworkinEnvironmentConfig)  =
    inherit DworkinRenderer(context, environmentConfig)

    let shapesQueue = System.Collections.Generic.Queue<int * ShapeAndPaint[]>()

    override this.OnSurfaceChanged(width, height) =
        base.OnSurfaceChanged(width, height)
        shapesQueue.Clear()
        match this.Environment with
        | Some(env) -> 
            while env.HasAgents do
                let agentsCount = env.AgentsCount
                for _ in TimeSamplers.Environment.Measure(agentsCount) do
                    shapesQueue.Enqueue(agentsCount, env.Step())
        | None -> ()

    override this.OnDrawFrame() = 
        if shapesQueue.Count > 0 then
            let mutable t = 0.0
            while t < 33.3 && shapesQueue.Count > 0 do
                let agentsCount, shapes = shapesQueue.Dequeue()
                for _ in TimeSamplers.Shapes.Measure(agentsCount) do this.RenderShapes(shapes)
                for _ in TimeSamplers.Image.Measure(agentsCount) do this.DrawImage()
                t <- t + TimeSamplers.Shapes.LastTime + TimeSamplers.Image.LastTime
        elif not this.StatsPrinted then
            let printStats (samples : TimeMeasurementSample[]) (name : string) =
                Android.Util.Log.Debug("avgTimes", 
                    sprintf "(%-6s) avg: %6.2f max: %6.2f top1%%: %6.2f stdev: %6.2f"
                        name
                        (TimeStats.average samples)
                        (TimeStats.max samples)
                        (TimeStats.top 0.01 samples)
                        (TimeStats.stdev samples) ) |> ignore
            
            printStats TimeSamplers.Environment.Samples TimeSamplers.Environment.Name
            printStats TimeSamplers.Shapes.Samples TimeSamplers.Shapes.Name
            printStats TimeSamplers.Image.Samples TimeSamplers.Image.Name
            printStats (TimeStats.add TimeSamplers.Shapes.Samples TimeSamplers.Image.Samples) "GPU"

            this.StatsPrinted <- true