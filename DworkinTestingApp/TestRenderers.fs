module DworkinTestingApp.TestRenderers

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.ES20
open Javax.Microedition.Khronos

open LibDworkin.Geometry
open LibDworkin.Renderer
open LibDworkin.AgentAbstractions
open LibDworkin.SpatialIndex
open LibDworkin.Colliders
open LibDworkin.SpecializedCollections



type ShapesTestRenderer(context) =
    inherit BaseRenderer(context)                
    
    override this.OnSurfaceChanged(gl, w, h) =
        base.OnSurfaceChanged(gl, w, h)
        let ratio = float32 w / float32 h
        if ratio < 1.0f then
            this.SetScale(0.5f, 0.5f * ratio)
        else
            this.SetScale(0.5f / ratio, 0.5f)
        GL.ClearColor(0.5f, 0.0f, 0.5f, 1.0f)
        GL.Clear(ClearBufferMask.ColorBufferBit)

    override this.OnDrawFrame(gl : Opengles.IGL10) =
        let canvas = this :> ICanvas
        //canvas.DrawQuad(Point(1.0f, 1.0f), Point(1.0f, 2.0f), Point(2.0f, 2.0f), Point(2.0f, 1.0f))
        //canvas.DrawQuad(Point(0.0f, 0.0f), Point(0.0f, 1.0f), Point(1.0f, 1.0f), Point(1.0f, 0.0f))
        canvas.DrawCircle(Point(1.0f, 1.0f), 0.5f)
        
        this.RenderShapes()
        this.DrawMaskedImage()
 

 type AdvancedShapesTestRenderer(context) =
    inherit ShapesTestRenderer(context)                
    override this.OnDrawFrame(gl : Opengles.IGL10) =
        let canvas = this :> ICanvas
        
        //canvas.DrawCircleContour(Point(1.0f, 1.0f), 0.95f, 0.1f)
        //canvas.DrawCircleContour(Point(0.5f, 1.0f), 0.25f, 0.025f)
        let points = [| Point(0.5f, 0.5f); Point(0.5f, 1.5f); Point(1.5f, 1.5f); Point(1.0f, 1.0f); Point(1.5f, 0.5f) |]
        //canvas.DrawPolygon(points)
        canvas.DrawPolygonContour(points, 0.1f)
        
        this.RenderShapes()
        this.DrawMaskedImage()



type CollidingAgentsEnvironment(bounds) =
    inherit Environment(bounds)

    let random = System.Random(42)
    let quadTree = QuadTree<Collider>(bounds, 8)

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
            (match c.Parent with | Some(p) -> p <> parent | None -> true) && collide collider c
        quadTree.Query((collider :> IHasBounds).Bounds, criterion)
 

type CollidingAgent() =
    inherit MovingAgent()

    let mutable lastStop = 0.0f
    
    override this.Step(canvas) =
        let prevPos = this.Position
        let prevDir = this.Direction

        this.Direction <- this.Direction + Point(
            float32((this.Environment.Value.Random.NextDouble() - 0.5) * 0.1), 
            float32((this.Environment.Value.Random.NextDouble() - 0.5) * 0.1))

        base.Step(canvas)
        
        let r = 1.0f
        let collider = CircleCollider(Some(this :> Agent), this.Position + this.Direction * r, r-0.1f)
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
        
        canvas.DrawQuad(prevPos + prevPerp, prevPos - prevPerp, this.Position - currPerp, this.Position + currPerp)



type CollidingAgentsTestRenderer(context) =
    inherit BaseRenderer(context)

    let mutable environment : CollidingAgentsEnvironment option = None

    override this.OnSurfaceChanged(gl, width, height) =
        base.OnSurfaceChanged(gl, width, height)

        let bounds = Rectangle.FromTLRB(0.0f, 0.0f, float32 width, float32 height)
        let env = CollidingAgentsEnvironment(bounds)
        environment <- Some(env)

        for i=1 to 1000 do
            let agent = CollidingAgent()
            agent.Position <- Point(float32(env.Random.NextDouble()) * bounds.Width, float32(env.Random.NextDouble()) * bounds.Height)
            agent.Direction <- Point(float32(env.Random.NextDouble() - 0.5), float32(env.Random.NextDouble() - 0.5))
            env.AddAgent(agent)
        
        this.SetScale(1.0f / float32 width, 1.0f / float32 height)

    override this.OnDrawFrame(gl : Opengles.IGL10) =
        environment |> Option.iter (fun env -> env.Step(this))
        this.RenderShapes()
        this.DrawMaskedImage()


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

    override this.OnSurfaceChanged(gl, width, height) =
        base.OnSurfaceChanged(gl, width, height)
        environment <- Some(environmentConfig.GetEnvironment(Point(float32 width, float32 height)))        
        this.SetScale(1.0f / float32 width, 1.0f / float32 height)

    member this.DrawCollider(collider : Collider) =
        if drawnColliders.Add collider then
            match collider with
            | :? CircleCollider as circle -> (this :> ICanvas).DrawCircle(circle.Center, circle.Radius)
            | :? SegmentCollider as segment -> 
                let mutable perp = (segment.End - segment.Start).PerpendicularLeft
                perp.Normalize()
                perp <- perp * 0.5f
                (this :> ICanvas).DrawQuad(segment.Start + perp, segment.Start - perp, segment.End - perp, segment.End + perp)
            | _ -> ()

    override this.OnDrawFrame(gl : Opengles.IGL10) =
        match environment with
        | Some(env) -> 
            env.Step(this)
            env.QueryColliders(env.Bounds, CollisionLayers.All, fun _ collider -> this.DrawCollider(collider); false) 
                |> ignore
        | None -> ()
        
        this.RenderShapes()
        this.DrawMaskedImage()