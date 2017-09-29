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



type ShapesTestRenderer(context) =
    inherit BaseRenderer(context)                
    
    override this.OnSurfaceChanged(gl, w, h) =
        base.OnSurfaceChanged(gl, w, h)
        GL.ClearColor(0.5f, 0.0f, 0.5f, 1.0f)
        GL.Clear(ClearBufferMask.ColorBufferBit)

    override this.OnDrawFrame(gl : Opengles.IGL10) =
        let canvas = this :> ICanvas
        canvas.DrawQuad(Vector2(-1.0f, -1.0f), Vector2(0.0f, -1.0f), Vector2(0.0f, 0.0f), Vector2(-1.0f, 0.0f))
        canvas.DrawQuad(Vector2(0.0f, 0.0f), Vector2(0.0f, 1.0f), Vector2(1.0f, 1.0f), Vector2(1.0f, 0.0f))
        
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
        quadTree.Exists((collider :> IHasBounds).Bounds, fun c -> Some(parent) <> c.Parent && collide collider c)
 

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

        for i=1 to 100 do
            let agent = CollidingAgent()
            agent.Position <- Point(float32(env.Random.NextDouble()) * bounds.Width, float32(env.Random.NextDouble()) * bounds.Height)
            agent.Direction <- Point(float32(env.Random.NextDouble() - 0.5), float32(env.Random.NextDouble() - 0.5))
            env.AddAgent(agent)
        
        this.SetScale(1.0f / float32 width, 1.0f / float32 height)

    override this.OnDrawFrame(gl : Opengles.IGL10) =
        environment |> Option.iter (fun env -> env.Step(this))
        this.RenderShapes()
        this.DrawMaskedImage()
