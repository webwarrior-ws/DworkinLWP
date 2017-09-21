module LibDworkin.ForTesting

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.ES20
open Javax.Microedition.Khronos

open LibDworkin.Geometry
open LibDworkin.Renderer
open LibDworkin.AgentAbstractions
open LibDworkin.SpatialIndex
open LibDworkin.Colliders



type ShapesTestRenderer(wallpaperService) =
    inherit BaseRenderer(wallpaperService)                
    
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
        quadTree.Insert(collider)
    
    member this.Collides(collider : Collider) =
        not <| quadTree.Query((collider :> IHasBounds).Bounds, fun c -> not (collide collider c))
 

type CollidingAgent() =
    inherit MovingAgent()
    
    override this.Step(canvas) =
        let prevPos = this.Position
        let prevDir = this.Direction

        base.Step(canvas)
        // detect collisions...

        let prevPerp = prevDir.LeftRotated * 2.0f //* (this.Thickness * 0.5f)
        let currPerp = this.Direction.LeftRotated * 2.0f //* (this.Thickness * 0.5f)
        
        canvas.DrawQuad(prevPos + prevPerp, prevPos - prevPerp, this.Position - currPerp, this.Position + currPerp)



type CollidingAgentsTestRenderer(wallpaperService) =
    inherit BaseRenderer(wallpaperService)

    let environment = CollidingAgentsEnvironment(Rectangle.FromTLRB(0.0f, 0.0f, 800.0f, 600.0f))

    override this.OnDrawFrame(gl : Opengles.IGL10) =
        environment.Step(this)
        this.RenderShapes()
        this.DrawMaskedImage()
