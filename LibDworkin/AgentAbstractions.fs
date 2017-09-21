module rec LibDworkin.AgentAbstractions

open Geometry

open OpenTK
open SpecializedCollections
open SpatialIndex


/// Interface that defines methods for drawing shapes such as triangle, quad, or circle
type ICanvas = 
    interface
        abstract member DrawTriangle : Point * Point * Point -> unit
        abstract member DrawQuad : Point * Point * Point * Point -> unit
    end


[<AbstractClass>]
type Collider(parent : Agent option) =
    let mutable boundingBox : Rectangle option = None

    interface SpatialIndex.IHasBounds with
        member this.Bounds =
            match boundingBox with
            | Some(rect) -> rect
            | None ->
                let rect = this.CalculateBoundingBox()
                boundingBox <- Some(rect)
                rect
     
    member this.Parent = parent
    member this.BoundingBox
        with get() : Rectangle option = boundingBox
        and set(rect : Rectangle option) = boundingBox <- rect
    
    abstract member CalculateBoundingBox : unit -> Rectangle
    
    abstract member FlushCache : unit -> unit
    default this.FlushCache() =
        boundingBox <- None



[<AbstractClass>]
type Agent() =
    let mutable age = 0
    let mutable environment : Environment option = None

    member this.Environment
        with get() : Environment option = environment
        and set(env : Environment option) = 
            environment <- env
            Option.iter this.InitializeIn environment
    
    member this.Age = age
    
    member this.Delete() = 
        match environment with
        | Some(env) -> env.RemoveAgent(this) |> ignore
        | None -> ()

    abstract member Do : (Agent -> Agent) -> unit
    default this.Do(action) = action this |> ignore
    
    abstract member InitializeIn : Environment -> unit
    default this.InitializeIn(env) = ()
    
    abstract member LiesWithinBounds : Rectangle -> bool

    abstract member Step : ICanvas -> unit
    default this.Step(canvas) =
        age <- age + 1


[<AbstractClass>]
type DelayAgent(initialAction : Environment -> Agent option) =
    inherit Agent()

    let mutable action = initialAction

    override this.LiesWithinBounds(bounds) = true

    override this.Do(func) =
        let oldAction = action
        action <- fun env -> Option.map func (oldAction env)
    
    override this.Step(canvas) =
        base.Step(canvas)
        if this.ReadyToRelease then
            match this.Environment with
            | Some(env) ->
                action env |> ignore
                this.Delete()
            | None -> ()
    
    abstract member ReadyToRelease : bool


[<AbstractClass>]
type MovingAgent() =
    inherit Agent()

    let mutable distanceCovered = 0.0f
    let mutable direction = Point(-1.0f, 0.0f)

    member val Position = Point(0.0f, 0.0f) with get, set
    member this.Direction 
        with get() : Point = direction
        and set(dir : Point) =
            direction <- dir
            direction.Normalize()
    
    abstract member Speed : float32 
    default this.Speed = 1.0f

    abstract member DistanceCovered : float32 
    default this.DistanceCovered = distanceCovered

    override this.LiesWithinBounds(bounds) = bounds.ContainsPoint(this.Position)

    override this.Step(canvas) =
        let currSpeed = this.Speed
        this.Position <- this.Position + (this.Direction * currSpeed)
        distanceCovered <- distanceCovered + currSpeed

        base.Step(canvas)


[<AbstractClass>]
type Environment(bounds : Rectangle) = 
    let mutable age = 0
    let agents = ResizeArray<Agent>()
    let mutable currId = 0
    // TODO: add everything to do with colliders
    // TODO: spawning
    // Collision detection and spawning methods will be added to actual DworkinEnvironment

    member this.Age = age
    member this.Bounds = bounds

    abstract member Random : System.Random
    abstract member Paint : OpenTK.Graphics.Color4

    member this.NextId =
        currId <- currId + 1
        currId
    
    member this.Step(canvas : ICanvas) =
        for agent in agents.ToArray() do
            agent.Step(canvas)
        age <- age + 1
        agents.RemoveAll(fun agent -> agent.LiesWithinBounds(bounds) |> not) |> ignore
    
    abstract member AddAgent: Agent -> unit
    default this.AddAgent(agent : Agent) = 
        agent.Environment <- Some(this)
        agents.Add(agent)
    
    abstract member RemoveAgent: Agent -> bool
    default this.RemoveAgent(agent) = agents.Remove(agent)
    
    abstract member Delay : (Environment -> Agent option) * int -> unit
    
    member this.HasAgents = agents.Count > 0
    
    abstract member HasActiveAgents : bool
    default this.HasActiveAgents = 
        this.HasAgents