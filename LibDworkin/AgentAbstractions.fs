module rec LibDworkin.AgentAbstractions

open Geometry

open OpenTK
open SpecializedCollections
open SpatialIndex
open System


/// Interface that defines methods for drawing shapes such as triangle, quad, or circle
type ICanvas = 
    interface
        abstract member DrawTriangle : Point * Point * Point -> unit
        abstract member DrawQuad : Point * Point * Point * Point -> unit
        abstract member DrawCircle : Point * float32 -> unit
        abstract member DrawCircleContour : Point * float32 * float32 -> unit
        abstract member DrawPolygon : Point[] -> unit
        abstract member DrawPolygonContour : Point[] * float32 -> unit
    end


type IUnique =
    interface
        abstract member Id : int
    end

/// Interface that represents some important information about Dworkin agent.
/// Used in colliders as information about parent and in DworkinProbeData class.
type IDworkinAgentState =
    interface
        inherit  IUnique
        
    end


[<AbstractClass>]
type Collider(parent : IDworkinAgentState option) =
    interface SpatialIndex.IHasBounds with
        member this.Bounds =
            if this.BoundingBox.IsEmpty then
                this.BoundingBox <- this.CalculateBoundingBox()
            this.BoundingBox
    
    /// Note: empty rectangle means no value
    member val internal BoundingBox : Rectangle = Rectangle() with get, set
    member val Parent = parent with get, set
    
    abstract member CalculateBoundingBox : unit -> Rectangle
    
    abstract member FlushCache : unit -> unit
    default this.FlushCache() =
        this.BoundingBox <- Rectangle()



[<AbstractClass>]
type Agent() =
    let mutable age = 0
    let mutable environment : Environment option = None
    let mutable id = 0

    member this.Environment
        with get() : Environment option = environment
        and set(env : Environment option) = 
            environment <- env
            Option.iter this.InitializeIn environment
    
    member internal this.Id = id
    
    member this.Age = age
    
    member this.Delete() = 
        match environment with
        | Some(env) -> env.RemoveAgent(this) |> ignore
        | None -> ()
    
    abstract member InitializeIn : Environment -> unit
    default this.InitializeIn(env) = 
        id <- env.NextId
    
    interface IEquatable<IUnique> with
        override this.Equals(other) =
            this.Id = other.Id
    
    override this.Equals(other) =
        match other with
        | :? IUnique as otherAgent -> this.Id = otherAgent.Id
        | _ -> false
    override this.GetHashCode() = hash this.Id
    
    abstract member LiesWithinBounds : Rectangle -> bool

    abstract member Step : ICanvas -> unit
    default this.Step(canvas) =
        age <- age + 1
    
    interface IUnique with
        member this.Id = id


[<AbstractClass>]
type DelayAgent() =
    inherit Agent()

    member val Action : Environment -> Agent option = (fun _ -> None) with get, set

    override this.LiesWithinBounds(bounds) = true
    
    override this.Step(canvas) =
        base.Step(canvas)
        if this.ReadyToRelease then
            match this.Environment with
            | Some(env) ->
                this.Action env |> ignore
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
    
    abstract member Speed : float32 with get, set
    default this.Speed 
        with get() = 1.0f
        and set(_) = ()

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
    let mutable currId = 0
    let agents = ResizeArray<Agent>()

    let agentsToRemove = Collections.Generic.HashSet<Agent>()
    let agentsToAdd = ResizeArray<Agent>()

    member this.Age = age
    member this.Bounds = bounds

    abstract member Random : System.Random
    abstract member Paint : OpenTK.Graphics.Color4

    member this.NextId =
        currId <- currId + 1
        currId
    
    abstract member Step : ICanvas -> unit
    default this.Step(canvas : ICanvas) : unit =
        for agent in agents do
            agent.Step(canvas)
        age <- age + 1

        agents.RemoveAll(fun agent -> (agent.LiesWithinBounds(bounds) |> not) || agentsToRemove.Contains(agent)) 
            |> ignore
        agentsToRemove.Clear()

        agents.AddRange(agentsToAdd)
        agentsToAdd.Clear()
    
    abstract member AddAgent: Agent -> unit
    default this.AddAgent(agent : Agent) = 
        agent.Environment <- Some(this)
        agentsToAdd.Add(agent)
    
    abstract member RemoveAgent: Agent -> bool
    default this.RemoveAgent(agent) = agentsToRemove.Add(agent)
    
    abstract member Delay : (Environment -> Agent option) * int -> unit

    member this.AgentsCount = agents.Count
    
    member this.HasAgents = agents.Count > 0
    
    abstract member HasActiveAgents : bool
    default this.HasActiveAgents = 
        this.HasAgents