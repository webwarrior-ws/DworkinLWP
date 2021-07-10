module rec LibDworkin.AgentAbstractions

open OpenTK

open Geometry

open SpecializedCollections
open SpatialIndex
open System



/// Represents shape that can be added to environment to be drawn
[<Struct>]
type Shape =
    | Circle of center: Point * radius: float32
    | Polygon of Point[]
    with
        // constructors for some frequently used polygons
        static member Triangle(a, b, c) = Polygon([| a; b; c |])
        static member Quad(a, b, c, d) = Polygon([| a; b; c; d |])
        // some basic shapes with size of 1
        static member PrimitiveCircle = Circle(Point(), 0.5f)
        static member PrimitiveTriangle = Shape.Triangle(Point(0.0f, 0.5f), Point(-0.866f, -0.5f), Point(0.866f, -0.5f))
        static member PrimitiveSquare = Shape.Quad(Point(-0.5f, -0.5f), Point(0.5f, -0.5f), Point(0.5f, 0.5f), Point(-0.5f, 0.5f))

[<Struct>]
type ShapePaint =
    {
        Fill : bool
        StrokeWidth: float32
    }
    with
        static member Default = { Fill=true; StrokeWidth=0.0f }
 

type ShapeAndPaint = (struct (Shape * ShapePaint))


type IUnique =
    interface
        abstract member Id : int
    end

/// Interface that represents some important information about Dworkin agent.
/// Used in colliders as information about parent and in DworkinProbeData class.
type IDworkinAgentState =
    interface
        inherit  IUnique
        
        abstract member Direction : Point
        abstract member Position : Point
        abstract member Thickness : float32
        abstract member DistanceCovered : float32
        abstract member ThicknessFactor : float32
        abstract member LengthFactor : float32
        abstract member Parent : IDworkinAgentState option

        abstract member GetFootprint : unit -> IDworkinAgentState
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
    
    member this.Id = id
    
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
    
    abstract member Step : unit -> unit
    default this.Step() =
        age <- age + 1
    
    interface IUnique with
        member this.Id = id


[<AbstractClass>]
type DelayAgent() =
    inherit Agent()

    member val Action : Environment -> Agent option = (fun _ -> None) with get, set

    override this.LiesWithinBounds(bounds) = true
    
    override this.Step() =
        base.Step()
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
            // precondition: dir is normalized
            direction <- dir
    
    abstract member Speed : float32 with get, set
    default this.Speed 
        with get() = 1.0f
        and set(_) = ()

    abstract member DistanceCovered : float32 
    default this.DistanceCovered = distanceCovered

    override this.LiesWithinBounds(bounds) = bounds.ContainsPoint(this.Position)

    override this.Step() =
        let currSpeed = this.Speed
        this.Position <- this.Position + (this.Direction * currSpeed)
        distanceCovered <- distanceCovered + currSpeed

        base.Step()


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

    member this.NextId =
        currId <- currId + 1
        currId
    
    abstract member Step : unit -> ShapeAndPaint[]
    
    member this.StepAgents() : unit =
        for agent in agents do
            agent.Step()
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

    member this.AgentsCount = agents.Count + agentsToAdd.Count - agentsToRemove.Count
    
    member this.HasAgents = this.AgentsCount > 0
    
    abstract member HasActiveAgents : bool
    default this.HasActiveAgents = 
        this.HasAgents