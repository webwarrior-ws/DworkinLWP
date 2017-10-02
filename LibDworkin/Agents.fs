module rec LibDworkin.Agents

open System

open Geometry
open SpecializedCollections
open SpatialIndex
open AgentAbstractions
open Colliders


[<Struct>]
type CollisionLayer = CollisionLayer of int

type Collision =
    {
        Subject: Collider
        Obstacle: Collider
        Layer: CollisionLayer
    }

//
type DworkinEnvironmentConfig = 
    interface
        abstract GetEnvironment : unit -> DworkinEnvironment
    end


type DworkinAgentConfig = 
    interface
        // TODO: complete
    end


type DworkinAgent(config: DworkinAgentConfig, randomSeed : int) =
    inherit MovingAgent()
    (* #paint. #brush. #terminationConditions*)
    let mutable speed = Nullable<float32>()
    let mutable curvature = Nullable<float32>()
    let mutable thickness = Nullable<float32>()
    let mutable flipFactor = 1.0f

    let mutable id = 0

    member val Random = Random(randomSeed)
    member val ThicknessFactor = 1.0f with get, set
    member val LengthFactor = 1.0f with get, set
    member val Generation = 1 with get, set
    member val GenerationLimit = 50 with get, set
    member val Collision : Collision option = None with get, set

    member this.Id = id
    
    interface IEquatable<Agent> with
        override this.Equals(other) =
            match other with
            | :? DworkinAgent as otherAgent -> this.Id = otherAgent.Id
            | _ -> false
    
    override this.Equals(other) =
        match other with
        | :? DworkinAgent as otherAgent -> this.Id = otherAgent.Id
        | _ -> false
    override this.GetHashCode() = hash this.Id
        

type DworkinEnvironment(bounds, randomSeed : int) =
    inherit Environment(bounds)
    (* #numDelayAgents *)
    let initalQuadTreeBounds = Rectangle.FromTLRB(bounds.Top - 10.0f, bounds.Left - 10.0f, bounds.Right + 15.0f, bounds.Bottom + 15.0f)
    let mutable currId = 0
    let colliders : Collections.Generic.IDictionary<int, QuadTree<Collider>> = SmallDictionary.Create()
    let mutable random = Random(randomSeed)
    do 
        random.Next() |> ignore
    
    override this.Random = random

    override this.Delay(action , numSteps) =
        failwith "notimplemented"
    
    override this.Paint = OpenTK.Graphics.Color4.White // ?? is needed?

