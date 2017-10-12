module LibDworkin.Configs


// Concrete config classes

open Aether
open Geometry
open PharoCompatibility
open AgentAbstractions
open Colliders
open Agents
open Brushes


// Brush strategies

type BrushStrategyCircles() = 
    inherit BrushStrategy()
    override this.GetBrushFor(anAgent) =
        BrushCircles() :> Brush
    
(*    
type BrushStrategyCirclesPacked() = 
    inherit BrushStrategy()
    member val Gap = 0.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> BrushStrategyCirclesPacked

    static member Gap_ : Lens<BrushStrategy, float32> = 
        (fun x -> (x :?> BrushStrategyCirclesPacked).Gap), (fun value x -> let y = (x :?> BrushStrategyCirclesPacked).Clone() in y.Gap <- value; y :> BrushStrategy)

    override this.GetBrushFor(anAgent) =
        BrushCirclesPacked(anAgent, Gap=this.Gap)
*)

type BrushStrategyDefault() = 
    inherit BrushStrategy()
    override this.IsFinal = false

    member this.FallbackBrush =
        BrushSolid(Cap=BrushCap.NoCap, AlignWithParent=false)    

    override this.GetBrushFor(anAgent) : Brush =
        let mutable curr = anAgent.Parent
        while curr |> Option.exists (fun a -> a.Config.Brush.IsFinal) do
            curr <- curr.Value.Parent
        match curr with 
        | Some(a) -> a.Config.Brush.GetBrushFor(anAgent) 
        | None -> this.FallbackBrush :> Brush


type BrushStrategyNone() = 
    inherit BrushStrategy()
    override this.GetBrushFor(anAgent) =
        BrushNone() :> Brush

    member this.IsFinal() = false
    

type BrushStrategySolid() = 
    inherit BrushStrategy()
    member val AlignWithParent = false with get, set
    member val Cap : BrushCap = BrushCap.NoCap with get, set

    member this.Clone() = this.MemberwiseClone() :?> BrushStrategySolid

    static member AlignWithParent_ : Lens<BrushStrategy, bool> = 
        (fun x -> (x :?> BrushStrategySolid).AlignWithParent), (fun value x -> let y = (x :?> BrushStrategySolid).Clone() in y.AlignWithParent <- value; y :> BrushStrategy)

    static member Cap_ : Lens<BrushStrategy, BrushCap> = 
        (fun x -> (x :?> BrushStrategySolid).Cap), (fun value x -> let y = (x :?> BrushStrategySolid).Clone() in y.Cap <- value; y :> BrushStrategy)

    override this.GetBrushFor(anAgent) =
        BrushSolid(Cap=this.Cap, AlignWithParent=this.AlignWithParent) :> Brush

    
type BrushStrategySolidJoint() = 
    inherit BrushStrategy()
    member val JointType : BrushJoint = BrushJoint.Triangle with get, set

    member this.Clone() = this.MemberwiseClone() :?> BrushStrategySolidJoint

    static member JointType_ : Lens<BrushStrategy, BrushJoint> = 
        (fun x -> (x :?> BrushStrategySolidJoint).JointType), (fun value x -> let y = (x :?> BrushStrategySolidJoint).Clone() in y.JointType <- value; y :> BrushStrategy)

    override this.GetBrushFor(anAgent) =
        BrushSolidJoint(JointType=this.JointType) :> Brush
 

 type BrushStrategy with
    static member Default = BrushStrategyDefault()
 

// collider strategies
[<AbstractClass>]
type ColliderStrategy() =
    abstract member Evaluate : DworkinAgent * CollisionLayer -> unit


type ColliderStrategyCircles() = 
    inherit ColliderStrategy()
    member val Density = 1.0f with get, set
    member val Offset = 0.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> ColliderStrategyCircles

    static member Density_ : Lens<ColliderStrategy, float32> = 
        (fun x -> (x :?> ColliderStrategyCircles).Density), (fun value x -> let y = (x :?> ColliderStrategyCircles).Clone() in y.Density <- value; y :> ColliderStrategy)

    static member Offset_ : Lens<ColliderStrategy, float32> = 
        (fun x -> (x :?> ColliderStrategyCircles).Offset), (fun value x -> let y = (x :?> ColliderStrategyCircles).Clone() in y.Offset <- value; y :> ColliderStrategy)

    member this.AddCollider(agent : DworkinAgent, pos, r, layer) =
        let collider = CircleCollider(Some(agent.Copy() :> Agent), pos, r)
        agent.DworkinEnvironment.AddColliderToLayer(collider, layer)
        collider

    override this.Evaluate(agent, layer) =
        let mutable lastCollider = null
        let mutable pos = null
        if agent.DistanceCovered >= this.Offset then
            let mutable pos = agent.Position
            let r = agent.Thickness * 0.5f
            match agent.Attributes.LastCollider.Get() with
            | Some(collider) -> 
                let mutable lastCollider = collider :?> CircleCollider
                let minR = agent.ThicknessFactor * 0.01f
                let mutable delta = 0.0f
                let mutable dist = 0.0f
                let criterion () = 
                    delta <- (r + lastCollider.Radius) / this.Density
                    dist <- agent.Position.DistanceTo(lastCollider.Center)
                    (r > minR) && ((dist - delta) > 0.0f)
                while criterion() do
                    pos <- lerp lastCollider.Center agent.Position (delta / dist)
                    lastCollider <- this.AddCollider(agent, pos, r, layer)
                agent.Attributes.LastCollider.Set(lastCollider)
            | None -> 
                agent.Attributes.LastCollider.Set(CircleCollider(Some(agent :> Agent), pos, 0.0f))
    

type ColliderStrategySegments() = 
    inherit ColliderStrategy()
    member val MinLength = 1.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> ColliderStrategySegments

    static member MinLength_ : Lens<ColliderStrategy, float32> = 
        (fun x -> (x :?> ColliderStrategySegments).MinLength), (fun value x -> let y = (x :?> ColliderStrategySegments).Clone() in y.MinLength <- value; y :> ColliderStrategy)

    override this.Evaluate(agent, layer) =
        let mutable lastCollider = null
        let pos = agent.Position
        match agent.Attributes.LastCollider.Get() with
        | Some(collider) -> 
            let mutable lastCollider = collider :?> SegmentCollider
            if lastCollider.End.SquaredDistanceTo(pos) < (squared (this.MinLength * agent.LengthFactor)) then
                ()
            else
                lastCollider <- SegmentCollider(Some(agent :> Agent), lastCollider.End, pos)
                agent.DworkinEnvironment.AddColliderToLayer(lastCollider, layer)
                agent.Attributes.LastCollider.Set(lastCollider)
        | None -> 
            agent.Attributes.LastCollider.Set(SegmentCollider(Some(agent :> Agent), pos, pos))
    

type ColliderStrategy with
    static member Default = ColliderStrategyCircles()


// collider options
type ColliderOptionsNone() =
    inherit ColliderOptions()
    override this.Evaluate(agent) = ()


type ColliderOptionsGeneral() =
    inherit ColliderOptions()

    member val Layer = CollisionLayer.Default with get, set
    member val Strategy = ColliderStrategy.Default with get, set

    override this.Evaluate(agent) =
        this.Strategy.Evaluate(agent, this.Layer)


type ColliderOptions with
    static member Default = ColliderOptionsNone()


// 