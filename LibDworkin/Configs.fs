module LibDworkin.Configs


// Concrete config classes

open Aether
open Geometry
open PharoCompatibility
open AgentAbstractions
open Colliders
open Agents
open Brushes
open System


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
       
    static member val Instance = BrushStrategyDefault()

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
    static member Default = BrushStrategyDefault.Instance
 

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
    static member Default = ColliderStrategyCircles() :> ColliderStrategy


// collider options
type ColliderOptionsNone() =
    inherit ColliderOptions()
    static member val Instance = ColliderOptionsNone()
    override this.Evaluate(agent) = ()


type ColliderOptionsGeneral() =
    inherit ColliderOptions()

    member val Layer = CollisionLayer.Default with get, set
    member val Strategy : ColliderStrategy = ColliderStrategy.Default with get, set

    member this.Clone() = this.MemberwiseClone() :?> ColliderOptionsGeneral

    static member Layer_ : Lens<ColliderOptions, CollisionLayer> = 
        (fun x -> (x :?> ColliderOptionsGeneral).Layer), (fun value x -> let y = (x :?> ColliderOptionsGeneral).Clone() in y.Layer <- value; y :> ColliderOptions)

    static member Strategy_ : Lens<ColliderOptions, ColliderStrategy> = 
        (fun x -> (x :?> ColliderOptionsGeneral).Strategy), (fun value x -> let y = (x :?> ColliderOptionsGeneral).Clone() in y.Strategy <- value; y :> ColliderOptions)

    override this.Evaluate(agent) =
        this.Strategy.Evaluate(agent, this.Layer)


type ColliderOptions with
    static member Default = ColliderOptionsNone.Instance :> ColliderOptions


// speed strategies
type AgentSpeedAdaptive() = 
    inherit AgentSpeed()
    static member val Instance = AgentSpeedAdaptive()
    override this.Evaluate(agent) =
        sqrt (max 0.01f (1.0f - (abs ((agent.Curvature / Single.Pi) * 4.0f))))
    

type AgentSpeedConstant() = 
    inherit AgentSpeed()
    member val Magnitude = 1.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> AgentSpeedConstant

    static member Magnitude_ : Lens<AgentSpeed, float32> = 
        (fun x -> (x :?> AgentSpeedConstant).Magnitude), (fun value x -> let y = (x :?> AgentSpeedConstant).Clone() in y.Magnitude <- value; y :> AgentSpeed)

    override this.Evaluate(agent) =
        this.Magnitude
    

type AgentSpeedScaleAdaptive() = 
    inherit AgentSpeedAdaptive()
    static member val Instance = AgentSpeedScaleAdaptive()
    override this.Evaluate(agent) =
        base.Evaluate(agent) / agent.LengthFactor

    
type AgentSpeedThickness() = 
    inherit AgentSpeed()
    member val Factor = 1.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> AgentSpeedThickness

    static member Factor_ : Lens<AgentSpeed, float32> = 
        (fun x -> (x :?> AgentSpeedThickness).Factor), (fun value x -> let y = (x :?> AgentSpeedThickness).Clone() in y.Factor <- value; y :> AgentSpeed)

    override this.Evaluate(agent) =
        max (agent.Thickness * this.Factor / agent.LengthFactor) 0.001f
    

type AgentSpeed with
    static member Default = AgentSpeedAdaptive.Instance


// spawning
[<AbstractClass>]
type SpawnConfig() = 
    member this.Evaluate(anAgent) =
        this.Evaluate(anAgent, anAgent.Position)
    
    abstract member Evaluate: DworkinAgent * Point -> unit


type SpawnConfig_Default() =
    inherit SpawnConfig()
    static member val Instance = SpawnConfig_Default()
    override this.Evaluate(agent, pos) = ()


type SpawnConfig with
    static member Default = SpawnConfig_Default.Instance :> SpawnConfig



// branching 
type BranchingNone() = 
    inherit BranchingStrategy()
    static member val Instance = BranchingNone()
    override this.Evaluate(agent) = ()
    

[<AbstractClass>]
type BranchingCustom() = 
    inherit BranchingStrategy()


type BranchingStrategy with
    static member Default = BranchingNone.Instance


// branching criterions
[<AbstractClass>]
type BranchingCriterion() = 
    abstract member Tag : int
    default this.Tag = this.GetHashCode()
    
    abstract member Evaluate : DworkinAgent -> bool
    

[<AbstractClass>]
type BranchingCriterionCustom() = 
    inherit BranchingCriterion()
    override this.Evaluate(anAgent) =
        this.Evaluate(anAgent, anAgent.LastBranchMarkFor(this.Tag))
    
    abstract member Evaluate: DworkinAgent * BranchMark -> bool


[<AbstractClass>]
type BranchingCriterionFormula() = 
    inherit BranchingCriterion()
    abstract member FallbackValue : float32
    
    member this.FuncValueFor(anAgent : DworkinAgent) =
        try
            this.Evaluate(anAgent.S, anAgent.T)
        with
        | _ -> this.FallbackValue
    
    abstract member Evaluate: float32 * float32 -> float32


[<AbstractClass>]
type BranchingCriterionDensity() = 
    inherit BranchingCriterionFormula()
    override this.Evaluate(anAgent) =
        let f = this.FuncValueFor(anAgent)
        let mark = anAgent.LastBranchMarkFor(this.Tag)
        mark.IncrementCumulativeDensityBy((f * anAgent.Speed) / anAgent.LengthFactor)
        mark.CumulativeDensity >= 1.0f
    
    override this.FallbackValue = 0.0f
    

[<AbstractClass>]
type BranchingCriterionInterval() = 
    inherit BranchingCriterionFormula()
    override this.Evaluate(anAgent) =
        (anAgent.S - anAgent.LastBranchMarkFor(this.Tag).S) >= this.FuncValueFor(anAgent)
    
    override this.FallbackValue =  10000.0f
  

type private BranchingCriterionInterval_Default() =
    inherit BranchingCriterionInterval()
    override this.Evaluate(s, t) = 10.0f


[<AbstractClass>]
type BranchingCriterionIntervalRandomized() = 
    inherit BranchingCriterion()
    override this.Evaluate(anAgent) =
        let mark = anAgent.LastBranchMarkFor(this.Tag)
        let d = 
            if mark.NextInterval.HasValue then
                mark.NextInterval.Value
            else
                let value = max (this.Evaluate(anAgent.S, anAgent.T, anAgent.Random)) 0.0f
                mark.NextInterval <- Nullable(value)
                value
        (anAgent.S - mark.S) >= d
    
    abstract member Evaluate: float32 * float32 * Random -> float32


type BranchingCriterion with
    static member Default = BranchingCriterionInterval_Default() :> BranchingCriterion
    

type BranchingCriterionBased() = 
    inherit BranchingStrategy()
    member val Criterion : BranchingCriterion = BranchingCriterion.Default with get, set
    member val SpawnConfig : SpawnConfig = SpawnConfig.Default with get, set

    member this.Clone() = this.MemberwiseClone() :?> BranchingCriterionBased

    static member Criterion_ : Lens<BranchingStrategy, BranchingCriterion> = 
        (fun x -> (x :?> BranchingCriterionBased).Criterion), (fun value x -> let y = (x :?> BranchingCriterionBased).Clone() in y.Criterion <- value; y :> BranchingStrategy)

    static member SpawnConfig_ : Lens<BranchingStrategy, SpawnConfig> = 
        (fun x -> (x :?> BranchingCriterionBased).SpawnConfig), (fun value x -> let y = (x :?> BranchingCriterionBased).Clone() in y.SpawnConfig <- value; y :> BranchingStrategy)

    override this.Evaluate(agent) =
        if this.Criterion.Evaluate(agent) then
            this.SpawnConfig.Evaluate(agent)
            agent.AddBranchMarkFor(BranchMark(agent.DistanceCovered), this.Criterion.Tag)
    

// termination
type TerminationNoop() = 
    inherit TerminationStrategy()
    static member val Instance = TerminationNoop()

    override this.Evaluate(agent) = ()
    

[<AbstractClass>]
type TerminationCustom() = 
    inherit TerminationStrategy()
    override this.Evaluate(agent) =
        this.Evaluate(agent, agent.Collision)
    
    abstract member Evaluate: DworkinAgent * Collision option -> unit


// Since it is single-function config but at the same time it must use SpawnConfig instance, 
// Pharo -> F# transplier must handle cases involving TerminationSpawn separately (introduce SpawnConfig_ lens into chain)
type TerminationSpawn() = 
    inherit TerminationStrategy()
    member val SpawnConfig = SpawnConfig.Default with get, set

    member this.Clone() = this.MemberwiseClone() :?> TerminationSpawn

    static member SpawnConfig_ : Lens<TerminationStrategy, SpawnConfig> = 
        (fun x -> (x :?> TerminationSpawn).SpawnConfig), (fun value x -> let y = (x :?> TerminationSpawn).Clone() in y.SpawnConfig <- value; y :> TerminationStrategy)

    override this.Evaluate(agent) =
        this.SpawnConfig.Evaluate(agent)



// default values of various config classes
type TerminationStrategy with
    static member Default = TerminationNoop.Instance :> TerminationStrategy

type TerminationConditions with
    static member Default = { Distance=Some(TerminationConditionDistance.Instance); Collision=None; Curvature=None }

type private CurvatureFormula_Default() =
    inherit CurvatureFormula()
    static member val Instance = CurvatureFormula_Default()
    override this.Evaluate(s, t) = 0.0f

type private ThicknessFormula_Default() =
    inherit ThicknessFormula()
    static member val Instance = ThicknessFormula_Default()
    override this.Evaluate(s, t) = 1.0f

type CurvatureFormula with
    static member Default = CurvatureFormula_Default.Instance :> CurvatureFormula

type ThicknessFormula with
    static member Default = ThicknessFormula_Default.Instance :> ThicknessFormula


// default for DworkinAgentConfig
type DworkinAgentConfig with
    static member Default = { 
        Curvature=CurvatureFormula.Default; 
        Thickness=ThicknessFormula.Default; 
        Speed=AgentSpeed.Default;
        TotalDistance=100.0f;
        Brush=BrushStrategy.Default; 
        Branching=BranchingStrategy.Default; 
        ColliderOptions=ColliderOptions.Default; 
        Termination=TerminationStrategy.Default; 
        TerminationConditions=TerminationConditions.Default; 
    }
