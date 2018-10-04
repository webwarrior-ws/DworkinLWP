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
        while (match curr with | Some(a) -> not a.Config.Brush.IsFinal | None -> false) do
            curr <- curr.Value.Parent
        match curr with 
        | Some(a) when a.Config.Brush.IsFinal -> a.Config.Brush.GetBrushFor(anAgent) 
        | _ -> this.FallbackBrush :> Brush


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

    static member AlignWithParent_ : Lens<BrushStrategySolid, bool> = 
        (fun x -> x.AlignWithParent), (fun value x -> let y = x.Clone() in y.AlignWithParent <- value; y)

    static member Cap_ : Lens<BrushStrategySolid, BrushCap> = 
        (fun x -> x.Cap), (fun value x -> let y = x.Clone() in y.Cap <- value; y)

    override this.GetBrushFor(anAgent) =
        BrushSolid(Cap=this.Cap, AlignWithParent=this.AlignWithParent) :> Brush

    
type BrushStrategySolidJoint() = 
    inherit BrushStrategy()
    member val JointType : BrushJoint = BrushJoint.Miter with get, set

    member this.Clone() = this.MemberwiseClone() :?> BrushStrategySolidJoint

    static member JointType_ : Lens<BrushStrategySolidJoint, BrushJoint> = 
        (fun x -> x.JointType), (fun value x -> let y = x.Clone() in y.JointType <- value; y)

    override this.GetBrushFor(anAgent) =
        BrushSolidJoint(JointType=this.JointType) :> Brush
 

 type BrushStrategy with
    static member Default = BrushStrategyDefault.Instance
 

// collider strategies
[<AbstractClass>]
type ColliderStrategy() =
    abstract member Evaluate : #IDworkinAgentState * #IColliderAttributes -> list<Collider>


type ColliderStrategyCircles() = 
    inherit ColliderStrategy()
    member val Density = 1.0f with get, set
    member val Offset = 0.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> ColliderStrategyCircles

    static member Density_ : Lens<ColliderStrategyCircles, float32> = 
        (fun x -> x.Density), (fun value x -> let y = x.Clone() in y.Density <- value; y)

    static member Offset_ : Lens<ColliderStrategyCircles, float32> = 
        (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)

    override this.Evaluate(agent, attributes) =
        if agent.DistanceCovered >= this.Offset then
            match attributes.LastCollider.Get() with
            | Some(collider) -> 
                let r = agent.Thickness * 0.5f
                let minR = agent.ThicknessFactor * 0.01f
                if r < minR then
                    []
                else
                    let mutable lastCollider = collider :?> CircleCollider
                    let mutable delta = 0.0f
                    let mutable dist = 0.0f
                    let inline criterion () = 
                        delta <- (r + lastCollider.Radius) / this.Density
                        dist <- agent.Position.DistanceTo(lastCollider.Center)
                        (dist - delta) > 0.0f
                    let mutable result : Collider list = []
                    while criterion() do
                        let pos = lerp lastCollider.Center agent.Position (delta / dist)
                        lastCollider <- CircleCollider(Some(agent.GetFootprint()), pos, r)
                        result <- (lastCollider :> Collider) :: result
                    if not <| List.isEmpty result then
                        attributes.LastCollider.Set(lastCollider)
                    result
            | None -> 
                let collider = CircleCollider(Some(agent.GetFootprint()), agent.Position, 0.0f)
                attributes.LastCollider.Set(collider)
                [ collider ]
        else
            []
    

type ColliderStrategySegments() = 
    inherit ColliderStrategy()
    member val MinLength = 1.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> ColliderStrategySegments

    static member MinLength_ : Lens<ColliderStrategySegments, float32> = 
        (fun x -> x.MinLength), (fun value x -> let y = x.Clone() in y.MinLength <- value; y)

    override this.Evaluate(agent, attributes) =
        let pos = agent.Position
        match attributes.LastCollider.Get() with
        | Some(collider) -> 
            let mutable lastCollider = collider :?> SegmentCollider
            if lastCollider.End.SquaredDistanceTo(pos) >= (squared (this.MinLength * agent.LengthFactor)) then
                lastCollider <- SegmentCollider(Some(agent.GetFootprint()), lastCollider.End, pos)
                attributes.LastCollider.Set(lastCollider)
                [ lastCollider ]
            else []
        | None -> 
            let collider = SegmentCollider(Some(agent.GetFootprint()), pos, pos)
            attributes.LastCollider.Set(collider)
            [ collider ]
    

type ColliderStrategy with
    static member Default = ColliderStrategyCircles() :> ColliderStrategy


// collider options
type ColliderOptionsNone() =
    inherit ColliderOptions()
    static member val Instance = ColliderOptionsNone()
    override this.Evaluate(agent) = ()
    override this.Evaluate(agentState, attributes, environment) = ()


type ColliderOptionsGeneral() =
    inherit ColliderOptions()

    member val Layer = CollisionLayer.Default with get, set
    member val Strategy : ColliderStrategy = ColliderStrategy.Default with get, set

    member this.Clone() = this.MemberwiseClone() :?> ColliderOptionsGeneral

    static member Layer_ : Lens<ColliderOptionsGeneral, CollisionLayer> = 
        (fun x -> x.Layer), (fun value x -> let y = x.Clone() in y.Layer <- value; y)

    static member Strategy_ : Lens<ColliderOptionsGeneral, ColliderStrategy> = 
        (fun x -> x.Strategy), (fun value x -> let y = x.Clone() in y.Strategy <- value; y)
    
    override this.Evaluate(agentState, attributes, environment) =
        for collider in this.Strategy.Evaluate(agentState, attributes) do
            environment.AddColliderToLayer(collider, this.Layer)


type ColliderOptions with
    static member Default = ColliderOptionsNone.Instance :> ColliderOptions


// speed strategies
type AgentSpeedAdaptive() = 
    inherit AgentSpeed()
    static member val Instance = AgentSpeedAdaptive()
    override this.Evaluate(agent) =
        match agent.Curvature with
        | 0.0f -> 1.0f
        | c -> sqrt (max 0.01f (1.0f - (abs (c / Single.Pi * 4.0f))))
    

type AgentSpeedConstant() = 
    inherit AgentSpeed()
    member val Magnitude = 1.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> AgentSpeedConstant

    static member Magnitude_ : Lens<AgentSpeedConstant, float32> = 
        (fun x -> x.Magnitude), (fun value x -> let y = x.Clone() in y.Magnitude <- value; y)

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

    static member Factor_ : Lens<AgentSpeedThickness, float32> = 
        (fun x -> x.Factor), (fun value x -> let y = x.Clone() in y.Factor <- value; y)

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
    

type PlaceBranchMark =
    | Always
    | OnSpawn


type BranchingCriterionBased() = 
    inherit BranchingStrategy()
    member val Criterion : BranchingCriterion = BranchingCriterion.Default with get, set
    member val PlaceBranchMark = PlaceBranchMark.Always with get, set
    member val SpawnConfig : SpawnConfig = SpawnConfig.Default with get, set
    member val Delay = 0 with get, set

    member this.Clone() = this.MemberwiseClone() :?> BranchingCriterionBased

    static member Criterion_ : Lens<BranchingCriterionBased, BranchingCriterion> = 
        (fun x -> x.Criterion), (fun value x -> let y = x.Clone() in y.Criterion <- value; y)
    
    static member PlaceBranchMark_ : Lens<BranchingCriterionBased, PlaceBranchMark> = 
        (fun x -> x.PlaceBranchMark), (fun value x -> let y = x.Clone() in y.PlaceBranchMark <- value; y)

    static member SpawnConfig_ : Lens<BranchingCriterionBased, SpawnConfig> = 
        (fun x -> x.SpawnConfig), (fun value x -> let y = x.Clone() in y.SpawnConfig <- value; y)
    
    static member Delay_ : Lens<BranchingCriterionBased, int> = 
        (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
    
    member private this.PerformBranchingFor(agent) =
        match this.PlaceBranchMark with
        | Always -> this.SpawnConfig.Evaluate(agent) // Branch mark already added in Evaluate() method
        | OnSpawn ->
            let initialAgentsCount = agent.DworkinEnvironment.AgentsCount
            this.SpawnConfig.Evaluate(agent)
            if agent.DworkinEnvironment.AgentsCount > initialAgentsCount then
                agent.AddBranchMarkFor(BranchMark(agent.DistanceCovered), this.Criterion.Tag)
    
    override this.Evaluate(agent) =
        let alwaysPlaceBranchMark = this.PlaceBranchMark = Always
        if alwaysPlaceBranchMark && not <| this.Criterion.Evaluate(agent) then 
            ()
        else
            if alwaysPlaceBranchMark && this.Criterion.Evaluate(agent) then
                agent.AddBranchMarkFor(BranchMark(agent.DistanceCovered), this.Criterion.Tag)
            
            if this.Delay <= 0 then
                if alwaysPlaceBranchMark || this.Criterion.Evaluate(agent) then
                    this.PerformBranchingFor(agent)
            else
                let agentSnapshot = agent.Copy()
                agent.DworkinEnvironment.Delay(
                    (fun _ -> 
                        if alwaysPlaceBranchMark || this.Criterion.Evaluate(agentSnapshot) then
                            this.PerformBranchingFor(agentSnapshot)
                        None), 
                    this.Delay)


type DworkinBranchingMultivariant() = 
    inherit BranchingStrategy()
    member val Strategies : BranchingStrategy[] = [||] with get, set

    member this.Clone() = this.MemberwiseClone() :?> DworkinBranchingMultivariant

    static member Strategies_ : Lens<DworkinBranchingMultivariant, BranchingStrategy[]> = 
        (fun x -> x.Strategies), (fun value x -> let y = x.Clone() in y.Strategies <- value; y)

    override this.Evaluate(agent) =
        for each in this.Strategies do each.Evaluate(agent)
    

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

    static member SpawnConfig_ : Lens<TerminationSpawn, SpawnConfig> = 
        (fun x -> x.SpawnConfig), (fun value x -> let y = x.Clone() in y.SpawnConfig <- value; y)

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


// Dworkin environment configs
[<AbstractClass>]
type DworkinEnvironmentConfig() =
    member val RandomSeed = 42 with get, set

    interface IDworkinEnvironmentConfig with
        member this.GetEnvironment(size) =
            let env = DworkinEnvironment(Rectangle(Point.Zero, size), this.RandomSeed)
            this.Evaluate(env)
            env
    
    abstract member Evaluate : DworkinEnvironment -> unit


[<AbstractClass>]
type DworkinEnvironmentConfigCustom() =
    inherit DworkinEnvironmentConfig()
    

type DworkinEnvironmentConfigSpawn() =
    inherit DworkinEnvironmentConfig()

    static let throwAwayAgentConfig = { DworkinAgentConfig.Default with Brush=BrushStrategyNone(); TotalDistance=0.0f }

    member val SpawnConfig = SpawnConfig.Default with get, set

    override this.Evaluate(environment) = 
        let opts = 
            AgentSpawnOptions(
                Position=environment.Bounds.Center+Point(0.5f, 0.5f), 
                Direction=Point(0.0f, -1.0f), 
                Seed=Some(environment.Random.Next()))
        let tempAgent = environment.Spawn(throwAwayAgentConfig, opts )
        this.SpawnConfig.Evaluate(tempAgent :?> DworkinAgent)
        environment.RemoveAgent(tempAgent) |> ignore


type DworkinEnvironmentConfigSingle() =
    inherit DworkinEnvironmentConfig()

    member val AgentConfig = DworkinAgentConfig.Default with get, set

    override this.Evaluate(environment) =
        let opts = AgentSpawnOptions(Position=environment.Bounds.Center+Point(0.5f, 0.5f), Direction=Point(0.0f, -1.0f))
        environment.Spawn(this.AgentConfig, opts) |> ignore
 

 // extensions to DworkinAgent and DworkinEnvironment that rely on SpawnConfig
 type DworkinAgent with    
    member this.Spawn(spawnConfig : SpawnConfig, options : AgentSpawnOptions, agentBlock : DworkinAgent -> unit) =
        if this.Generation < this.GenerationLimit then
            options.Parent <- Some(this)
            options.Config <- Some({ DworkinAgentConfig.Default with TotalDistance=0.0f })
            let spawnAgent = this.SpawnUsingOptions(options) :?> DworkinAgent
            spawnAgent |> performAction (fun agent -> agentBlock agent; spawnConfig.Evaluate(agent))
    
    member this.Spawn(spawnConfig : SpawnConfig, options : AgentSpawnOptions) =
        this.Spawn(spawnConfig, options, ignore)


type DworkinEnvironment with
    member this.Spawn(spawnConfig : SpawnConfig, options : AgentSpawnOptions, agentBlock : DworkinAgent -> unit) =
        options.Config <- Some({ DworkinAgentConfig.Default with TotalDistance=0.0f })
        let spawnAgent = this.SpawnUsingOptions(options)
        spawnAgent |> performAction (fun agent -> agentBlock agent; spawnConfig.Evaluate(agent))