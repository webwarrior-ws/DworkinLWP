namespace FlowingLeaves


open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Geometry
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.Lenses
open LibDworkin.SpecializedCollections
open LibDworkin.PharoCompatibility
open LibDworkin.Probes
open LibDworkin.SimplexNoise

open MiscConfigs


module SpawnConfig =
    type FlowingLeavesVarSize() = 
         inherit SpawnConfig()
         member val GenLimit = 15 with get, set
         member val Angle = -35.0f with get, set
         member val Offset = 4.0f with get, set
         member val CurvRandRange = NumberRange(0.8f, 1.2f) with get, set
         member val Steps = 3 with get, set
         member val MaxLength = 80.0f with get, set
         member val MinLength = 20.0f with get, set
         member val Delay = 50 with get, set
         member val ScaleRange = NumberRange(1.0f, 2.0f) with get, set
         member val BaseSpeed = 1.0f with get, set

         member this.Clone() = this.MemberwiseClone() :?> FlowingLeavesVarSize

         static member GenLimit_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.GenLimit), (fun value x -> let y = x.Clone() in y.GenLimit <- value; y)
         static member Angle_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
         static member Offset_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
         static member CurvRandRange_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.CurvRandRange), (fun value x -> let y = x.Clone() in y.CurvRandRange <- value; y)
         static member Steps_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.Steps), (fun value x -> let y = x.Clone() in y.Steps <- value; y)
         static member MaxLength_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.MaxLength), (fun value x -> let y = x.Clone() in y.MaxLength <- value; y)
         static member MinLength_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.MinLength), (fun value x -> let y = x.Clone() in y.MinLength <- value; y)
         static member Delay_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
         static member ScaleRange_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.ScaleRange), (fun value x -> let y = x.Clone() in y.ScaleRange <- value; y)
         static member BaseSpeed_ : Lens<FlowingLeavesVarSize, _> = 
             (fun x -> x.BaseSpeed), (fun value x -> let y = x.Clone() in y.BaseSpeed <- value; y)

         override this.Evaluate(agent, pos) =
             agent.GenerationLimit <- this.GenLimit
             seq { 
                 for phi in [| this.Angle; -this.Angle; |] do
                     let offset = this.Offset + (agent.Thickness * 0.5f)
                     let scales = this.ScaleRange.By(this.ScaleRange.Extent / (float32 this.Steps)) |> Seq.toArray
                     let curvatures = this.CurvRandRange.By(this.CurvRandRange.Extent / (float32 this.Steps)) |> Seq.toArray
                     curvatures.ShuffleBy(agent.Random)
                     scales.ShuffleBy(agent.Random)
                     for scale in scales do 
                         for c in curvatures do 
                             let agentBlock = 
                                 fun (a : DworkinAgent) -> 
                                     a.Flip()
                                     a.ThicknessFactor <- scale
                                     a.LengthFactor <- scale
                             let mutable l = this.MaxLength
                             while l >= this.MinLength do
                                 let config = 
                                     agent.Config
                                     |> Optic.set totalDistance_ l
                                     |> Optic.map (curvature_ >-> cast<_, CurvatureFormula.Quadratic> >-> c_) (fun x -> x * c)
                                     |> Optic.set (speed_ >-> magnitude_) (this.BaseSpeed / scale)
                                 let probeData = 
                                     agent.LaunchProbe(
                                         config, 
                                         ProbeSpawnOptions(AngleDeg=phi, OffsetSideways=offset, StepsBetweenCollisionCheck=2),
                                         agentBlock)
                                 if probeData.ReachedEnd then
                                     probeData.SpawnColliders()
                                     let spawnOptions = AgentSpawnOptions(AngleDeg=phi, OffsetSideways=offset)
                                     spawnOptions.DelayBy(agent.Random.NextFloat() * (float32 this.Delay) |> int)
                                     yield agent.Spawn(config, 
                                                       spawnOptions, 
                                                       fun a -> 
                                                           agentBlock a
                                                           a.NoCollisions <- true)
                                 else
                                     l <- probeData.S
                 }
             |> Seq.tryHead
             |> ignore

module DworkinAgentConfig = 
    let flowingLeavesVarSize = 
        { DworkinAgentConfig.Default with 
            Branching=
                BranchingCriterionBased(
                    Criterion=BranchingCriterionIntervalRandomized.Uniform(IntervalRange=NumberRange(5.0f, 15.0f)),
                    SpawnConfig=SpawnConfig.FlowingLeavesVarSize(MaxLength=50.0f)
                );
            Brush=BrushStrategySolidSmoother();
            ColliderOptions=ColliderOptionsGeneral()
                 |> Optic.set (strategy_ >-> density_) 1.5f;
            Curvature=CurvatureFormula.Quadratic(A= -1.136f, C= -0.02f, B=0.68f, K=0.75f);
            TerminationConditions=
                { Collision=Some(TerminationConditionCollision(SelfIntersection=false)
                              |> Optic.set (primaryDetector_ >-> angle_) 120.0f
                              |> Optic.set (primaryDetector_ >-> distance_) 2.0f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance()); };
            Thickness=ThicknessFormula.LeafPower_Base(A=2.5f, P=0.75f, C=5.0f, Base=0.0f);
            Speed=AgentSpeedConstant();
            TotalDistance=35.0f
         }


type FlowingLeaves_WIP() = 
    inherit DworkinEnvironmentConfigCustom()
    
    member val AgentConfig = DworkinAgentConfig.flowingLeavesVarSize with get, set
    member val N = 20 with get, set
    member val Margin = 20.0f with get, set
    member val MaxIterations = 1000 with get, set
    member val Delay = 200 with get, set
    member val AngleSpread = 1.0f with get, set
    member val Density = 1.0f with get, set
    member val Scale = 2.0f with get, set
    member val ScaleVariance = 0.5f with get, set
    member val Curviness = 0.0f with get, set
    member val LeafThickness = 10.0f with get, set
    member val LeafLength = 90.0f with get, set
    member val LeafShape = 0.65f with get, set
    member val Speed = 1.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> FlowingLeaves_WIP

    static member AgentConfig_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member N_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.N), (fun value x -> let y = x.Clone() in y.N <- value; y)
    static member Margin_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.Margin), (fun value x -> let y = x.Clone() in y.Margin <- value; y)
    static member MaxIterations_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.MaxIterations), (fun value x -> let y = x.Clone() in y.MaxIterations <- value; y)
    static member Delay_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
    static member AngleSpread_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.AngleSpread), (fun value x -> let y = x.Clone() in y.AngleSpread <- value; y)
    static member Density_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.Density), (fun value x -> let y = x.Clone() in y.Density <- value; y)
    static member Scale_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.Scale), (fun value x -> let y = x.Clone() in y.Scale <- value; y)
    static member ScaleVariance_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.ScaleVariance), (fun value x -> let y = x.Clone() in y.ScaleVariance <- value; y)
    static member Curviness_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.Curviness), (fun value x -> let y = x.Clone() in y.Curviness <- value; y)
    static member LeafThickness_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.LeafThickness), (fun value x -> let y = x.Clone() in y.LeafThickness <- value; y)
    static member LeafLength_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.LeafLength), (fun value x -> let y = x.Clone() in y.LeafLength <- value; y)
    static member LeafShape_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.LeafShape), (fun value x -> let y = x.Clone() in y.LeafShape <- value; y)
    static member Speed_ : Lens<FlowingLeaves_WIP, _> = 
        (fun x -> x.Speed), (fun value x -> let y = x.Clone() in y.Speed <- value; y)

    override this.Evaluate(env) =
        let mutable currN = 0
        let mutable i = 0
        let config = 
            let branching_ = branching_ >-> cast<_, BranchingCriterionBased>
            let branching_spawnConfig_ = branching_ >-> spawnConfig_ >-> cast<_, SpawnConfig.FlowingLeavesVarSize>
            let curvature_ = curvature_ >-> cast<_, CurvatureFormula.Quadratic>
            let thickness_ = thickness_ >-> cast<_, ThicknessFormula.LeafPower_Base>
            this.AgentConfig
            |> Optic.map (branching_spawnConfig_ >-> offset_) (fun x -> x / this.Density)
            |> Optic.map (terminationConditions_ >-> collision_ >-> primaryDetector_ >-> distance_) (fun x -> x / this.Density * sqrt (10.0f / this.LeafThickness))
            |> Optic.map (branching_ >-> criterion_ >-> cast<_, BranchingCriterionIntervalRandomized.Uniform> >-> intervalRange_) (fun x -> x * (1.0f / this.Density))
            |> Optic.set (branching_spawnConfig_ >-> scaleRange_) (NumberRange(lerp 1.2f 0.6f this.ScaleVariance, lerp 1.2f 2.4f this.ScaleVariance) * this.Scale)
            |> Optic.map (branching_spawnConfig_ >-> delay_) (fun x -> float32(x) * this.Density |> int)
            |> Optic.set (branching_spawnConfig_ >-> baseSpeed_) (this.Speed * this.Scale)
            |> Optic.set (speed_ >-> magnitude_) this.Speed
            |> Optic.set (curvature_ >-> a_) (lerp (-1.36f) (-1.136f) this.Curviness)
            |> Optic.set (curvature_ >-> c_) (lerp (-0.04f) (-0.02f) this.Curviness)
            |> Optic.set (thickness_ >-> c_) this.LeafThickness
            |> Optic.set (branching_spawnConfig_ >-> maxLength_) this.LeafLength
            |> Optic.set (thickness_ >-> p_) this.LeafShape
        while (currN < this.N) && (i < this.MaxIterations) do
            let p = env.Random.NextInRectangle(env.Bounds.InsetBy(this.Margin))
            let angle = (((p / (max env.Bounds.Width env.Bounds.Height) * (sqrt 2.0f)).SNoise()) * Single.Pi) * this.AngleSpread
            let spawnOptions = ProbeSpawnOptions(Position=p, Angle=angle)
            let agentBlock = fun (a : DworkinAgent) -> a.ScaleBy(this.Scale)
            let probeData = env.LaunchProbe(config, spawnOptions, agentBlock)
            if Option.isNone probeData.Collision then
                probeData.SpawnColliders()
                if this.Delay > 0 then
                    spawnOptions.DelayBy(env.Random.NextFloat() * (float32 this.Delay) |> int)
                env.Spawn(config, 
                          spawnOptions,
                          fun a -> 
                              agentBlock a
                              a.NoCollisions <- true) |> ignore
                currN <- currN + 1
            i <- i + 1
