module rec MiscConfigs
/// Miscellaneous configs that may have recursive dependencies


open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Geometry
open LibDworkin.AgentAbstractions
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.Lenses
open LibDworkin.SpecializedCollections
open LibDworkin.PharoCompatibility
open LibDworkin.Colliders
open LibDworkin.Probes
open LibDworkin.Brushes


module CollisionHandler =
    type IgnoreSiblingUpTo() = 
        inherit CollisionHandler()
        member val MinT = 0.1f with get, set

        member this.Clone() = this.MemberwiseClone() :?> IgnoreSiblingUpTo

        static member MinT_ : Lens<IgnoreSiblingUpTo, _> = 
            (fun x -> x.MinT), (fun value x -> let y = x.Clone() in y.MinT <- value; y)

        override this.Evaluate(agent, collision) =
            (match agent.Parent, collision.Obstacle.Parent with
            | None, None -> false
            | Some(p1), Some(p2) -> 
                match p2.Parent with
                | None -> true
                | Some(gp2) -> p1 :> IDworkinAgentState <> gp2
            | _ -> true) || (agent.T >= this.MinT)
    


module BranchingCriterionCustom =
    type IntervalBoundedTRand() = 
        inherit BranchingCriterionCustom()
        member val MinRange = NumberRange(0.2f, 0.5f) with get, set
        member val MaxRange = NumberRange(0.5f, 0.8f) with get, set
        member val D = 10.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> IntervalBoundedTRand

        static member MinRange_ : Lens<IntervalBoundedTRand, _> = 
            (fun x -> x.MinRange), (fun value x -> let y = x.Clone() in y.MinRange <- value; y)
        static member MaxRange_ : Lens<IntervalBoundedTRand, _> = 
            (fun x -> x.MaxRange), (fun value x -> let y = x.Clone() in y.MaxRange <- value; y)
        static member D_ : Lens<IntervalBoundedTRand, _> = 
            (fun x -> x.D), (fun value x -> let y = x.Clone() in y.D <- value; y)

        override this.Evaluate(agent, mark, tag) =
            let tInterval = 
                agent.Attributes.BranchInterval.GetOrSetIfAbsent(
                    fun () -> NumberRange(agent.Random.NextInRange(this.MinRange), agent.Random.NextInRange(this.MaxRange)))
            ((mark.S + this.D) < agent.S) && (tInterval.Contains agent.T)
    

    type Interval_MaxBranches() = 
        inherit BranchingCriterionCustom()
        member val D = 18.0f with get, set
        member val N = 2 with get, set
        
        member this.Clone() = this.MemberwiseClone() :?> Interval_MaxBranches
        
        static member D_ : Lens<Interval_MaxBranches, _> = 
            (fun x -> x.D), (fun value x -> let y = x.Clone() in y.D <- value; y)
        static member N_ : Lens<Interval_MaxBranches, _> = 
            (fun x -> x.N), (fun value x -> let y = x.Clone() in y.N <- value; y)
        
        override this.Evaluate(agent, mark, tag) =
            ((agent.NumBranchesFor(tag)) < this.N) && ((mark.S + this.D) < agent.S)


    type RandEachStepMinInt() = 
        inherit BranchingCriterionCustom()
        member val P = 0.125f with get, set
        member val MinInterval = 2.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> RandEachStepMinInt

        static member P_ : Lens<RandEachStepMinInt, _> = 
            (fun x -> x.P), (fun value x -> let y = x.Clone() in y.P <- value; y)
        static member MinInterval_ : Lens<RandEachStepMinInt, _> = 
            (fun x -> x.MinInterval), (fun value x -> let y = x.Clone() in y.MinInterval <- value; y)

        override this.Evaluate(agent, mark, tag) =
            (agent.Random.NextFloat() < (1.0f - ((1.0f - this.P) ** (agent.Speed / agent.LengthFactor)))) && ((agent.S - mark.S) >= this.MinInterval)



module BranchingCriterionInterval =
    type ConstantBoundedT() = 
        inherit BranchingCriterionInterval()
        member val D = 10.0f with get, set
        member val Start = 0.2f with get, set
        member val End = 0.8f with get, set
    
        member this.Clone() = this.MemberwiseClone() :?> ConstantBoundedT
    
        static member D_ : Lens<ConstantBoundedT, _> = 
            (fun x -> x.D), (fun value x -> let y = x.Clone() in y.D <- value; y)
        static member Start_ : Lens<ConstantBoundedT, _> = 
            (fun x -> x.Start), (fun value x -> let y = x.Clone() in y.Start <- value; y)
        static member End_ : Lens<ConstantBoundedT, _> = 
            (fun x -> x.End), (fun value x -> let y = x.Clone() in y.End <- value; y)
    
        override this.Evaluate(s, t) =
            if between this.Start this.End t then this.D else 1000000.0f


module BranchingCriterionIntervalRandomized =
    type Uniform() = 
        inherit BranchingCriterionIntervalRandomized()
        member val IntervalRange = NumberRange(10.0f, 30.0f) with get, set

        member this.Clone() = this.MemberwiseClone() :?> Uniform

        static member IntervalRange_ : Lens<Uniform, _> = 
            (fun x -> x.IntervalRange), (fun value x -> let y = x.Clone() in y.IntervalRange <- value; y)

        override this.Evaluate(s, t, random) =
            random.NextInRange(this.IntervalRange)
    

    type UniformBounded() = 
        inherit BranchingCriterionIntervalRandomized()
        member val IntervalRange = NumberRange(10.0f, 60.0f) with get, set
        member val Margin = 5.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> UniformBounded

        static member IntervalRange_ : Lens<UniformBounded, _> = 
            (fun x -> x.IntervalRange), (fun value x -> let y = x.Clone() in y.IntervalRange <- value; y)
        static member Margin_ : Lens<UniformBounded, _> = 
            (fun x -> x.Margin), (fun value x -> let y = x.Clone() in y.Margin <- value; y)

        override this.Evaluate(s, t, random) =
            let x = random.NextInRange(this.IntervalRange)
            if (s + x) > ((s / t) - this.Margin) then 100000.0f else x

    
    type Gaussian() = 
        inherit BranchingCriterionIntervalRandomized()
        member val Mu = 20.0f with get, set
        member val Sigma = 5.0f with get, set
        
        member this.Clone() = this.MemberwiseClone() :?> Gaussian
        
        static member Mu_ : Lens<Gaussian, _> = 
            (fun x -> x.Mu), (fun value x -> let y = x.Clone() in y.Mu <- value; y)
        static member Sigma_ : Lens<Gaussian, _> = 
            (fun x -> x.Sigma), (fun value x -> let y = x.Clone() in y.Sigma <- value; y)
        
        override this.Evaluate(s, t, random) =
            random.NextGaussian(this.Mu, this.Sigma)
    
 

module DworkinAgentConfig =
    let swirls2_7_fast_ = 
        { DworkinAgentConfig.Default with 
            Branching=
                BranchingCriterionBased(
                    Criterion=
                        (BranchingCriterionCustom.RandEachStepMinInt()
                            |> Optic.set p_ 0.075f
                            |> Optic.set minInterval_ 15.0f),
                    SpawnConfig=
                        (SpawnConfig.Probe1BranchRecByLen()
                            |> Optic.set angle_ -15.0f
                            |> Optic.set genLimit_ 20
                            |> Optic.set lengthRange_ (NumberRange(50.0f, 300.0f))
                            |> Optic.set lengthSteps_ 3));
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.Tan_t_() |> Optic.set c_ 0.0315f;
            Speed=AgentSpeedConstant();
            TerminationConditions=
                { Collision=Some(TerminationConditionCollision()
                              |> Optic.set minDistance_ 7.5f
                              |> Optic.set (primaryDetector_ >-> angle_) 60.0f
                              |> Optic.set (primaryDetector_ >-> distance_) 3.0f
                              |> Optic.set selfIntersection_ false);
                Curvature=None;
                Distance=Some(TerminationConditionDistance()); };
            Thickness=
                ThicknessFormula.ConcaveParabolic()
                    |> Optic.set c_ 2.0f
                    |> Optic.set alpha_ 0.4f;
            TotalDistance=200.0f
         }
    
    let leafWithStem = 
        { DworkinAgentConfig.Default with 
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.Linear(A=0.069f,B=0.085f);
            TerminationConditions={ 
                Collision=Some(
                    TerminationConditionCollision(MinDistance=2.5f)
                    |> Optic.set (primaryDetector_ >-> angle_) 120.0f
                    |> Optic.set (primaryDetector_ >-> distance_) 1.5f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance())
             };
            Thickness=ThicknessFormula.LeafPowerWithStem(P=0.33f,C=5.0f,St=0.1f);
            TotalDistance=12.0f
         }



module SpawnConfig =
    open LibDworkin.SpatialIndex
    open LibDworkin.Agents

    type CircleAdvanced() = 
        inherit SpawnConfig()
        member val Radius = 3.0f with get, set
        member val Offset = 0.5f with get, set
        member val HollowPart = 0.5f with get, set
        member val InsidePart = 0.0f with get, set
        member val SpawnCollider = true with get, set
        member val Proximity = 2.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> CircleAdvanced

        static member Radius_ : Lens<CircleAdvanced, _> = 
            (fun x -> x.Radius), (fun value x -> let y = x.Clone() in y.Radius <- value; y)
        static member Offset_ : Lens<CircleAdvanced, _> = 
            (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
        static member HollowPart_ : Lens<CircleAdvanced, _> = 
            (fun x -> x.HollowPart), (fun value x -> let y = x.Clone() in y.HollowPart <- value; y)
        static member InsidePart_ : Lens<CircleAdvanced, _> = 
            (fun x -> x.InsidePart), (fun value x -> let y = x.Clone() in y.InsidePart <- value; y)
        static member SpawnCollider_ : Lens<CircleAdvanced, _> = 
            (fun x -> x.SpawnCollider), (fun value x -> let y = x.Clone() in y.SpawnCollider <- value; y)
        static member Proximity_ : Lens<CircleAdvanced, _> = 
            (fun x -> x.Proximity), (fun value x -> let y = x.Clone() in y.Proximity <- value; y)

        override this.Evaluate(agent, pos) =
            let r = (agent.Thickness * this.Radius) / agent.ThicknessFactor
            let collider = 
                CircleCollider(
                    Some(agent :> IDworkinAgentState), 
                    pos + ((agent.Direction * r) * (this.Offset - 0.5f)), 
                    ((r * 0.5f) * this.Proximity) * agent.ThicknessFactor)
            let exit =
                let predicate (layer : CollisionLayer) (each : Collider) =
                    (each.Parent <> Some(agent :> IDworkinAgentState)) && ((box agent.Parent) <> (box each.Parent)) && (collide each collider)
                agent.DworkinEnvironment.QueryColliders(
                    (collider :> IHasBounds).Bounds, 
                    CollisionLayers.All, 
                    predicate)
            
            if not exit then
                let offset = r * (this.Offset - 0.5f)
                if this.HollowPart > 0.0f then
                    let scaleFactor = (1.0f + this.HollowPart) * 0.5f
                    let strokeWidth = (1.0f - this.HollowPart) * 0.5f
                    agent.SpawnShape(
                        Shape.PrimitiveCircle, 
                        ShapeSpawnOptions(Scale=r*scaleFactor, Offset=offset, Paint={ Fill=false; StrokeWidth=strokeWidth/scaleFactor }))
                else
                    agent.SpawnShape(Shape.PrimitiveCircle, ShapeSpawnOptions(Scale=r, Offset=offset))

                if this.InsidePart > 0.0f then
                    agent.SpawnShape(Shape.PrimitiveCircle, ShapeSpawnOptions(Scale=(r * this.HollowPart) * this.InsidePart, Offset=offset))
            
                if this.SpawnCollider then
                    let layer = 
                        match agent.Config.ColliderOptions with
                        | :? ColliderOptionsGeneral as co -> co.Layer
                        | _ -> CollisionLayer.Default
                    agent.DworkinEnvironment.AddColliderToLayer(CircleCollider(collider.Parent, collider.Center, (r*0.5f) * agent.ThicknessFactor), layer)
    
    
    type Probe1BranchRecByLen() = 
        inherit SpawnConfig()
        member val Angle = 0.0f with get, set
        member val GenLimit = 12 with get, set
        member val LengthRange = NumberRange(100.0f, 500.0f) with get, set
        member val LengthSteps = 6 with get, set

        member this.Clone() = this.MemberwiseClone() :?> Probe1BranchRecByLen

        static member Angle_ : Lens<Probe1BranchRecByLen, float32> = 
            (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
        static member GenLimit_ : Lens<Probe1BranchRecByLen, _> = 
            (fun x -> x.GenLimit), (fun value x -> let y = x.Clone() in y.GenLimit <- value; y)
        static member LengthRange_ : Lens<Probe1BranchRecByLen, _> = 
            (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
        static member LengthSteps_ : Lens<Probe1BranchRecByLen, _> = 
            (fun x -> x.LengthSteps), (fun value x -> let y = x.Clone() in y.LengthSteps <- value; y)

        override this.Evaluate(agent, pos) =
            let agentBlock = fun (a : DworkinAgent) -> a.Flip()
            agent.GenerationLimit <- this.GenLimit
            this.LengthRange.By(this.LengthRange.Extent / float32(this.LengthSteps))
                |> Seq.rev
                |> Seq.exists
                (fun length -> 
                    let config = agent.Config|> Optic.set totalDistance_ length
                    let probeData = 
                        agent.LaunchProbe(
                            config, 
                            ProbeSpawnOptions(AngleDeg=this.Angle, StepsBetweenCollisionCheck=4), 
                            agentBlock)
                    if probeData.Collision.IsNone then
                        agent.Spawn(
                            config, 
                            AgentSpawnOptions(AngleDeg=this.Angle), 
                            fun a -> 
                                agentBlock a
                                a.NoCollisions <- true
                                probeData.SpawnColliders())
                        true
                    else false)
                 |> ignore
    
    module ProbeBunchMagneticSpirals2Branch3 = 
        module Config = 
            module Branching = 
                type SpawnConfig() = 
                    inherit LibDworkin.Configs.SpawnConfig()
                    member val Angle = 0.0f with get, set
                    member val GenLimit = 5 with get, set
                    member val LengthRange = NumberRange(100.0f, 400.0f) with get, set
                    member val LengthSteps = 4 with get, set
                    member val AlphaRange = NumberRange(0.65f, 0.8f) with get, set
                    member val AlphaSteps = 4 with get, set
                
                    member this.Clone() = this.MemberwiseClone() :?> SpawnConfig
                
                    static member Angle_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
                    static member GenLimit_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.GenLimit), (fun value x -> let y = x.Clone() in y.GenLimit <- value; y)
                    static member LengthRange_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
                    static member LengthSteps_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.LengthSteps), (fun value x -> let y = x.Clone() in y.LengthSteps <- value; y)
                    static member AlphaRange_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.AlphaRange), (fun value x -> let y = x.Clone() in y.AlphaRange <- value; y)
                    static member AlphaSteps_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.AlphaSteps), (fun value x -> let y = x.Clone() in y.AlphaSteps <- value; y)
                
                    override this.Evaluate(agent, pos) =
                        let agentBlock = fun (a : DworkinAgent) -> a.Flip()
                        agent.GenerationLimit <- this.GenLimit
                        let lengthRange = this.LengthRange.By(this.LengthRange.Extent / float32(this.LengthSteps)) |> Seq.toArray
                        lengthRange.ShuffleBy(agent.Random)
                        lengthRange 
                            |> Array.rev 
                            |> Array.exists
                                (fun length -> 
                                    let alphaRange = this.AlphaRange.By(this.AlphaRange.Extent / float32(this.AlphaSteps)) |> Seq.toArray
                                    alphaRange.ShuffleBy(agent.Random)
                                    alphaRange 
                                        |> Array.rev 
                                        |> Array.exists
                                            (fun alpha -> 
                                                let config = 
                                                    let curvature_ = curvature_ >-> cast<_, CurvatureFormula.MagneticWithFlip>
                                                    agent.Config
                                                    |> Optic.set totalDistance_ length
                                                    |> Optic.set (curvature_ >-> midpoint_) 0.0f
                                                    |> Optic.set (curvature_ >-> alpha_) alpha
                                                let probeData = 
                                                    agent.LaunchProbe(
                                                        config, 
                                                        ProbeSpawnOptions(AngleDeg=this.Angle, StepsBetweenCollisionCheck=4), 
                                                        agentBlock)
                                                match probeData.Collision with
                                                | Some(_) -> false
                                                | None -> 
                                                    (*return*) 
                                                    agent.Spawn(
                                                        config, 
                                                        AgentSpawnOptions(AngleDeg=this.Angle), 
                                                        fun a -> 
                                                            agentBlock a
                                                            a.NoCollisions <- true
                                                            probeData.SpawnColliders())
                                                    true)) |> ignore
                    
    
    type ProbeBunchMagneticSpirals2Branch3() = 
        inherit SpawnConfig()
        member val Angle = 30.0f with get, set
        member val Config = { DworkinAgentConfig.Default with 
                                Branching=
                                    BranchingCriterionBased(
                                        Criterion=(BranchingCriterionIntervalRandomized.UniformBounded() |> Optic.set intervalRange_ (NumberRange(20.0f, 40.0f))),
                                        SpawnConfig=ProbeBunchMagneticSpirals2Branch3.Config.Branching.SpawnConfig()
                                    );
                                ColliderOptions=ColliderOptionsGeneral();
                                Curvature=CurvatureFormula.MagneticWithFlip();
                                TerminationConditions=
                                    { Collision=Some(TerminationConditionCollision()
                                                  |> Optic.set handler_ (CollisionHandler.IgnoreSiblingUpTo() |> Optic.set minT_ 0.5f)
                                                  |> Optic.set minDistance_ 15.0f
                                                  |> Optic.set (primaryDetector_ >-> angle_) 90.0f
                                                  |> Optic.set (primaryDetector_ >-> distance_) 5.0f);
                                    Curvature=None;
                                    Distance=Some(TerminationConditionDistance()); };
                                Thickness=
                                    ThicknessFormula.LeafPower_Base()
                                        |> Optic.set a_ 2.0f
                                        |> Optic.set c_ 1.5f
                                        |> Optic.set base_ 0.5f
                            }
            with get, set
        member val MidpointInterval = Interval(0.1f, 0.5f, 0.05f) with get, set
        member val AlphaInterval = Interval(0.65f, 0.75f, 0.025f) with get, set
        member val LengthInterval = Interval(300.0f, 600.0f, 50.0f) with get, set
        member val MaxInitialAgents = 5 with get, set

        member this.Clone() = this.MemberwiseClone() :?> ProbeBunchMagneticSpirals2Branch3

        static member Angle_ : Lens<ProbeBunchMagneticSpirals2Branch3, _> = 
            (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
        static member Config_ : Lens<ProbeBunchMagneticSpirals2Branch3, _> = 
            (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
        static member MidpointInterval_ : Lens<ProbeBunchMagneticSpirals2Branch3, _> = 
            (fun x -> x.MidpointInterval), (fun value x -> let y = x.Clone() in y.MidpointInterval <- value; y)
        static member AlphaInterval_ : Lens<ProbeBunchMagneticSpirals2Branch3, _> = 
            (fun x -> x.AlphaInterval), (fun value x -> let y = x.Clone() in y.AlphaInterval <- value; y)
        static member LengthInterval_ : Lens<ProbeBunchMagneticSpirals2Branch3, _> = 
            (fun x -> x.LengthInterval), (fun value x -> let y = x.Clone() in y.LengthInterval <- value; y)
        static member MaxInitialAgents_ : Lens<ProbeBunchMagneticSpirals2Branch3, _> = 
            (fun x -> x.MaxInitialAgents), (fun value x -> let y = x.Clone() in y.MaxInitialAgents <- value; y)

        override this.Evaluate(agent, pos) =
            let variants = [|
                for x1 in this.MidpointInterval do
                    for x2 in this.AlphaInterval do
                        for x3 in this.LengthInterval do
                            for x4 in [|1|] do yield x1, x2, x3, x4 |]
            variants.ShuffleBy(agent.Random)
            let mutable n = 0
            variants |> Seq.takeWhile (
                fun (midpoint, alpha, length, flipFactor) -> 
                    let agentBlock = 
                        fun (a : DworkinAgent) -> 
                            if flipFactor < 0 then
                                a.Flip()
                    let conf = 
                        let curvature_ = curvature_ >-> cast<_, CurvatureFormula.MagneticWithFlip>
                        this.Config
                        |> Optic.set (curvature_ >-> midpoint_) midpoint
                        |> Optic.set (curvature_ >-> alpha_) alpha
                        |> Optic.set totalDistance_ length
                    let probeData = 
                        agent.LaunchProbe(
                            conf, 
                            ProbeSpawnOptions(
                                AngleDeg=this.Angle * float32(flipFactor), 
                                StepsBetweenCollisionCheck=int(this.Config.TerminationConditions.Collision.Value.PrimaryDetector.Distance)),
                            agentBlock)
                    if Option.isNone probeData.Collision then
                        probeData.SpawnColliders()
                        agent.Spawn(
                            conf, 
                            AgentSpawnOptions(AngleDeg=this.Angle * float32(flipFactor)), 
                            fun a -> 
                                agentBlock a
                                a.NoCollisions <- true)
                        n <- n + 1
                    n < this.MaxInitialAgents)
                    |> Seq.iter ignore


    type ProbeSplitRecursive() = 
        inherit SpawnConfig()
        member val GenLimit = 20 with get, set
        member val PBothSides = 0.0f with get, set
        member val LengthRange = NumberRange(40.0f, 80.0f) with get, set
        
        member this.Clone() = this.MemberwiseClone() :?> ProbeSplitRecursive
        
        static member GenLimit_ : Lens<ProbeSplitRecursive, _> = 
            (fun x -> x.GenLimit), (fun value x -> let y = x.Clone() in y.GenLimit <- value; y)
        static member PBothSides_ : Lens<ProbeSplitRecursive, _> = 
            (fun x -> x.PBothSides), (fun value x -> let y = x.Clone() in y.PBothSides <- value; y)
        static member LengthRange_ : Lens<ProbeSplitRecursive, _> = 
            (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
        
        override this.Evaluate(agent, pos) =
            agent.GenerationLimit <- this.GenLimit
            let mutable flipValues = [| true |]
            let mutable config = agent.Config
            if (agent.Random.NextFloat()) < this.PBothSides then 
                flipValues <- [| true; false |] 
            elif agent.Random.NextDouble() < 0.5 then 
                flipValues <- [| true |]
            Seq.iter 
                (fun flip ->
                    let agentBlock (a : DworkinAgent) = if flip then a.Flip()
                    config <- config |> Optic.set totalDistance_ (agent.Random.NextInRange(this.LengthRange))
                    let probeData = agent.LaunchProbe(config, ProbeSpawnOptions(), agentBlock)
                    if probeData.S >= (this.LengthRange.Min) then 
                        agent.Spawn(
                            config |> Optic.set totalDistance_ probeData.S, 
                            AgentSpawnOptions(), 
                            fun a ->
                                probeData.Id <- a.Id
                                probeData.SpawnColliders()
                                agentBlock a
                                a.NoCollisions <- true)) 
                flipValues
    
    
    type Probe2BranchByScale() = 
        inherit SpawnConfig()
        member val Angle = 0 with get, set
        member val Steps = 5 with get, set
        member val ScaleRange = NumberRange(0.4f, 1.0f) with get, set
        member val Config = 
            { DworkinAgentConfig.Default with 
                ColliderOptions=ColliderOptionsGeneral();
                Curvature=CurvatureFormula.ArchimedeanSpiralRev(N=4.0f);
                TerminationConditions={ 
                    Collision=Some(TerminationConditionCollision(MinDistance=5.0f) |> Optic.set (primaryDetector_ >-> distance_) 2.0f);
                    Curvature=None;
                    Distance=Some(TerminationConditionDistance())
                 };
                TotalDistance=590.0f
             } with get, set
        member val Levels = NumberRange(1.0f, 20.0f) with get, set
        
        member this.Clone() = this.MemberwiseClone() :?> Probe2BranchByScale
        
        static member Angle_ : Lens<Probe2BranchByScale, _> = 
            (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
        static member Steps_ : Lens<Probe2BranchByScale, _> = 
            (fun x -> x.Steps), (fun value x -> let y = x.Clone() in y.Steps <- value; y)
        static member ScaleRange_ : Lens<Probe2BranchByScale, _> = 
            (fun x -> x.ScaleRange), (fun value x -> let y = x.Clone() in y.ScaleRange <- value; y)
        static member Config_ : Lens<Probe2BranchByScale, _> = 
            (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
        static member Levels_ : Lens<Probe2BranchByScale, _> = 
            (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
        
        override this.Evaluate(agent, pos) =
            if this.Levels.Contains(float32 agent.Generation) then 
                let angles = ResizeArray([| float32 this.Angle; -(float32 this.Angle) |])
                let possibleScales = this.ScaleRange.By(this.ScaleRange.Extent / float32 this.Steps) |> Seq.rev
                for scale in possibleScales |> Seq.takeWhile (fun _ -> not angles.IsEmpty) do
                    for phi in angles.ToArray() do 
                        let agentBlock (a : DworkinAgent) = 
                            if (sign phi) < 0 then a.Flip()
                            a.ScaleBy(Point(1.0f, scale))
                        let spawnOptions = ProbeSpawnOptions(AngleDeg=phi, StepsBetweenCollisionCheck=2)
                        spawnOptions.DelayBy(10)
                        let probeData = 
                            agent.LaunchProbe(
                                this.Config
                                |> Optic.map (terminationConditions_ >-> collision_ >-> minDistance_) (fun x -> x / scale), 
                                spawnOptions,
                                agentBlock)
                        if probeData.Collision.IsNone then
                            angles.Remove(phi) |> ignore
                            agent.Spawn(
                                this.Config, 
                                ProbeSpawnOptions(AngleDeg=phi), 
                                fun a ->
                                    agentBlock a
                                    a.NoCollisions <- true
                                    probeData.SpawnColliders())
    



