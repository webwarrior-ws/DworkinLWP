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

open BranchingCriteria


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
    

    type IgnoreParentUpTo() = 
        inherit CollisionHandler()
        member val MinDistance = 1.0f with get, set
        
        member this.Clone() = this.MemberwiseClone() :?> IgnoreParentUpTo
        
        static member MinDistance_ : Lens<IgnoreParentUpTo, _> = 
            (fun x -> x.MinDistance), (fun value x -> let y = x.Clone() in y.MinDistance <- value; y)
        
        override this.Evaluate(agent, collision) =
            (collision.Obstacle.Parent <> (agent.Parent |> Option.map (fun x -> x :> IDworkinAgentState))) || (agent.S > this.MinDistance)

    
    type IgnoreIfIntersectsAtAngleGt() = 
        inherit CollisionHandler()
        member val Angle = 60.0f with get, set
        
        member this.Clone() = this.MemberwiseClone() :?> IgnoreIfIntersectsAtAngleGt
        
        static member Angle_ : Lens<IgnoreIfIntersectsAtAngleGt, _> = 
            (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
        
        override this.Evaluate(agent, collision) =
            match collision.Obstacle.Parent with
            | Some(p) -> 
                let dot = Point.Dot(agent.Direction, p.Direction)
                (abs dot) > (cos (deg2rad this.Angle))
            | None -> true
    
 

module DworkinAgentConfig =
    module SwirlMagnetic = 
        type TerminationConditions() = 
            inherit CollisionHandler()
            member val MinT = 0.5f with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> TerminationConditions
            
            static member MinT_ : Lens<TerminationConditions, _> = 
                (fun x -> x.MinT), (fun value x -> let y = x.Clone() in y.MinT <- value; y)
            
            override this.Evaluate(agent, collision) =
                agent.T >= this.MinT

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

    let fernLeaf = 
        { DworkinAgentConfig.Default with 
            Curvature=CurvatureFormula.FixedPower(A=8.0f, P=10.0f, B=0.407f, C=0.002f);
            Thickness=ThicknessFormula.DecLinear(A=10.0f, P=0.98f);
            TotalDistance=200.0f }

    let leafSimple = 
        { DworkinAgentConfig.Default with 
            Curvature=CurvatureFormula.Constant(C=0.0f);
            Thickness=ThicknessFormula.LeafPower(A=2.0f,P=0.5f);
            TotalDistance=20.0f }

    let swirlMagnetic = 
        { DworkinAgentConfig.Default with 
            Branching=BranchingMultivariant();
            ColliderOptions=ColliderOptionsGeneral(Strategy=ColliderStrategySegments());
            Curvature=CurvatureFormula.MagneticWithFlip();
            TerminationConditions={ 
                Collision=Some(
                    TerminationConditionCollision(
                        DetectorSize=DetectorSize.Absolute,
                        Handler=Some(DworkinAgentConfig.SwirlMagnetic.TerminationConditions() :> CollisionHandler),
                        MinDistance=300.0f,
                        SelfIntersection=false)
                    |> Optic.set (primaryDetector_ >-> angle_) 120.0f
                    |> Optic.set (primaryDetector_ >-> distance_) 15.0f);
                Curvature=Some(TerminationConditionCurvature(MaxCurvature=0.5f));
                Distance=Some(TerminationConditionDistance())
             };
            Thickness=ThicknessFormula.DecLinear(A=5.0f,P=0.5f)
         }


module SpawnConfig =
    open LibDworkin.SpatialIndex

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
    

    module Probe1BranchSwirlsShifted = 
        module AgentConfig = 
            module Branching = 
                type SpawnConfig() = 
                    inherit LibDworkin.Configs.SpawnConfig()
                    member val Angle = 0 with get, set
                    member val LengthRange = NumberRange(100.0f, 300.0f) with get, set
                    member val AlphaRange = NumberRange(0.6f, 1.5f) with get, set
                    member val AlphaSteps = 8 with get, set
                    member val Offset = 5.0f with get, set
                    member val Levels = NumberRange(1.0f, 2.0f) with get, set
                    
                    member this.Clone() = this.MemberwiseClone() :?> SpawnConfig
                    
                    static member Angle_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
                    static member LengthRange_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
                    static member AlphaRange_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.AlphaRange), (fun value x -> let y = x.Clone() in y.AlphaRange <- value; y)
                    static member AlphaSteps_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.AlphaSteps), (fun value x -> let y = x.Clone() in y.AlphaSteps <- value; y)
                    static member Offset_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
                    static member Levels_ : Lens<SpawnConfig, _> = 
                        (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
                    
                    override this.Evaluate(agent, pos) =
                        if this.Levels.Contains (float32 agent.Generation) then 
                            seq {
                                for alpha in this.AlphaRange.By(this.AlphaRange.Extent / (float32 this.AlphaSteps)) do
                                    let mutable config = 
                                        agent.Config
                                        |> Optic.set totalDistance_ (this.LengthRange.Max)
                                        |> Optic.set (curvature_ >-> cast<_, CurvatureFormula.DiminishingSpiral> >-> alpha_) alpha
                                    let mutable probeData = 
                                        agent.LaunchProbe(
                                            config, 
                                            ProbeSpawnOptions(AngleDeg=float32 this.Angle, OffsetSideways=this.Offset, StepsBetweenCollisionCheck=4),
                                            ignore)
                                    match probeData.Collision with
                                    | Some(value) -> 
                                        if (probeData.S / 2.0f) > (float32 this.LengthRange.Min) then 
                                            config <- 
                                                config
                                                |> Optic.set totalDistance_ (agent.Random.NextInRange([| this.LengthRange.Min; probeData.S / 2.0f |]))
                                            probeData <- 
                                                agent.LaunchProbe(
                                                    config, 
                                                    ProbeSpawnOptions(AngleDeg=float32 this.Angle, OffsetSideways=this.Offset, StepsBetweenCollisionCheck=100),
                                                    ignore)
                                            yield agent.Spawn(
                                                config, 
                                                AgentSpawnOptions(AngleDeg=float32 this.Angle, OffsetSideways=this.Offset), 
                                                fun a ->
                                                    a.NoCollisions <- true
                                                    probeData.SpawnColliders())
                                    | None -> 
                                        yield agent.Spawn(
                                            config, 
                                            AgentSpawnOptions(AngleDeg=float32 this.Angle, OffsetSideways=this.Offset), 
                                            fun a ->
                                                a.NoCollisions <- true
                                                probeData.SpawnColliders())
                                }
                                |> Seq.tryHead
                                |> ignore
            
    [<AbstractClass>]
    type Probe1BranchSwirlsShifted() = 
        inherit SpawnConfig()
        member val Angle = 0 with get, set
        member val LengthRange = NumberRange(150.0f, 250.0f) with get, set
        member val LengthSteps = 3 with get, set
        member val AlphaRange = NumberRange(0.2f, 1.2f) with get, set
        member val AlphaSteps = 6 with get, set
        member val AgentConfig = 
            { DworkinAgentConfig.Default with 
                Branching=BranchingCriterionBased(
                    Criterion=BranchingCriterionCustom.Interval_MaxBranches(D=10.0f,N=1),
                    SpawnConfig=Probe1BranchSwirlsShifted.AgentConfig.Branching.SpawnConfig()
                );
                ColliderOptions=ColliderOptionsGeneral();
                Curvature=CurvatureFormula.DiminishingSpiral(C=0.7f,Alpha=1.5f);
                TerminationConditions={ 
                    Collision=Some(
                        TerminationConditionCollision(
                            Handler=Some(CollisionHandler.IgnoreParentUpTo(MinDistance=20.0f) :> CollisionHandler),
                            SelfIntersection=false) 
                            |> Optic.set (primaryDetector_ >-> distance_) 4.0f
                        );
                    Curvature=Some(TerminationConditionCurvature());
                    Distance=Some(TerminationConditionDistance())
                };
                Thickness=ThicknessFormula.LeafPower(A=3.0f,P=0.68f,C=2.0f)
            } with get, set
        member val Offset = 5 with get, set
        member val Levels = NumberRange(1.0f, 2.0f) with get, set
    
        member this.Clone() = this.MemberwiseClone() :?> Probe1BranchSwirlsShifted
    
        static member Angle_ : Lens<Probe1BranchSwirlsShifted, _> = 
            (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
        static member LengthRange_ : Lens<Probe1BranchSwirlsShifted, _> = 
            (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
        static member LengthSteps_ : Lens<Probe1BranchSwirlsShifted, _> = 
            (fun x -> x.LengthSteps), (fun value x -> let y = x.Clone() in y.LengthSteps <- value; y)
        static member AlphaRange_ : Lens<Probe1BranchSwirlsShifted, _> = 
            (fun x -> x.AlphaRange), (fun value x -> let y = x.Clone() in y.AlphaRange <- value; y)
        static member AlphaSteps_ : Lens<Probe1BranchSwirlsShifted, _> = 
            (fun x -> x.AlphaSteps), (fun value x -> let y = x.Clone() in y.AlphaSteps <- value; y)
        static member AgentConfig_ : Lens<Probe1BranchSwirlsShifted, _> = 
            (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
        static member Offset_ : Lens<Probe1BranchSwirlsShifted, _> = 
            (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
        static member Levels_ : Lens<Probe1BranchSwirlsShifted, _> = 
            (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
        
        abstract GetModifiedAgentConfig : DworkinAgent -> float32 -> DworkinAgentConfig -> DworkinAgentConfig

        override this.Evaluate(agent, pos) =
            if this.Levels.Contains (float32 agent.Generation) then 
                let combinations = 
                    [| for x1 in this.LengthRange.By(this.LengthRange.Extent / (float32 this.LengthSteps)) do
                       for x2 in this.AlphaRange.By(this.AlphaRange.Extent / (float32 this.AlphaSteps)) do yield x1, x2 |].ShuffleBy(agent.Random)
                seq {
                    for length, alpha in combinations do
                        let agentBlock (a : DworkinAgent) = if (agent.Curvature * a.FlipFactor) < 0.0f then a.Flip()
                        let config = 
                            this.AgentConfig
                            |> Optic.set totalDistance_ length
                            |> Optic.set (curvature_ >-> cast<_, CurvatureFormula.DiminishingSpiral> >-> alpha_) alpha
                            |> this.GetModifiedAgentConfig agent alpha
                        let probeData = 
                            agent.LaunchProbe(
                                config, 
                                ProbeSpawnOptions(
                                    AngleDeg=float32 this.Angle, 
                                    OffsetSideways=float32(this.Offset * (sign agent.Curvature)),
                                    StepsBetweenCollisionCheck = int(floor (this.AgentConfig.TerminationConditions.Collision.Value.PrimaryDetector.Distance))), 
                                agentBlock)
                        match probeData.Collision with
                        | Some(value) -> ()
                        | None -> 
                            yield agent.Spawn(
                                config, 
                                AgentSpawnOptions( AngleDeg=float32 this.Angle, OffsetSideways=float32(this.Offset * (sign agent.Curvature))), 
                                fun a ->
                                    agentBlock a
                                    a.NoCollisions <- true
                                    probeData.Id <- a.Id
                                    probeData.SpawnColliders())
                    }
                    |> Seq.tryHead
                    |> ignore

    type Probe1BranchSwirlsShifted1() = 
        inherit Probe1BranchSwirlsShifted(
            LengthRange = NumberRange(100.0f, 300.0f), 
            AlphaRange = NumberRange(0.6f, 1.5f), 
            AlphaSteps = 8,
            AgentConfig = { DworkinAgentConfig.Default with
                                Curvature=CurvatureFormula.DiminishingSpiral(C=0.7f,Alpha=1.5f);
                                Thickness=ThicknessFormula.LeafPower(A=2.0f,P=0.68f,C=10.0f);
                                Brush=BrushStrategyCirclesPacked(Gap=1.0f);
                                Branching=BranchingCriterionBased(
                                    Criterion=BranchingCriterionCustom.Interval_MaxBranches(D=10.0f,N=1),
                                    SpawnConfig=Probe1BranchSwirlsShifted.AgentConfig.Branching.SpawnConfig(Levels=NumberRange(1.0f, 2.0f))
                                );
                                TerminationConditions={ 
                                    Collision=Some(
                                        TerminationConditionCollision(
                                            Handler=Some(CollisionHandler.IgnoreParentUpTo(MinDistance=20.0f) :> CollisionHandler),
                                            SelfIntersection=false) 
                                        |> Optic.set (primaryDetector_ >-> distance_) 2.0f);
                                    Curvature=Some(TerminationConditionCurvature());
                                    Distance=Some(TerminationConditionDistance.Instance)
                                 };
                                ColliderOptions=ColliderOptionsGeneral();
                          }) 

        override this.GetModifiedAgentConfig agent alpha agentConfig =
            let newAlphaRange = NumberRange([| alpha + (this.AlphaRange.Extent * 0.05f); this.AlphaRange.Max + (this.AlphaRange.Extent * 0.1f) |])
            agentConfig
            |> Optic.set 
                (branching_<BranchingCriterionBased> >-> spawnConfig_ >-> cast<_, Probe1BranchSwirlsShifted.AgentConfig.Branching.SpawnConfig> >-> alphaRange_) 
                newAlphaRange
    
    type Probe1BranchSwirlsShifted1Rec() as this = 
        inherit Probe1BranchSwirlsShifted(Offset= -7, AlphaRange = NumberRange(0.2f, 0.6f), AlphaSteps=4, Levels=NumberRange(1.0f, 1.0f))
        do
            this.AgentConfig <- 
                this.AgentConfig
                |> Optic.set 
                    (terminationConditions_ >-> collision_ >-> handler_<CollisionHandler.IgnoreParentUpTo> >-> CollisionHandler.IgnoreParentUpTo.MinDistance_) 
                    50.0f
                |> Optic.set (thickness_<ThicknessFormula.LeafPower_Base> >-> a_) 1.0f
                |> Optic.set (thickness_<ThicknessFormula.LeafPower_Base> >-> c_) 5.0f

        override this.GetModifiedAgentConfig agent alpha agentConfig = agentConfig

    type Probe1BranchSwirlsShifted3() = 
        inherit Probe1BranchSwirlsShifted()

        override this.GetModifiedAgentConfig agent alpha agentConfig =
            agentConfig |> Optic.set branching_ agent.Config.Branching

    
    type Probe1BranchSpiralByLen_AlphaRandRec() = 
        inherit SpawnConfig()
        member val Angle = 0 with get, set
        member val LengthRange = NumberRange(75.0f, 275.0f) with get, set
        member val LengthSteps = 4 with get, set
        member val AlphaRange = NumberRange(0.65f, 1.0f) with get, set
        member val AlphaSteps = 3 with get, set
        member val AgentConfig = 
            { DworkinAgentConfig.Default with 
                ColliderOptions=ColliderOptionsGeneral();
                Curvature=CurvatureFormula.Magnetic(Alpha=0.65f);
                TerminationConditions={ 
                    Collision=Some(TerminationConditionCollision(MinDistance=10.0f,SelfIntersection=false) 
                                   |> Optic.set (primaryDetector_ >-> distance_) 2.0f);
                    Curvature=None;
                    Distance=Some(TerminationConditionDistance())
                 };
                Thickness=ThicknessFormula.DecLinear(A=2.0f,P=0.495f)
             } with get, set
        member val Levels = NumberRange(1.0f, 5.0f) with get, set
        member val IsRecursive = true with get, set
        
        member this.Clone() = this.MemberwiseClone() :?> Probe1BranchSpiralByLen_AlphaRandRec
        
        static member Angle_ : Lens<Probe1BranchSpiralByLen_AlphaRandRec, _> = 
            (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
        static member LengthRange_ : Lens<Probe1BranchSpiralByLen_AlphaRandRec, _> = 
            (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
        static member LengthSteps_ : Lens<Probe1BranchSpiralByLen_AlphaRandRec, _> = 
            (fun x -> x.LengthSteps), (fun value x -> let y = x.Clone() in y.LengthSteps <- value; y)
        static member AlphaRange_ : Lens<Probe1BranchSpiralByLen_AlphaRandRec, _> = 
            (fun x -> x.AlphaRange), (fun value x -> let y = x.Clone() in y.AlphaRange <- value; y)
        static member AlphaSteps_ : Lens<Probe1BranchSpiralByLen_AlphaRandRec, _> = 
            (fun x -> x.AlphaSteps), (fun value x -> let y = x.Clone() in y.AlphaSteps <- value; y)
        static member AgentConfig_ : Lens<Probe1BranchSpiralByLen_AlphaRandRec, _> = 
            (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
        static member Levels_ : Lens<Probe1BranchSpiralByLen_AlphaRandRec, _> = 
            (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
        static member IsRecursive_ : Lens<Probe1BranchSpiralByLen_AlphaRandRec, _> = 
            (fun x -> x.IsRecursive), (fun value x -> let y = x.Clone() in y.IsRecursive <- value; y)
        
        override this.Evaluate(agent, pos) =
            if this.Levels.Contains (float32 agent.Generation) then 
                let combinations = 
                    [| for x1 in this.LengthRange.By(this.LengthRange.Extent / (float32 this.LengthSteps)) do
                        for x2 in this.AlphaRange.By(this.AlphaRange.Extent / (float32 this.AlphaSteps)) do
                            for x3 in [| float32 this.Angle; -(float32 this.Angle) |] do yield x1, x2, x3 |].ShuffleBy(agent.Random)
                seq {
                    for length, alpha, phi in combinations do
                        let agentBlock (a : DworkinAgent) = if (sign phi) < 0 then a.Flip()
                        let mutable config = 
                            this.AgentConfig
                            |> Optic.set totalDistance_ length
                            |> Optic.set (curvature_<CurvatureFormula.DiminishingSpiral> >-> alpha_) alpha
                        if this.IsRecursive then
                            config <- config |> Optic.set branching_ agent.Config.Branching
                        let probeData = 
                            agent.LaunchProbe(
                                config, 
                                ProbeSpawnOptions(
                                    AngleDeg=phi, 
                                    StepsBetweenCollisionCheck=int(floor(this.AgentConfig.TerminationConditions.Collision.Value.PrimaryDetector.Distance))), 
                                agentBlock)
                        match probeData.Collision with
                        | Some(value) -> ()
                        | None -> 
                            yield agent.Spawn(
                                    config, 
                                    AgentSpawnOptions(AngleDeg=phi), 
                                    fun a ->
                                        agentBlock a
                                        a.NoCollisions <- true
                                        probeData.Id <- a.Id
                                        probeData.SpawnColliders()) 
                    }
                    |> Seq.tryHead 
                    |> ignore

