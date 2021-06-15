module rec FloralSwirls


open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Geometry
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.Lenses
open LibDworkin.PharoCompatibility
open LibDworkin.SpecializedCollections
open LibDworkin.Colliders
open LibDworkin.Probes
open LibDworkin.Brushes

open MiscConfigs
open BranchingCriteria



module BranchingCustom =
    type NoBranching() =
        inherit BranchingCustom()
        override this.Evaluate(_) = ()


module BranchingMultivariant = 
    module SwirlsMagneticMany = 
        type SpawnConfig() = 
            inherit LibDworkin.Configs.SpawnConfig()
            member val LengthRange = NumberRange(75.0f, 250.0f) with get, set
            member val LengthSteps = 6 with get, set
            member val AlphaRange = NumberRange(0.7f, 0.8f) with get, set
            member val AlphaSteps = 3 with get, set
            member val AgentConfig = 
                { DworkinAgentConfig.Default with 
                    Brush=BrushStrategySolid();
                    ColliderOptions=ColliderOptionsGeneral();
                    Curvature=CurvatureFormula.Magnetic();
                    TerminationConditions={ 
                        Collision=Some(
                            TerminationConditionCollision(
                                DetectorSize=DetectorSize.Absolute,
                                Handler=Some(CollisionHandler.IgnoreIfIntersectsAtAngleGt(Angle=75.0f) :> CollisionHandler),
                                IgnoreParent=true,
                                MinDistance=10.0f)
                            |> Optic.set (primaryDetector_ >-> angle_) 60.0f
                            |> Optic.set (primaryDetector_ >-> distance_) 10.0f);
                        Curvature=None;
                        Distance=Some(TerminationConditionDistance.Instance)
                     };
                    Thickness=ThicknessFormula.LeafPower_Base(P=0.6f)
                 } with get, set
            member val BothSides = true with get, set
            member val NBranches = 6 with get, set
            member val AngleRange = NumberRange(1.0f, 10.0f) with get, set
            member val AngleSteps = 2 with get, set
            member val Levels = NumberRange(1.0f, 1.0f) with get, set
            member val ThicknessMean = 3.0f with get, set
            member val ThicknessSpread = 1.0f with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> SpawnConfig
            
            static member LengthRange_ : Lens<SpawnConfig, _> = 
                (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
            static member LengthSteps_ : Lens<SpawnConfig, _> = 
                (fun x -> x.LengthSteps), (fun value x -> let y = x.Clone() in y.LengthSteps <- value; y)
            static member AlphaRange_ : Lens<SpawnConfig, _> = 
                (fun x -> x.AlphaRange), (fun value x -> let y = x.Clone() in y.AlphaRange <- value; y)
            static member AlphaSteps_ : Lens<SpawnConfig, _> = 
                (fun x -> x.AlphaSteps), (fun value x -> let y = x.Clone() in y.AlphaSteps <- value; y)
            static member AgentConfig_ : Lens<SpawnConfig, _> = 
                (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
            static member BothSides_ : Lens<SpawnConfig, _> = 
                (fun x -> x.BothSides), (fun value x -> let y = x.Clone() in y.BothSides <- value; y)
            static member NBranches_ : Lens<SpawnConfig, _> = 
                (fun x -> x.NBranches), (fun value x -> let y = x.Clone() in y.NBranches <- value; y)
            static member AngleRange_ : Lens<SpawnConfig, _> = 
                (fun x -> x.AngleRange), (fun value x -> let y = x.Clone() in y.AngleRange <- value; y)
            static member AngleSteps_ : Lens<SpawnConfig, _> = 
                (fun x -> x.AngleSteps), (fun value x -> let y = x.Clone() in y.AngleSteps <- value; y)
            static member Levels_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
            static member ThicknessMean_ : Lens<SpawnConfig, _> = 
                (fun x -> x.ThicknessMean), (fun value x -> let y = x.Clone() in y.ThicknessMean <- value; y)
            static member ThicknessSpread_ : Lens<SpawnConfig, _> = 
                (fun x -> x.ThicknessSpread), (fun value x -> let y = x.Clone() in y.ThicknessSpread <- value; y)
            
            override this.Evaluate(agent, pos) =
                if this.Levels.Contains (float32 agent.Generation) then 
                    let mutable angles = this.AngleRange.By(this.AngleRange.Extent / (float32 this.AngleSteps))
                    if this.BothSides then angles <- Seq.append angles (Seq.map (fun each -> -(float32 each)) angles)
                    let mutable n = 0
                    
                    let combinations =
                        [| for x1 in this.LengthRange.By(this.LengthRange.Extent / (float32 this.LengthSteps)) do
                            for x2 in this.AlphaRange.By(this.AlphaRange.Extent / (float32 this.AlphaSteps)) do
                                for x3 in angles do yield x1, x2, x3 |].ShuffleBy(agent.Random)
                    
                    seq { 
                        for length, alpha, phi in  combinations do
                            if n >= this.NBranches then 
                                yield ()
                            else 
                                let agentBlock = fun (a : DworkinAgent) -> if (sign phi) < 0 then a.Flip()
                                let mutable config = 
                                    this.AgentConfig
                                    |> Optic.set totalDistance_ length
                                    |> Optic.set (curvature_ >-> tryCast<_, CurvatureFormula.Magnetic> >?> alpha_) alpha
                                    |> Optic.set (thickness_ >-> tryCast<_, ThicknessFormula.LeafPower_Base> >?> c_) (max (agent.Random.NextGaussian(this.ThicknessMean, this.ThicknessSpread)) 0.5f)
                                if length > this.LengthRange.Average then config <- config |> Optic.set branching_ agent.Config.Branching
                                let probeData = 
                                    agent.LaunchProbe(
                                        config, 
                                        ProbeSpawnOptions(
                                            AngleDeg = phi, 
                                            StepsBetweenCollisionCheck = (this.AgentConfig.TerminationConditions.Collision.Value.PrimaryDetector.Distance |> floor |> int)),
                                        agentBlock)
                                if Option.isNone probeData.Collision then 
                                    n <- n + 1
                                    agent.Spawn(
                                        config |> Optic.set totalDistance_ probeData.S, 
                                        ProbeSpawnOptions(AngleDeg=phi), 
                                        fun a ->
                                            agentBlock a
                                            a.NoCollisions <- true
                                            probeData.Id <- a.Id
                                            probeData.SpawnColliders()) 
                            } 
                            |> Seq.tryHead |> ignore        



module DworkinAgentConfig =
    let plantWithSpiralBranches2 = 
        { DworkinAgentConfig.Default with 
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.Cosine(A=0.01f,B=2.06f);
            Thickness=ThicknessFormula.LeafPower(A=1.0f,P=0.4f,C=4.0f);
            TotalDistance=500.0f
         }


module LargeSwirlsStrategies =
    let swirlsMagneticMany : BranchingStrategy[] = 
        [|
            BranchingCriterionBased(
                Criterion=BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(60.0f, 150.0f),TInterval=NumberRange(0.1f, 0.9f)),
                SpawnConfig=BranchingMultivariant.SwirlsMagneticMany.SpawnConfig()
            ) :> BranchingStrategy
        |]

    let plantLike4Cosine : BranchingStrategy[]  = 
        [|
            BranchingCriterionBased(
                Criterion=BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(60.0f, 150.0f),TInterval=NumberRange(0.1f, 0.8f)),
                Delay=30,
                SpawnConfig=SpawnConfig.Probe1BranchSwirlsShifted3()
            );
            
            BranchingCriterionBased(
                Criterion=BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(80.0f, 150.0f),TInterval=NumberRange(0.1f, 0.8f)),
                Delay=50,
                SpawnConfig=(SpawnConfig.Probe1BranchSwirlsShifted1Rec())
            );
            
            BranchingCriterionBased(
                Criterion=(BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(120.0f, 160.0f),TInterval=NumberRange(0.2f, 0.8f))),
                Delay=100,
                SpawnConfig=(SpawnConfig.Probe1BranchSpiralByLen_AlphaRandRec(Angle=5,LengthRange=NumberRange(100.0f, 250.0f),LengthSteps=5,AlphaRange=NumberRange(0.5f, 1.5f),AlphaSteps=5,Levels=NumberRange(1.0f, 1.0f))
                             |> Optic.set (agentConfig_ >-> curvature_) (CurvatureFormula.DiminishingSpiral(C=0.68f))
                             |> Optic.set (agentConfig_ >-> terminationConditions_ >-> collision_ >-> handler_) (CollisionHandler.IgnoreParentUpTo(MinDistance=50.0f))
                             |> Optic.set (agentConfig_ >-> terminationConditions_ >-> collision_ >-> minDistance_) 0.0f
                             |> Optic.set (agentConfig_ >-> thickness_) (ThicknessFormula.LeafPower(A=2.0f,P=0.77f,C=10.0f)))
            );
            
            BranchingCriterionBased(
                Criterion=BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(60.0f, 160.0f),TInterval=NumberRange(0.0f, 0.8f)),
                Delay=20,
                SpawnConfig=SpawnConfig.Probe1BranchSwirlsShifted1()
            );
        |]


type CosineSwirlSingle() = 
    inherit DworkinEnvironmentConfigCustom()
    member val AgentConfig = 
        DworkinAgentConfig.plantWithSpiralBranches2 
        |> Optic.set totalDistance_ 650.0f with get, set
    member val LargeSwirls : BranchingStrategy[] = [| |] with get, set
    member val Offset = 0.06f with get, set
    member val Thickness = 1.0f with get, set
    member val LeafStrategy : BranchingStrategy = BranchingCustom.NoBranching() :> BranchingStrategy with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> CosineSwirlSingle
    
    static member AgentConfig_ : Lens<CosineSwirlSingle, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member LargeSwirls_ : Lens<CosineSwirlSingle, _> = 
        (fun x -> x.LargeSwirls), (fun value x -> let y = x.Clone() in y.LargeSwirls <- value; y)
    static member Offset_ : Lens<CosineSwirlSingle, _> = 
        (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
    static member Thickness_ : Lens<CosineSwirlSingle, _> = 
        (fun x -> x.Thickness), (fun value x -> let y = x.Clone() in y.Thickness <- value; y)
    static member LeafStrategy_ : Lens<CosineSwirlSingle, _> = 
        (fun x -> x.LeafStrategy), (fun value x -> let y = x.Clone() in y.LeafStrategy <- value; y)
    
    override this.Evaluate(env) =
        let width = env.Bounds.Width
        let height = env.Bounds.Height

        Util.buildWalls env
        
        let pos = 
            if width > height then 
                env.Bounds.LeftCenter + Point(1.0f, height * this.Offset) 
            else 
                env.Bounds.BottomCenter + Point(width * this.Offset, -1.0f)
        let angle = if width > height then -Single.HalfPi else 0.0f
        let scale = (max width height) / 640.0f

        let spawnOptions = ProbeSpawnOptions(Position=pos, Angle=angle)
        let agentBlock = fun (a : DworkinAgent) -> a.ScaleBy(Point(scale * this.Thickness, scale))
        let probeData = env.LaunchProbe(this.AgentConfig, spawnOptions, agentBlock)
        if probeData.ReachedEnd then 
            env.Spawn(
                this.AgentConfig
                |> Optic.set branching_ (BranchingMultivariant(Strategies=Array.append this.LargeSwirls [| this.LeafStrategy |])), 
                spawnOptions, 
                fun a ->
                    agentBlock a
                    probeData.Id <- a.Id
                    probeData.SpawnColliders()
                    a.NoCollisions <- true)
            |> ignore
