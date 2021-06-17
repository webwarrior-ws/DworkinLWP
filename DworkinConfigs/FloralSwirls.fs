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



module BranchingCriterionBased = 
    module Leaves2SidesProbesStraight1_fs = 
        type SpawnConfig() = 
            inherit LibDworkin.Configs.SpawnConfig()
            member val Angle = 30 with get, set
            member val Steps = 2 with get, set
            member val ScaleRange = NumberRange(0.6f, 1.0f) with get, set
            member val Config = 
                DworkinAgentConfig.leafSimple
                |> Optic.set colliderOptions_ (ColliderOptionsGeneral())
                |> Optic.set terminationConditions_ { 
                    Collision=Some(TerminationConditionCollision(IgnoreParent=true,SelfIntersection=false) |> Optic.set (primaryDetector_ >-> distance_) 2.0f);
                    Curvature=None;
                    Distance=Some(TerminationConditionDistance.Instance)
                 } with get, set
            member val Offset = 3 with get, set
            member val Levels = NumberRange(1.0f, 10.0f) with get, set
            member val MaxCurvature = 0.03f with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> SpawnConfig
            
            static member Angle_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
            static member Steps_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Steps), (fun value x -> let y = x.Clone() in y.Steps <- value; y)
            static member ScaleRange_ : Lens<SpawnConfig, _> = 
                (fun x -> x.ScaleRange), (fun value x -> let y = x.Clone() in y.ScaleRange <- value; y)
            static member Config_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
            static member Offset_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
            static member Levels_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
            static member MaxCurvature_ : Lens<SpawnConfig, _> = 
                (fun x -> x.MaxCurvature), (fun value x -> let y = x.Clone() in y.MaxCurvature <- value; y)
            
            override this.Evaluate(agent, pos) =
                if this.Levels.Contains (float32 agent.Generation) && (abs agent.Curvature) <= this.MaxCurvature then 
                    let scaleValues = this.ScaleRange.By(this.ScaleRange.Extent / (float32 this.Steps))
                    let angles = ResizeArray([| float32 this.Angle; -(float32 this.Angle) |])
                    seq {
                        for scale in Seq.rev scaleValues do
                            match angles with
                            | NotEmpty(value) -> 
                                for phi in angles.ToArray() do
                                    let agentBlock (a :DworkinAgent) = 
                                        if (sign phi) < 0 then a.Flip()
                                    let spawnBlock = 
                                        ProbeSpawnOptions(
                                            AngleDeg=phi,
                                            OffsetSideways=(float32 this.Offset) * agent.ThicknessFactor,
                                            StepsBetweenCollisionCheck=2)
                                    let probeData = 
                                        agent.LaunchProbe(
                                            this.Config
                                            |> Optic.map (terminationConditions_ >-> collision_ >-> minDistance_) (fun x -> x / scale), 
                                            spawnBlock, 
                                            fun a ->
                                                agentBlock a
                                                a.ScaleBy(Point(1.0f, scale)))
                                    match probeData.Collision with
                                    | Some(_) -> ()
                                    | None -> 
                                        angles.Remove(phi) |> ignore
                                        agent.Spawn(
                                            this.Config, 
                                            spawnBlock, 
                                            fun a ->
                                                agentBlock a
                                                a.ScaleBy(Point(1.0f, scale))
                                                a.NoCollisions <- true
                                                probeData.Id <- a.Id
                                                probeData.SpawnColliders())
                                        ()
                            | Empty -> yield ()
                        }
                        |> Seq.tryHead
                        |> ignore
    
    module LeavesProbesFollowCurv_fs = 
        type SpawnConfig() = 
            inherit LibDworkin.Configs.SpawnConfig()
            member val Angle = 15 with get, set
            member val Steps = 3 with get, set
            member val ScaleRange = NumberRange(1.5f, 2.5f) with get, set
            member val Config = 
                { DworkinAgentConfig.Default with 
                    ColliderOptions=ColliderOptionsGeneral();
                    Curvature=CurvatureFormula.Constant(C=0.042f);
                    TerminationConditions={ 
                        Collision=Some(TerminationConditionCollision(MinDistance=2.0f,SelfIntersection=false) 
                                       |> Optic.set (primaryDetector_ >-> distance_) 2.0f);
                        Curvature=None;
                        Distance=Some(TerminationConditionDistance())
                     };
                    Thickness=ThicknessFormula.LeafPower(A=2.5f,P=0.79f);
                    TotalDistance=15.0f
                 } with get, set
            member val Offset = 5 with get, set
            member val Levels = NumberRange(1.0f, 10.0f) with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> SpawnConfig
            
            static member Angle_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
            static member Steps_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Steps), (fun value x -> let y = x.Clone() in y.Steps <- value; y)
            static member ScaleRange_ : Lens<SpawnConfig, _> = 
                (fun x -> x.ScaleRange), (fun value x -> let y = x.Clone() in y.ScaleRange <- value; y)
            static member Config_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
            static member Offset_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
            static member Levels_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
            
            override this.Evaluate(agent, pos) =
                if this.Levels.Contains (float32 agent.Generation) then 
                    let phi = if (agent.Curvature * agent.FlipFactor) < 0.0f then -this.Angle else this.Angle
                    for scale in this.ScaleRange.By(this.ScaleRange.Extent / (float32 this.Steps)) |> Seq.rev do
                        let agentBlock (a : DworkinAgent) = if (sign phi) < 0 then a.Flip()
                        let spawnBlock = 
                            ProbeSpawnOptions(
                                AngleDeg=float32 phi, 
                                OffsetSideways=(float32 this.Offset) * agent.ThicknessFactor,
                                StepsBetweenCollisionCheck=2)
                        let probeData = 
                            agent.LaunchProbe(
                                this.Config
                                |> Optic.map (terminationConditions_ >-> collision_ >-> minDistance_) (fun x -> x / scale), 
                                spawnBlock, 
                                fun a ->
                                    agentBlock a
                                    a.ScaleBy(scale))
                        match probeData.Collision with
                        | Some(_) -> ()
                        | None -> 
                            agent.Spawn(
                                this.Config, 
                                spawnBlock, 
                                fun a ->
                                    agentBlock a
                                    a.ScaleBy(scale)
                                    a.NoCollisions <- true
                                    probeData.Id <- a.Id
                                    probeData.SpawnColliders())
                            ()
        


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
    

    module SwirlsRecursive = 
        type SpawnConfig() = 
            inherit LibDworkin.Configs.SpawnConfig()
            member val Angle = 10 with get, set
            member val LengthRange = NumberRange(250.0f, 600.0f) with get, set
            member val LengthSteps = 4 with get, set
            member val AlphaRange = NumberRange(0.68f, 0.75f) with get, set
            member val AlphaSteps = 2 with get, set
            member val AgentConfig = 
                DworkinAgentConfig.fernLeaf
                |> Optic.set colliderOptions_ (ColliderOptionsGeneral())
                |> Optic.set curvature_ (CurvatureFormula.Magnetic(Alpha=0.65f))
                |> Optic.set speed_ (AgentSpeedConstant())
                |> Optic.set terminationConditions_ { 
                    Collision=Some(
                        TerminationConditionCollision(DetectorSize=DetectorSize.Absolute,MinDistance=15.0f,SelfIntersection=false)
                        |> Optic.set (primaryDetector_ >-> angle_) 120.0f
                        |> Optic.set (primaryDetector_ >-> distance_) 4.0f);
                    Curvature=None;
                    Distance=Some(TerminationConditionDistance())
                 }
                |> Optic.set thickness_ (ThicknessFormula.DecLinear(A=3.0f,P=0.4f)) 
                |> Optic.set brush_ (BrushStrategySolid(AlignWithParent=true)) with get, set
            member val Offset = 0.0f with get, set
            member val Levels = NumberRange(1.0f, 8.0f) with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> SpawnConfig
            
            static member Angle_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
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
            static member Offset_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
            static member Levels_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
            
            override this.Evaluate(agent, pos) =
                if this.Levels.Contains (float32 agent.Generation) then 
                    let combinations = 
                        [| for x1 in this.LengthRange.By(this.LengthRange.Extent / (float32 this.LengthSteps)) do
                            for x2 in this.AlphaRange.By(this.AlphaRange.Extent / (float32 this.AlphaSteps)) do
                                for x3 in [| float32 this.Angle; -(float32 this.Angle) |] do yield x1, x2, x3 |].ShuffleBy(agent.Random)
                    seq {
                        for length, alpha, phi in combinations do
                            let spawnOptions = 
                                ProbeSpawnOptions(
                                    AngleDeg=phi, 
                                    OffsetSideways=this.Offset * (if ((sign phi) < 0) && (phi = 0.0f) then -1.0f else 1.0f),
                                    StepsBetweenCollisionCheck=int(floor (this.AgentConfig.TerminationConditions.Collision.Value.PrimaryDetector.Distance))
                                )
                            let agentBlock (a : DworkinAgent) = if (sign phi) < 0 then a.Flip()
                            let config = 
                                this.AgentConfig
                                |> Optic.set totalDistance_ length
                                |> Optic.set (curvature_<CurvatureFormula.Magnetic> >-> alpha_) alpha
                                |> Optic.set branching_ agent.Config.Branching
                            let probeData = agent.LaunchProbe(config, spawnOptions, agentBlock)
                            match probeData.Collision with
                            | Some(value) -> ()
                            | None -> 
                                yield agent.Spawn(
                                    config, 
                                    spawnOptions, 
                                    fun a ->
                                        agentBlock a
                                        a.NoCollisions <- true
                                        probeData.Id <- a.Id
                                        probeData.SpawnColliders())
                        }
                        |> Seq.tryHead
                        |> ignore

    module SwirlsShifted4Cosine = 
        module SpawnConfig = 
            type AgentConfig() = 
                inherit SpawnConfig()
                member val Angle = 0 with get, set
                member val GenLimit = 6 with get, set
                member val LengthRange = NumberRange(100.0f, 300.0f) with get, set
                member val AlphaRange = NumberRange(0.6f, 1.5f) with get, set
                member val AlphaSteps = 8 with get, set
                member val Offset = 4.0f with get, set
                
                member this.Clone() = this.MemberwiseClone() :?> AgentConfig
                
                static member Angle_ : Lens<AgentConfig, _> = 
                    (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
                static member GenLimit_ : Lens<AgentConfig, _> = 
                    (fun x -> x.GenLimit), (fun value x -> let y = x.Clone() in y.GenLimit <- value; y)
                static member LengthRange_ : Lens<AgentConfig, _> = 
                    (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
                static member AlphaRange_ : Lens<AgentConfig, _> = 
                    (fun x -> x.AlphaRange), (fun value x -> let y = x.Clone() in y.AlphaRange <- value; y)
                static member AlphaSteps_ : Lens<AgentConfig, _> = 
                    (fun x -> x.AlphaSteps), (fun value x -> let y = x.Clone() in y.AlphaSteps <- value; y)
                static member Offset_ : Lens<AgentConfig, _> = 
                    (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
                
                override this.Evaluate(agent, pos) =
                    let spawnBlock = ProbeSpawnOptions(AngleDeg=float32 this.Angle, OffsetSideways=this.Offset, StepsBetweenCollisionCheck=4)
                    agent.GenerationLimit <- this.GenLimit
                    seq {
                        for alpha in this.AlphaRange.By(this.AlphaRange.Extent / float32 this.AlphaSteps) do
                            let mutable config = 
                                agent.Config
                                |> Optic.set totalDistance_ this.LengthRange.Max
                                |> Optic.set (curvature_<CurvatureFormula.DiminishingSpiral> >-> alpha_) alpha
                            let mutable probeData = agent.LaunchProbe(config, spawnBlock, ignore)
                            match probeData.Collision with
                            | Some(value) -> 
                                if (probeData.S / 2.0f) > this.LengthRange.Min then 
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
                                        spawnBlock, 
                                        fun a ->
                                            a.NoCollisions <- true
                                            probeData.SpawnColliders())
                            | None -> 
                                yield agent.Spawn(
                                    config, 
                                    spawnBlock, 
                                    fun a ->
                                        a.NoCollisions <- true
                                        probeData.SpawnColliders())
                        }
                        |> Seq.tryHead
                        |> ignore


    module SwirlsShifted4S = 
        type SpawnConfig() = 
            inherit LibDworkin.Configs.SpawnConfig()
            member val Angle = 5 with get, set
            member val LengthRange = NumberRange(300.0f, 400.0f) with get, set
            member val LengthSteps = 3 with get, set
            member val AlphaRange = NumberRange(0.75f, 0.85f) with get, set
            member val AlphaSteps = 2 with get, set
            member val AgentConfig = 
                { DworkinAgentConfig.Default with 
                    ColliderOptions=ColliderOptionsGeneral();
                    Curvature=CurvatureFormula.Magnetic(Alpha=0.65f);
                    TerminationConditions={ 
                        Collision=Some(
                            TerminationConditionCollision(
                                DetectorSize=DetectorSize.Absolute,
                                Handler=Some(CollisionHandler.IgnoreIfIntersectsAtAngleGt() :> CollisionHandler),
                                MinDistance=30.0f,
                                SelfIntersection=false) 
                                |> Optic.set (primaryDetector_ >-> distance_) 2.0f);
                        Curvature=None;
                        Distance=Some(TerminationConditionDistance())
                     };
                    Thickness=ThicknessFormula.LeafPower_Base(P=0.3f,C=2.0f)
                 } with get, set
            member val Levels = NumberRange(1.0f, 1.0f) with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> SpawnConfig
            
            static member Angle_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
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
            static member Levels_ : Lens<SpawnConfig, _> = 
                (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
            
            override this.Evaluate(agent, pos) =
                if this.Levels.Contains (float32 agent.Generation) && this.LengthRange.Min < agent.TotalDistance then 
                    let combinations =
                        [| for x1 in seq { this.LengthRange.Min .. (this.LengthRange.Extent / (float32 this.LengthSteps)) .. (min this.LengthRange.Max agent.TotalDistance) } do
                            for x2 in this.AlphaRange.By(this.AlphaRange.Extent / (float32 this.AlphaSteps)) do yield x1, x2 |].ShuffleBy(agent.Random)
                    
                    seq {
                        for length, alpha in combinations do
                            let phi = this.Angle * (sign agent.Curvature)
                            let spawnBlock = 
                                ProbeSpawnOptions(
                                    AngleDeg=float32 phi, 
                                    StepsBetweenCollisionCheck=int(floor (this.AgentConfig.TerminationConditions.Collision.Value.PrimaryDetector.Distance)))
                            let agentBlock (a : DworkinAgent) = if (sign phi) < 0 then a.Flip()
                            let config = 
                                this.AgentConfig
                                |> Optic.set totalDistance_ length
                                |> Optic.set (curvature_<CurvatureFormula.Magnetic> >-> alpha_) alpha
                                |> Optic.set branching_ agent.Config.Branching
                            let probeData = agent.LaunchProbe(config, spawnBlock, agentBlock)
                            match probeData.Collision with
                            | Some(value) -> ()
                            | None -> 
                                yield agent.Spawn(
                                    config, 
                                    spawnBlock, 
                                    fun a ->
                                        agentBlock a
                                        a.NoCollisions <- true
                                        probeData.Id <- a.Id
                                        probeData.SpawnColliders()) 
                        }
                        |> Seq.tryHead
                        |> ignore



module BranchingCustom = 
    module _2BranchIndependentLeafWithStem_fs = 
        type Criterion() = 
            inherit BranchingCriterionIntervalRandomized()
            let mutable tag = 0

            member val Mu = 30.0f with get, set
            member val Sigma = 10.0f with get, set
            member val MinInterval = 15.0f with get, set
            member val Start = 25.0f with get, set
            
            override this.Tag = tag
            member this.SetTag(newTag : int) = tag <- newTag

            member this.Clone() = this.MemberwiseClone() :?> Criterion
            
            static member Mu_ : Lens<Criterion, _> = 
                (fun x -> x.Mu), (fun value x -> let y = x.Clone() in y.Mu <- value; y)
            static member Sigma_ : Lens<Criterion, _> = 
                (fun x -> x.Sigma), (fun value x -> let y = x.Clone() in y.Sigma <- value; y)
            static member MinInterval_ : Lens<Criterion, _> = 
                (fun x -> x.MinInterval), (fun value x -> let y = x.Clone() in y.MinInterval <- value; y)
            static member Start_ : Lens<Criterion, _> = 
                (fun x -> x.Start), (fun value x -> let y = x.Clone() in y.Start <- value; y)
            static member Tag_ : Lens<Criterion, _> = 
                (fun x -> x.Tag), (fun value x -> let y = x.Clone() in y.SetTag(value); y)
            
            override this.Evaluate(s, t, random) =
                if s >= this.Start then max (random.NextGaussian(this.Mu, this.Sigma)) this.MinInterval else this.Start
        
        type Config() = 
            inherit SpawnConfig()
            member val Angle = 30 with get, set
            member val Scale = 2.0f with get, set
            member val Config = DworkinAgentConfig.leafWithStem with get, set
            member val Offset = 0.0f with get, set
            member val MaxCurv = 0.08f with get, set
            member val Delay = 50 with get, set
            member val LeafLengthRange = NumberRange(12.0f, 24.0f) with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> Config
            
            static member Angle_ : Lens<Config, _> = 
                (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
            static member Scale_ : Lens<Config, _> = 
                (fun x -> x.Scale), (fun value x -> let y = x.Clone() in y.Scale <- value; y)
            static member Config_ : Lens<Config, _> = 
                (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
            static member Offset_ : Lens<Config, _> = 
                (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
            static member MaxCurv_ : Lens<Config, _> = 
                (fun x -> x.MaxCurv), (fun value x -> let y = x.Clone() in y.MaxCurv <- value; y)
            static member Delay_ : Lens<Config, _> = 
                (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
            static member LeafLengthRange_ : Lens<Config, _> = 
                (fun x -> x.LeafLengthRange), (fun value x -> let y = x.Clone() in y.LeafLengthRange <- value; y)
            
            override this.Evaluate(agent, pos) =
                if agent.Curvature <= this.MaxCurv then 
                    let agentCopy = agent.Copy()
                    agent.DworkinEnvironment.Delay(
                        (fun env ->
                            let agentConfig = this.Config |> Optic.set totalDistance_ (agent.Random.NextInRange(this.LeafLengthRange))
                            let spawnBlock = 
                                ProbeSpawnOptions(AngleDeg=float32 this.Angle, OffsetSideways=this.Offset * agent.ThicknessFactor)
                            let agentBlock (a : DworkinAgent) = 
                                let t = 0.5f
                                let s = agentCopy.Config.TotalDistance * t
                                a.ScaleBy(this.Scale * (max (agentCopy.RelativeThickness / (agentCopy.Config.Thickness.Evaluate(s, t))) 0.5f))
                                if this.Angle < 0 then a.Flip()
                            let probeData = agentCopy.LaunchProbe(agentConfig, spawnBlock, agentBlock)
                            match probeData.Collision with
                            | Some(value) -> None
                            | None -> 
                                probeData.SpawnColliders()
                                agentCopy.Spawn(
                                    agentConfig, 
                                    spawnBlock, 
                                    fun a ->
                                        agentBlock a
                                        a.NoCollisions <- true)
                                None), 
                        this.Delay)

    module _2BranchIndLeafByScale_fs = 
        type Config() = 
            inherit SpawnConfig()
            member val Angle = 15 with get, set
            member val Config = 
                { DworkinAgentConfig.Default with 
                    TotalDistance=20.0f;
                    Curvature=CurvatureFormula.DirectionPreservingTurn(A=0.024f);
                    ColliderOptions=ColliderOptionsGeneral();
                    TerminationConditions={ 
                        Collision=Some(TerminationConditionCollision(MinDistance=2.0f) |> Optic.set (primaryDetector_ >-> distance_) 1.5f);
                        Curvature=None;
                        Distance=Some(TerminationConditionDistance())
                     };
                    Thickness=ThicknessFormula.LeafPower(A=3.0f,P=0.635f,C=4.0f) } with get, set
            member val Offset = 2.0f with get, set
            member val Delay = 60 with get, set
            member val ScaleRange = Interval(0.6f, 1.0f, 0.2f) with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> Config
            
            static member Angle_ : Lens<Config, _> = 
                (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
            static member Config_ : Lens<Config, _> = 
                (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
            static member Offset_ : Lens<Config, _> = 
                (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
            static member Delay_ : Lens<Config, _> = 
                (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
            static member ScaleRange_ : Lens<Config, _> = 
                (fun x -> x.ScaleRange), (fun value x -> let y = x.Clone() in y.ScaleRange <- value; y)
            
            override this.Evaluate(agent, pos) =
                let agentCopy = agent.Copy()
                agent.DworkinEnvironment.Delay(
                    (fun env ->
                        for scale in this.ScaleRange |> Seq.rev do
                            let spawnBlock = 
                                ProbeSpawnOptions(
                                    AngleDeg=float32 this.Angle, 
                                    OffsetSideways=this.Offset * agentCopy.Thickness * 0.5f)
                            let agentBlock (a : DworkinAgent) = 
                                let t = 0.5f
                                let s = agentCopy.Config.TotalDistance * t
                                a.ScaleBy(scale * (max (agentCopy.RelativeThickness / (agentCopy.Config.Thickness.Evaluate(s, t))) 0.5f))
                                if this.Angle < 0 then a.Flip()
                            let probeData = agentCopy.LaunchProbe(this.Config, spawnBlock, agentBlock)
                            match probeData.Collision with
                            | Some(_) -> ()
                            | None -> 
                                probeData.SpawnColliders()
                                agentCopy.Spawn(
                                    this.Config, 
                                    spawnBlock, 
                                    fun a ->
                                        agentBlock a
                                        a.NoCollisions <- true)
                        None), 
                    this.Delay)



module DworkinAgentConfig =
    let plantWithSpiralBranches2 = 
        { DworkinAgentConfig.Default with 
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.Cosine(A=0.01f,B=2.06f);
            Thickness=ThicknessFormula.LeafPower(A=1.0f,P=0.4f,C=4.0f);
            TotalDistance=500.0f
         }


module SpawnConfig =
    type Probe1BranchSwirlsShifted1x() = 
        inherit SpawnConfig.Probe1BranchSwirlsShifted1()

        override this.GetModifiedAgentConfig agent alpha agentConfig =
            let newAlphaRange = NumberRange([| alpha + (this.AlphaRange.Extent * 0.05f); this.AlphaRange.Max + (this.AlphaRange.Extent * 0.1f) |])
            agentConfig
            |> Optic.set 
                (branching_<BranchingCriterionBased> >-> spawnConfig_ >-> cast<_, BranchingMultivariant.SwirlsShifted4Cosine.SpawnConfig.AgentConfig> >-> alphaRange_) 
                newAlphaRange


module LargeSwirlsStrategies =
    let swirlsMagneticMany : BranchingStrategy[] = 
        [|
            BranchingCriterionBased(
                Criterion=BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(60.0f, 150.0f),TInterval=NumberRange(0.1f, 0.9f)),
                SpawnConfig=BranchingMultivariant.SwirlsMagneticMany.SpawnConfig()
            ) :> BranchingStrategy
        |]

    let swirlsRecursive4Cosine : BranchingStrategy[] = 
        [|
            BranchingCriterionBased(
                Criterion=BranchingCriterionCustom.IntervalRandBoundedT(TInterval=NumberRange(0.1f, 0.9f)),
                SpawnConfig=BranchingMultivariant.SwirlsRecursive.SpawnConfig()
            )
        |]
    
    let swirlsRecursive4Magnetic : BranchingStrategy[] = 
        [|
            (swirlsRecursive4Cosine.[0] :?> BranchingCriterionBased)
            |> Optic.set 
                (spawnConfig_ >-> cast<_, BranchingMultivariant.SwirlsRecursive.SpawnConfig> >-> lengthRange_) 
                (NumberRange(200.0f, 600.0f))
            |> Optic.set 
                (spawnConfig_ >-> cast<_, BranchingMultivariant.SwirlsRecursive.SpawnConfig> >-> agentConfig_ >-> terminationConditions_ >-> collision_ >-> minDistance_) 
                10.0f
        |]

    let swirlsRecursive4S : BranchingStrategy[] = 
        [|
            BranchingCriterionBased(
                Criterion=(swirlsRecursive4Cosine.[0] :?> BranchingCriterionBased).Criterion,
                SpawnConfig=BranchingMultivariant.SwirlsRecursive.SpawnConfig(
                    Angle=15, 
                    LengthRange=NumberRange(200.0f, 400.0f),
                    Levels=NumberRange(1.0f, 3.0f))
            )
        |]

    let plantLike4Cosine : BranchingStrategy[] = 
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

    let swirlsShifted4Cosine : BranchingStrategy[] = 
        let spawnConfigForFirstStrat =
            (SpawnConfig.Probe1BranchSwirlsShifted1x(LengthRange=NumberRange(200.0f, 350.0f),Offset=5,Levels=NumberRange(1.0f, 5.0f)) :> SpawnConfig.Probe1BranchSwirlsShifted)
            |> Optic.set 
                (agentConfig_ >-> branching_<BranchingCriterionBased> >-> spawnConfig_) 
                (BranchingMultivariant.SwirlsShifted4Cosine.SpawnConfig.AgentConfig() :> SpawnConfig)
            |> Optic.set (agentConfig_ >-> brush_) (BrushStrategyDefault())
            |> Optic.set (agentConfig_ >-> terminationConditions_ >-> collision_ >-> handler_ >-> CollisionHandler.IgnoreParentUpTo.MinDistance_) 10.0f
            |> Optic.set (agentConfig_ >-> terminationConditions_ >-> collision_ >-> primaryDetector_ >-> distance_) 4.0f
            |> Optic.set (agentConfig_ >-> thickness_<ThicknessFormula.LeafPower_Base> >-> a_) 3.0f
            |> Optic.set (agentConfig_ >-> thickness_<ThicknessFormula.LeafPower_Base> >-> c_) 3.0f
        [|
            BranchingCriterionBased(
                Criterion=BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(30.0f, 60.0f),TInterval=NumberRange(0.1f, 0.8f)),
                SpawnConfig=spawnConfigForFirstStrat
            );

            BranchingCriterionBased(
                Criterion=BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(60.0f, 120.0f),TInterval=NumberRange(0.1f, 0.9f)),
                SpawnConfig=((SpawnConfig.Probe1BranchSwirlsShifted3(
                                Angle= -5,
                                LengthRange=NumberRange(100.0f, 200.0f),
                                LengthSteps=4,
                                AlphaRange=NumberRange(-2.4f, -1.0f),
                                AlphaSteps=4,
                                Levels=NumberRange(1.0f, 1.0f)) :> SpawnConfig.Probe1BranchSwirlsShifted)
                                |> Optic.set (agentConfig_ >-> branching_) (BranchingNone()))
            );
        |]

    let swirlsShifted4S : BranchingStrategy[] = 
        [|
            (swirlsShifted4Cosine.[0] :?> BranchingCriterionBased)
            |> Optic.set 
                criterion_ 
                (BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(80.0f, 160.0f),TInterval=NumberRange(0.25f, 0.75f)))
            |> Optic.set delay_ 50
            |> Optic.map spawnConfig_ 
                (fun x ->
                    let spawnConfig_ =
                        agentConfig_ >-> branching_<BranchingCriterionBased> >-> spawnConfig_ >-> cast<_, BranchingMultivariant.SwirlsShifted4Cosine.SpawnConfig.AgentConfig>
                    (x :?> SpawnConfig.Probe1BranchSwirlsShifted) 
                    |> Optic.set lengthRange_ (NumberRange(75.0f, 150.0f))
                    |> Optic.set lengthSteps_ 5
                    |> Optic.set alphaRange_ (NumberRange(1.0f, 2.0f))
                    |> Optic.set (spawnConfig_ >-> lengthRange_) (NumberRange(50.0f, 200.0f))
                    |> Optic.set (spawnConfig_ >-> alphaRange_) (NumberRange(0.5f, 2.0f))
                    |> Optic.set (spawnConfig_ >-> alphaSteps_) 10
                    |> Optic.set (spawnConfig_ >-> offset_) 3.0f
                    |> Optic.set (agentConfig_ >-> curvature_<CurvatureFormula.DiminishingSpiral> >-> c_) 0.5f
                    |> Optic.set (agentConfig_ >-> terminationConditions_ >-> collision_ >-> detectorSize_) DetectorSize.Absolute
                    |> Optic.set (agentConfig_ >-> terminationConditions_ >-> collision_ >-> handler_ >-> CollisionHandler.IgnoreParentUpTo.MinDistance_) 25.0f
                    |> Optic.set (agentConfig_ >-> terminationConditions_ >-> collision_ >-> primaryDetector_ >-> angle_) 60.0f
                    |> Optic.set (agentConfig_ >-> thickness_<ThicknessFormula.LeafPower_Base> >-> p_) 0.635f
                    |> Optic.set (agentConfig_ >-> thickness_<ThicknessFormula.LeafPower_Base> >-> c_) 1.5f
                    |> Optic.set offset_ 4
                    |> Optic.set levels_ (NumberRange(1.0f, 6.0f))
                    :> SpawnConfig);

            BranchingCriterionBased(
                Criterion=BranchingCriterionCustom.IntervalRandBoundedT(DRange=NumberRange(150.0f, 200.0f),TInterval=NumberRange(0.15f, 0.55f)),
                SpawnConfig=BranchingMultivariant.SwirlsShifted4S.SpawnConfig()
            );
        |]

    let swirlsShifted4Magnetic : BranchingStrategy[] = 
        [|
            (swirlsShifted4Cosine.[0] :?> BranchingCriterionBased)
            |> Optic.set (criterion_<BranchingCriterionCustom.IntervalRandBoundedT> >-> tInterval_) (NumberRange(0.1f, 0.4f))
            |> Optic.map
                (spawnConfig_ >-> cast<_, SpawnConfig.Probe1BranchSwirlsShifted> >-> agentConfig_ >-> terminationConditions_ >-> collision_)
                (fun x -> 
                    x
                    |> Optic.set (detectorSize_) DetectorSize.Absolute
                    |> Optic.set (handler_ >-> CollisionHandler.IgnoreParentUpTo.MinDistance_) 30.0f
                    |> Optic.set (primaryDetector_ >-> angle_) 90.0f
                    |> Optic.set (primaryDetector_ >-> distance_) 5.0f)
            :> BranchingStrategy;

            (swirlsShifted4Cosine.[1] :?> BranchingCriterionBased)
            |> Optic.set (criterion_<BranchingCriterionCustom.IntervalRandBoundedT> >-> tInterval_) (NumberRange(0.1f, 0.5f))
            |> Optic.set 
                (spawnConfig_ >-> cast<_, SpawnConfig.Probe1BranchSwirlsShifted> >-> agentConfig_ >-> terminationConditions_ >-> collision_ >-> primaryDetector_ >-> angle_) 
                90.0f
            |> Optic.set (spawnConfig_ >-> cast<_, SpawnConfig.Probe1BranchSwirlsShifted> >-> levels_) (NumberRange(1.0f, 5.0f))
            :> BranchingStrategy;
        |]



module LeafStrategies =
    type _2BranchIndependentLeafWithStem_fs() = 
        inherit BranchingCustom()
        member val Criterion = BranchingCustom._2BranchIndependentLeafWithStem_fs.Criterion() with get, set
        member val Config = BranchingCustom._2BranchIndependentLeafWithStem_fs.Config() with get, set
        member val Levels = NumberRange(2.0f, 20.0f) with get, set
        
        member this.Clone() = this.MemberwiseClone() :?> _2BranchIndependentLeafWithStem_fs
        
        static member Criterion_ : Lens<_2BranchIndependentLeafWithStem_fs, _> = 
            (fun x -> x.Criterion), (fun value x -> let y = x.Clone() in y.Criterion <- value; y)
        static member Config_ : Lens<_2BranchIndependentLeafWithStem_fs, _> = 
            (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
        static member Levels_ : Lens<_2BranchIndependentLeafWithStem_fs, _> = 
            (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
        
        override this.Evaluate(agent) =
            if this.Levels.Contains (float32 agent.Generation) then 
                let offset = this.Config.Offset
                for side in [| 1;  -1 |] do
                    let criterion =
                        this.Criterion
                        |> Optic.map minInterval_ (fun x -> x / (1.0f - ((agent.Curvature * (float32 side)) * offset)))
                        |> Optic.map start_ (fun x -> if side > 0 then x else x / 2.0f)
                        |> Optic.set BranchingCustom._2BranchIndependentLeafWithStem_fs.Criterion.Tag_ side
                    if criterion.Evaluate(agent) then 
                        (this.Config |> Optic.map angle_ (fun x -> x * side)).Evaluate(agent)
                        agent.AddBranchMarkFor(BranchMark(s=agent.S), side)
    
    
    type _2BranchIndLeafByScale_fs() = 
        inherit BranchingCustom()
        member val Criterion = 
            BranchingCustom._2BranchIndependentLeafWithStem_fs.Criterion()
            |> Optic.set BranchingCustom._2BranchIndependentLeafWithStem_fs.Criterion.Mu_ 10.0f
            |> Optic.set BranchingCustom._2BranchIndependentLeafWithStem_fs.Criterion.Sigma_ 2.0f
            |> Optic.set minInterval_ 5.0f
            |> Optic.set start_ 15.0f with get, set
        member val Config = BranchingCustom._2BranchIndLeafByScale_fs.Config() with get, set
        member val Levels = NumberRange(2.0f, 20.0f) with get, set
        
        member this.Clone() = this.MemberwiseClone() :?> _2BranchIndLeafByScale_fs
        
        static member Criterion_ : Lens<_2BranchIndLeafByScale_fs, _> = 
            (fun x -> x.Criterion), (fun value x -> let y = x.Clone() in y.Criterion <- value; y)
        static member Config_ : Lens<_2BranchIndLeafByScale_fs, _> = 
            (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
        static member Levels_ : Lens<_2BranchIndLeafByScale_fs, _> = 
            (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
        
        override this.Evaluate(agent) =
            if this.Levels.Contains (float32 agent.Generation) then 
                let offset = this.Config.Offset
                for side in [| 1;  -1 |] do
                    let criterion = 
                        this.Criterion 
                        |> Optic.map minInterval_ (fun x -> x / (1.0f - ((agent.Curvature * (float32 side)) * offset)))
                        |> Optic.set BranchingCustom._2BranchIndependentLeafWithStem_fs.Criterion.Tag_ side
                    if criterion.Evaluate(agent) then 
                        (this.Config |> Optic.map angle_ (fun x -> x * side)).Evaluate(agent)
                        agent.AddBranchMarkFor(BranchMark(s=agent.S), side)


    let leaves2SidesProbesStraight1_fs = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionIntervalRandomized.Uniform(IntervalRange=NumberRange(20.0f, 25.0f)),
            Delay=50,
            SpawnConfig=BranchingCriterionBased.Leaves2SidesProbesStraight1_fs.SpawnConfig()
        )

    let leaves2SidesProbes1_2_fs = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionInterval.ConstantBoundedT(D=15.0f,Start=0.05f,End=0.85f),
            Delay=50,
            SpawnConfig=((leaves2SidesProbesStraight1_fs.SpawnConfig :?> BranchingCriterionBased.Leaves2SidesProbesStraight1_fs.SpawnConfig)
                         |> Optic.set angle_ 20
                         |> Optic.set steps_ 1
                         |> Optic.set (config_ >-> curvature_<CurvatureFormula.Constant> >-> c_) 0.06f
                         |> Optic.set (config_ >-> terminationConditions_ >-> collision_ >-> minDistance_) 4.0f
                         |> Optic.set offset_ 2
                         |> Optic.set BranchingCriterionBased.Leaves2SidesProbesStraight1_fs.SpawnConfig.MaxCurvature_ Single.Pi
                         :> SpawnConfig)
        )
    
    let leavesProbesCurved1_fs = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionInterval.ConstantBoundedT(D=25.0F,Start=0.05f,End=0.95f),
            Delay=50,
            SpawnConfig=((leaves2SidesProbesStraight1_fs.SpawnConfig :?> BranchingCriterionBased.Leaves2SidesProbesStraight1_fs.SpawnConfig)
                         |> Optic.set angle_ 20
                         |> Optic.set steps_ 1
                         |> Optic.set scaleRange_ (NumberRange(0.8f, 1.6f))
                         |> Optic.set (config_ >-> curvature_) (CurvatureFormula.Linear(A=1.045f,B= -0.018f))
                         |> Optic.set (config_ >-> terminationConditions_ >-> collision_ >-> minDistance_) 4.0f
                         |> Optic.set (config_ >-> thickness_<ThicknessFormula.LeafPower_Base> >-> p_) 0.78f
                         |> Optic.set (config_ >-> totalDistance_) 33.0f
                         |> Optic.set offset_ 1
                         |> Optic.set levels_ (NumberRange(1.0f, 20.0f))
                        )
        )

    let leavesProbesFollowCurv_fs = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionInterval.ConstantBoundedT(D=8.0f,Start=0.05f,End=0.45f),
            Delay=50,
            SpawnConfig=BranchingCriterionBased.LeavesProbesFollowCurv_fs.SpawnConfig()
        )


type CosineSwirl() = 
    inherit DworkinEnvironmentConfigCustom()
    member val AgentConfig = 
        DworkinAgentConfig.plantWithSpiralBranches2 
        |> Optic.set totalDistance_ 650.0f with get, set
    member val LargeSwirls : BranchingStrategy[] = [| |] with get, set
    member val Shift = 0.29f with get, set
    member val Thickness = 1.0f with get, set
    member val LeafStrategy : BranchingStrategy = BranchingNone() :> BranchingStrategy with get, set
    member val IsDouble = false with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> CosineSwirl
    
    static member LargeSwirls_ : Lens<CosineSwirl, _> = 
        (fun x -> x.LargeSwirls), (fun value x -> let y = x.Clone() in y.LargeSwirls <- value; y)
    static member Thickness_ : Lens<CosineSwirl, _> = 
        (fun x -> x.Thickness), (fun value x -> let y = x.Clone() in y.Thickness <- value; y)
    static member LeafStrategy_ : Lens<CosineSwirl, _> = 
        (fun x -> x.LeafStrategy), (fun value x -> let y = x.Clone() in y.LeafStrategy <- value; y)
    static member IsDouble_ : Lens<CosineSwirl, _> = 
        (fun x -> x.IsDouble), (fun value x -> let y = x.Clone() in y.IsDouble <- value; y)

    override this.Evaluate(env) =
        let width = env.Bounds.Width
        let height = env.Bounds.Height

        Util.buildWalls env

        let angle = if width > height then -Single.HalfPi else 0.0f
        let scale = (max width height) / 640.0f
        let offset = if this.IsDouble then 0.0f else 0.06f

        let spawnAgent(pos : Point, angleShift) =
            let spawnOptions = ProbeSpawnOptions(Position=pos, Angle=angle + angleShift)
            let agentBlock (a : DworkinAgent) = a.ScaleBy(Point(scale * this.Thickness, scale))
            let probeData = env.LaunchProbe(this.AgentConfig, spawnOptions, agentBlock)
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

        if this.IsDouble then
            spawnAgent(
                (if width > height then env.Bounds.LeftCenter + Point(1.0f, height * (this.Shift + offset)) 
                 else env.Bounds.BottomCenter + Point(width * (this.Shift + offset), -1.0f)), 
                0.0f)
            spawnAgent(
                (if width > height then env.Bounds.RightCenter + Point(-1.0f, height * (offset - this.Shift)) 
                 else env.Bounds.TopCenter + Point(width * (offset - this.Shift), 1.0f)), 
                Single.Pi)
        else
            let pos = 
                if width > height then env.Bounds.LeftCenter + Point(1.0f, height * offset) 
                else env.Bounds.BottomCenter + Point(width * offset, -1.0f)
            spawnAgent(pos, 0.0f)



type MagneticSpiralsBunchFromEdge() = 
    inherit DworkinEnvironmentConfigCustom()
    member val MidpointInterval = Interval(0.1f, 0.5f, 0.05f) with get, set
    member val AlphaInterval = Interval(0.75f, 0.8f, 0.025f) with get, set
    member val LengthInterval = Interval(600.0f, 1200.0f, 100.0f) with get, set
    member val Angle = 30 with get, set
    member val AgentConfig = DworkinAgentConfig.swirlMagnetic with get, set
    member val FlipFactors = [| 1 |] with get, set
    member val LargeSwirls : BranchingStrategy[] = [| |] with get, set
    member val N = 3 with get, set
    member val LeafStrategy : BranchingStrategy = BranchingNone() :> BranchingStrategy with get, set
    member val Thickness = 1.0f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> MagneticSpiralsBunchFromEdge
    
    static member MidpointInterval_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.MidpointInterval), (fun value x -> let y = x.Clone() in y.MidpointInterval <- value; y)
    static member AlphaInterval_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.AlphaInterval), (fun value x -> let y = x.Clone() in y.AlphaInterval <- value; y)
    static member LengthInterval_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.LengthInterval), (fun value x -> let y = x.Clone() in y.LengthInterval <- value; y)
    static member Angle_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
    static member AgentConfig_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member FlipFactors_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.FlipFactors), (fun value x -> let y = x.Clone() in y.FlipFactors <- value; y)
    static member LargeSwirls_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.LargeSwirls), (fun value x -> let y = x.Clone() in y.LargeSwirls <- value; y)
    static member N_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.N), (fun value x -> let y = x.Clone() in y.N <- value; y)
    static member LeafStrategy_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.LeafStrategy), (fun value x -> let y = x.Clone() in y.LeafStrategy <- value; y)
    static member Thickness_ : Lens<MagneticSpiralsBunchFromEdge, _> = 
        (fun x -> x.Thickness), (fun value x -> let y = x.Clone() in y.Thickness <- value; y)
    
    override this.Evaluate(env) =
        let c = env.Bounds.Center
        
        Util.buildWalls env

        let pos = 
            if env.Bounds.Width > env.Bounds.Height then 
                env.Bounds.LeftCenter + Point(1.0f, 0.0f)
            else 
                env.Bounds.BottomCenter - Point(0.0f, 1.0f)
        let angleShift = if env.Bounds.Width > env.Bounds.Height then -90 else 0
        let scale = (max env.Bounds.Extent.X env.Bounds.Extent.Y) / 640.0f
        
        let combinations = 
            [| for x1 in this.MidpointInterval do
                for x2 in this.AlphaInterval do
                    for x3 in this.LengthInterval do
                        for x4 in this.FlipFactors do yield x1, x2, x3, x4 |].ShuffleBy(env.Random)
        let mutable i = 0
        seq {
            for midpoint, alpha, length, flipFactor in combinations do
                if i >= this.N then 
                    yield ()
                let spawnOptions = 
                    ProbeSpawnOptions(
                        AngleDeg=float32((this.Angle * flipFactor) + angleShift),
                        Position=pos,
                        StepsBetweenCollisionCheck=int(floor (this.AgentConfig.TerminationConditions.Collision.Value.PrimaryDetector.Distance))
                    )
                let agentBlock (a : DworkinAgent) =
                    a.ScaleBy(Point(scale * this.Thickness, scale))
                    if flipFactor < 0 then a.Flip()
                let conf = 
                    this.AgentConfig
                    |> Optic.set (curvature_<CurvatureFormula.MagneticWithFlip> >-> midpoint_) midpoint
                    |> Optic.set (curvature_<CurvatureFormula.MagneticWithFlip> >-> alpha_) alpha
                    |> Optic.set totalDistance_ length
                    |> Optic.set branching_ (BranchingMultivariant(Strategies=Array.append this.LargeSwirls [| this.LeafStrategy |]))
                let probeData = env.LaunchProbe(conf, spawnOptions, agentBlock)
                if (Option.isNone probeData.Collision) && (probeData.S > (length * 0.9f)) then 
                    probeData.SpawnColliders()
                    env.Spawn(
                        conf, 
                        spawnOptions, 
                        fun a ->
                            agentBlock a
                            a.NoCollisions <- true)
                        |> ignore
                    i <- i + 1
            }
            |> Seq.tryHead
            |> ignore


let magneticSpiralsBunch = MagneticSpiralsBunchFromEdge()

let magneticSpiralSingle = 
    MagneticSpiralsBunchFromEdge(
        MidpointInterval=Interval(0.2f, 0.4f, 0.05f), 
        AlphaInterval=Interval(0.75f, 0.8f, 0.05f),
        LengthInterval=Interval(900.0f, 1200.0f, 100.0f),
        N=1)
        |> Optic.set (agentConfig_ >-> terminationConditions_ >-> collision_ >-> primaryDetector_ >-> angle_) 180.0f
        |> Optic.set (agentConfig_ >-> terminationConditions_ >-> collision_ >-> primaryDetector_ >-> distance_) 60.0f