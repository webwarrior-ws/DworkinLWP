module rec Vine


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


[<AbstractClass>]
type SpawnConfigWithScaleRange() =
    inherit LibDworkin.Configs.SpawnConfig()
    abstract ScaleRange : NumberRange with get, set
    abstract WithScaleRange : NumberRange -> SpawnConfigWithScaleRange


module BranchingCriterionBased = 
    module LeavesProbesCurved1_vines = 
        type SpawnConfig() = 
            inherit SpawnConfigWithScaleRange()
            member val Angle = 20 with get, set
            member val Steps = 1 with get, set
            override val ScaleRange = NumberRange(0.8f, 1.6f) with get, set
            member val Config = 
                { DworkinAgentConfig.Default with 
                    ColliderOptions=ColliderOptionsGeneral();
                    Curvature=CurvatureFormula.Linear(A=1.045f, B= -0.018f);
                    TerminationConditions={ 
                        Collision=
                            Some(TerminationConditionCollision(IgnoreParent=true,MinDistance=4.0f,SelfIntersection=false) 
                                 |> Optic.set (primaryDetector_ >-> distance_) 2.0f);
                        Curvature=None;
                        Distance=Some(TerminationConditionDistance())
                     };
                    Thickness=ThicknessFormula.LeafPower(A=2.0f,P=0.78f,C=5.0f);
                    TotalDistance=33.0f
                 } with get, set
            member val Offset = 1.0f with get, set
            member val Levels = NumberRange(1.0f, 20.0f) with get, set
            
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

            override this.WithScaleRange(newRange) = (this |> Optic.set SpawnConfig.ScaleRange_ newRange) :> SpawnConfigWithScaleRange
            
            override this.Evaluate(agent, pos) =
                if this.Levels.Contains (float32 agent.Generation) then 
                    let angles = ResizeArray([| float32 this.Angle; -(float32 this.Angle) |])
                    for scale in this.ScaleRange.By(this.ScaleRange.Extent / (float32 this.Steps)) |> Seq.rev do
                        match angles with
                        | NotEmpty(_) -> 
                            for phi in angles.ToArray() do
                                let agentBlock = fun (a : DworkinAgent) -> if (sign phi) < 0 then a.Flip()
                                let probeData = 
                                    agent.LaunchProbe(
                                        this.Config 
                                        |> Optic.map (terminationConditions_ >-> collision_ >-> minDistance_) (fun x -> x / scale), 
                                        ProbeSpawnOptions(
                                            AngleDeg=phi, 
                                            OffsetSideways=this.Offset * agent.ThicknessFactor, 
                                            StepsBetweenCollisionCheck=2),
                                        fun (a : DworkinProbeAgent) ->
                                            agentBlock a
                                            a.ScaleBy(Point(1.0f, scale)))
                                match probeData.Collision with
                                | Some(_) -> ()
                                | None -> 
                                    angles.Remove(phi) |> ignore
                                    agent.Spawn(
                                        this.Config, 
                                        ProbeSpawnOptions(AngleDeg=phi, OffsetSideways=this.Offset * agent.ThicknessFactor), 
                                        fun a ->
                                            agentBlock a
                                            a.ScaleBy(Point(1.0f, scale))
                                            a.NoCollisions <- true
                                            probeData.Id <- a.Id
                                            probeData.SpawnColliders())
                        | Empty -> ()
    
    
    module Vine3 = 
        type Criterion() = 
            inherit BranchingCriterionCustom()
            member val DRange = NumberRange(15.0f, 25.0f) with get, set
            member val TInterval = NumberRange(0.2f, 0.6f) with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> Criterion
            
            static member DRange_ : Lens<Criterion, _> = 
                (fun x -> x.DRange), (fun value x -> let y = x.Clone() in y.DRange <- value; y)
            static member TInterval_ : Lens<Criterion, _> = 
                (fun x -> x.TInterval), (fun value x -> let y = x.Clone() in y.TInterval <- value; y)
            
            override this.Evaluate(agent, mark, tag) =
                let d = agent.Attributes.BranchOffset.GetOrSetIfAbsent(fun () -> agent.Random.NextInRange(this.DRange))
                (mark.S + d) < agent.S && this.TInterval.Contains(agent.T)


    let vine1 = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionCustom.Interval_MaxBranches(D=20.0f,N=3),
            SpawnConfig=DworkinAgentConfig.Vine1.Branching.SpawnConfig()
        )

    let vine2 = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionIntervalRandomized.Gaussian(Mu=15.0f, Sigma=10.0f),
            SpawnConfig=DworkinAgentConfig.Vine1.Branching.SpawnConfig(Angle=0, AngleSpread=5, AngleSteps=3, ScaleRange=NumberRange(1.0f, 1.6f), ScaleSteps=3)
        )

    let vine3 = 
        BranchingCriterionBased(
            Criterion=Vine3.Criterion(),
            SpawnConfig=DworkinAgentConfig.Vine1.Branching.SpawnConfig(Angle=0, AngleSpread=60, ScaleRange=NumberRange(0.6f, 1.4f), ScaleSteps=4)
        )

    let leavesProbesCurved1_vines = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionInterval.ConstantBoundedT(D=25.0f,Start=0.05f,End=0.65f),
            Delay=30,
            SpawnConfig=LeavesProbesCurved1_vines.SpawnConfig()
        )

    let leaves2SidesProbes1_2_vines = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionInterval.ConstantBoundedT(D=15.0f,Start=0.05f,End=0.85f),
            Delay=30,
            SpawnConfig = (
                LeavesProbesCurved1_vines.SpawnConfig(ScaleRange=NumberRange(0.6f, 1.0f), Offset=2.0f)
                |> Optic.set (config_ >-> DworkinAgentConfig.Curvature_) (CurvatureFormula.Constant(C=0.06f) :> CurvatureFormula)
                |> Optic.set (config_ >-> DworkinAgentConfig.Thickness_ >-> cast<ThicknessFormula, ThicknessFormula.LeafPower_Base> >-> p_) 0.5f
                |> Optic.set (config_ >-> totalDistance_) 20.0f)
        )

    let leaves2SidesProbes2_vines = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionIntervalRandomized.Uniform(IntervalRange=NumberRange(6.0f, 8.0f)),
            SpawnConfig=SpawnConfig.Probe2BranchByScale(Angle=20,Steps=2,ScaleRange=NumberRange(1.0f, 1.4f),
                Config={ DworkinAgentConfig.Default with 
                            Brush=BrushStrategySolid(AlignWithParent=true);
                            ColliderOptions=ColliderOptionsGeneral();
                            Curvature=CurvatureFormula.Constant(C=0.15f);
                            Speed=AgentSpeedConstant(Magnitude=0.25f);
                            TerminationConditions={ 
                                Collision=Some(TerminationConditionCollision(IgnoreParent=true,MinDistance=3.0f,SelfIntersection=false));
                                Curvature=None;
                                Distance=Some(TerminationConditionDistance.Instance)
                             };
                            Thickness=ThicknessFormula.LeafPower_Base(A=3.0f,P=0.785f,C=3.5f);
                            TotalDistance=8.0f
                        }))

    let leaves2SidesProbesStraigh1_vines = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionIntervalRandomized.Uniform(IntervalRange=NumberRange(20.0f, 25.0f)),
            SpawnConfig=BranchingCriterionBased.LeavesProbesCurved1_vines.SpawnConfig(
                Angle=30, Steps=2, ScaleRange=NumberRange(0.6f, 1.0f), Offset=3.0f, 
                Config={ DworkinAgentConfig.Default with 
                            ColliderOptions=ColliderOptionsGeneral();
                            Curvature=CurvatureFormula.Constant(C=0.0f);
                            TerminationConditions={ 
                                Collision=
                                    Some(TerminationConditionCollision(IgnoreParent=true,MinDistance=0.0f,SelfIntersection=false) 
                                         |> Optic.set (primaryDetector_ >-> distance_) 2.0f);
                                Curvature=None;
                                Distance=Some(TerminationConditionDistance())
                             };
                            Thickness=ThicknessFormula.LeafPower(A=2.0f,P=0.5f,C=5.0f);
                            TotalDistance=20.0f
                        }))


module BranchingCustom = 
    module _2BranchIndLeafAlt_vines = 
        type Criterion() = 
            inherit BranchingCriterionIntervalRandomized()
            
            let mutable tag = None
            override this.Tag = defaultArg tag base.Tag
            member this.SetTag(value) = tag <- Some(value)

            member val Mu = 25.0f with get, set
            member val Sigma = 5.0f with get, set
            member val MinInterval = 15.0f with get, set
            member val Start = 25.0f with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> Criterion
            
            static member Mu_ : Lens<Criterion, _> = 
                (fun x -> x.Mu), (fun value x -> let y = x.Clone() in y.Mu <- value; y)
            static member Sigma_ : Lens<Criterion, _> = 
                (fun x -> x.Sigma), (fun value x -> let y = x.Clone() in y.Sigma <- value; y)
            static member MinInterval_ : Lens<Criterion, _> = 
                (fun x -> x.MinInterval), (fun value x -> let y = x.Clone() in y.MinInterval <- value; y)
            static member Start_ : Lens<Criterion, _> = 
                (fun x -> x.Start), (fun value x -> let y = x.Clone() in y.Start <- value; y)
            
            override this.Evaluate(s, t, random) =
                if s >= this.Start then max (random.NextGaussian(this.Mu, this.Sigma)) this.MinInterval else this.Start
        
        type Config() = 
            inherit SpawnConfig()
            member val Angle = 75 with get, set
            member val Scale = 2.0f with get, set
            member val Config = 
                DworkinAgentConfig.leafWithStem
                |> Optic.set curvature_ (CurvatureFormula.Constant(C=0.0f))
                |> Optic.set speed_ (AgentSpeedConstant(Magnitude=0.5f))
                |> Optic.set (terminationConditions_ >-> collision_ >-> minDistance_) 0.0f
                |> Optic.set thickness_ (ThicknessFormula.LeafPower(A=2.0f,P=0.505f))
                |> Optic.set totalDistance_ 8.0f with get, set
            member val Offset = 2.5f with get, set
            member val MaxCurv = 0.08f with get, set
            member val Delay = 25 with get, set
            member val LeafLengthRange = NumberRange(7.0f, 10.0f) with get, set
            
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
                            let spawnOptions = ProbeSpawnOptions(AngleDeg=float32 this.Angle, 
                                                                 OffsetSideways=this.Offset * agent.ThicknessFactor)
                            let agentBlock (a : DworkinAgent) =
                                let s = agentCopy.Config.TotalDistance / 2.0f
                                let t = 0.5f 
                                let thickness = agentCopy.Config.Thickness.Evaluate(s, t)
                                a.ScaleBy(this.Scale * agentCopy.RelativeThickness / thickness)
                                if this.Angle < 0 then a.Flip()
                            let probeData = agentCopy.LaunchProbe(agentConfig, spawnOptions, agentBlock)
                            if probeData.Collision.IsNone then
                                probeData.SpawnColliders()
                                agentCopy.Spawn(
                                    agentConfig, 
                                    spawnOptions, 
                                    fun a ->
                                        agentBlock a
                                        a.NoCollisions <- true)
                            None), 
                        this.Delay)
        
        
    type _2BranchIndLeafAlt_vines() = 
        inherit BranchingCustom()
        member val Criterion = _2BranchIndLeafAlt_vines.Criterion() with get, set
        member val Config = _2BranchIndLeafAlt_vines.Config() with get, set
        member val Levels = NumberRange(1.0f, 20.0f) with get, set
    
        member this.Clone() = this.MemberwiseClone() :?> _2BranchIndLeafAlt_vines
    
        static member Criterion_ : Lens<_2BranchIndLeafAlt_vines, _> = 
            (fun x -> x.Criterion), (fun value x -> let y = x.Clone() in y.Criterion <- value; y)
        static member Config_ : Lens<_2BranchIndLeafAlt_vines, _> = 
            (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
        static member Levels_ : Lens<_2BranchIndLeafAlt_vines, _> = 
            (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
    
        override this.Evaluate(agent) =
            if this.Levels.Contains(float32 agent.Generation) then 
                let offset = this.Config.Offset
                for side in [| 1;  -1 |] do
                    let criterion = 
                        this.Criterion
                        |> Optic.map minInterval_ (fun x -> x / (1.0f - (agent.Curvature * (float32 side) * offset)))
                        |> Optic.map start_ (fun x -> if side > 0 then x else x / 2.0f)
                    criterion.SetTag(side)
                    if criterion.Evaluate(agent) then 
                        (this.Config |> Optic.map angle_ (fun x -> x * side)).Evaluate(agent)
                        agent.AddBranchMarkFor(BranchMark(agent.S), side)
                    

    module _2BranchIndLeafByScale_vines =         
        type Config() = 
            inherit SpawnConfig()
            member val Angle = 15 with get, set
            member val Config = 
                { DworkinAgentConfig.Default with 
                    ColliderOptions=ColliderOptionsGeneral();
                    Curvature=CurvatureFormula.DirectionPreservingTurn(A=0.024f);
                    TerminationConditions={ 
                        Collision=Some(TerminationConditionCollision(MinDistance=2.0f) |> Optic.set (primaryDetector_ >-> distance_) 1.5f);
                        Curvature=None;
                        Distance=Some(TerminationConditionDistance())
                     };
                    Thickness=ThicknessFormula.LeafPower(A=3.0f,P=0.635f,C=4.0f);
                    TotalDistance=25.0f
                 } with get, set
            member val Offset = 2.0f with get, set
            member val Delay = 20 with get, set
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
                            let spawnOptions = ProbeSpawnOptions(AngleDeg=float32 this.Angle, OffsetSideways=this.Offset * agent.ThicknessFactor)
                            let agentBlock (a : DworkinAgent) =
                                let s = agentCopy.Config.TotalDistance / 2.0f
                                let t = 0.5f 
                                let thickness = agentCopy.Config.Thickness.Evaluate(s, t)
                                a.ScaleBy(scale * agentCopy.RelativeThickness / thickness)
                                if this.Angle < 0 then a.Flip()
                            let probeData = agentCopy.LaunchProbe(this.Config, spawnOptions, agentBlock)
                            if probeData.Collision.IsNone then
                                probeData.SpawnColliders()
                                agentCopy.Spawn(
                                    this.Config, 
                                    spawnOptions, 
                                    fun a ->
                                        agentBlock a
                                        a.NoCollisions <- true)
                        None), 
                    this.Delay)
        
        
    type _2BranchIndLeafByScale_vines() = 
        inherit BranchingCustom()
        member val Criterion = _2BranchIndLeafAlt_vines.Criterion(Mu=10.0f, Sigma=2.0f, MinInterval=5.0f, Start=15.0f) with get, set
        member val Config = _2BranchIndLeafByScale_vines.Config() with get, set
        member val Levels = NumberRange(1.0f, 20.0f) with get, set
    
        member this.Clone() = this.MemberwiseClone() :?> _2BranchIndLeafByScale_vines
    
        static member Criterion_ : Lens<_2BranchIndLeafByScale_vines, _> = 
            (fun x -> x.Criterion), (fun value x -> let y = x.Clone() in y.Criterion <- value; y)
        static member Config_ : Lens<_2BranchIndLeafByScale_vines, _> = 
            (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
        static member Levels_ : Lens<_2BranchIndLeafByScale_vines, _> = 
            (fun x -> x.Levels), (fun value x -> let y = x.Clone() in y.Levels <- value; y)
    
        override this.Evaluate(agent) =
            if this.Levels.Contains(float32 agent.Generation) then 
                let offset = this.Config.Offset
                for side in [| 1;  -1 |] do
                    let criterion = 
                        this.Criterion
                        |> Optic.map minInterval_ (fun x -> x / (1.0f - (agent.Curvature * (float32 side) * offset)))
                    criterion.SetTag(side)
                    if criterion.Evaluate(agent) then 
                        (this.Config |> Optic.map angle_ (fun x -> x * side)).Evaluate(agent)
                        agent.AddBranchMarkFor(BranchMark(agent.S), side)
                    

    let _2BranchIndLeafWithStem_vines = 
        _2BranchIndLeafAlt_vines(
            Criterion=_2BranchIndLeafAlt_vines.Criterion(Mu=30.0f, Sigma=10.0f),
            Config=(_2BranchIndLeafAlt_vines.Config(Angle=30, Offset=0.0f, LeafLengthRange=NumberRange(12.0f, 20.0f))
                    |> Optic.set (config_ >-> curvature_) (CurvatureFormula.Linear(A=0.69f, B=0.085f))
                    |> Optic.set (config_ >-> DworkinAgentConfig.Speed_) (AgentSpeedAdaptive.Instance :> AgentSpeed)
                    |> Optic.set (config_ >-> terminationConditions_ >-> collision_ >-> minDistance_) 2.5f
                    |> Optic.set (config_ >-> thickness_) (ThicknessFormula.LeafPowerWithStem(A=3.0f, P=0.33f, C=5.0f, Sl=0.25f, St=0.1f))
                    |> Optic.set (config_ >-> totalDistance_) 12.0f ))


module DworkinAgentConfig = 
    module Vine1 = 
        module Branching = 
            type SpawnConfig() = 
                inherit SpawnConfigWithScaleRange()
                member val Angle = 25 with get, set
                member val GenLimit = 20 with get, set
                member val AngleSpread = 90 with get, set
                member val AngleSteps = 5 with get, set
                override val ScaleRange = NumberRange(1.0f, 2.0f) with get, set
                member val ScaleSteps = 5 with get, set

                override this.WithScaleRange(newRange) = (this |> Optic.set SpawnConfig.ScaleRange_ newRange) :> SpawnConfigWithScaleRange
                
                member this.Clone() = this.MemberwiseClone() :?> SpawnConfig
                
                static member Angle_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
                static member GenLimit_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.GenLimit), (fun value x -> let y = x.Clone() in y.GenLimit <- value; y)
                static member AngleSpread_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.AngleSpread), (fun value x -> let y = x.Clone() in y.AngleSpread <- value; y)
                static member AngleSteps_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.AngleSteps), (fun value x -> let y = x.Clone() in y.AngleSteps <- value; y)
                static member ScaleRange_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.ScaleRange), (fun value x -> let y = x.Clone() in y.ScaleRange <- value; y)
                static member ScaleSteps_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.ScaleSteps), (fun value x -> let y = x.Clone() in y.ScaleSteps <- value; y)
                
                override this.Evaluate(agent, pos) =
                    agent.GenerationLimit <- this.GenLimit
                    let angles = ResizeArray([| this.Angle |])
                    for delta in 0 .. (this.AngleSpread / this.AngleSteps) .. this.AngleSpread  do
                        angles.Add(this.Angle + delta)
                        angles.Add(this.Angle - delta)
                    angles.ShuffleBy(agent.Random) |> ignore
                    
                    seq { 
                        for phi in angles do 
                            for scale in this.ScaleRange.By(this.ScaleRange.Extent / (float32 this.ScaleSteps)) |> Seq.rev do 
                                yield phi, scale }
                    |> Seq.exists 
                        (fun (phi, scale) ->
                            let agentBlock = 
                                fun (a : DworkinAgent) ->
                                    a.LengthFactor <- scale
                                    a.Flip()
                            let probeData = 
                                agent.LaunchProbe(
                                    agent.Config, 
                                    ProbeSpawnOptions(AngleDeg=float32 phi, StepsBetweenCollisionCheck=2), 
                                    agentBlock)
                            match probeData.Collision with
                            | Some(_) -> false
                            | None -> 
                                agent.Spawn(
                                    agent.Config, 
                                    ProbeSpawnOptions(AngleDeg=float32 phi), 
                                    fun a ->
                                        agentBlock a
                                        a.NoCollisions <- true
                                        probeData.SpawnColliders())
                                true (* return *) )
                    |> ignore
    

    module VineSplitting1 = 
        module Termination = 
            type SpawnConfig() = 
                inherit LibDworkin.Configs.SpawnConfig()
                member val GenLimit = 20 with get, set
                member val PBothSides = 0.75f with get, set
                member val LengthRange = NumberRange(50.0f, 100.0f) with get, set
                member val CurvRange = Interval(0.01f, 0.04f, 0.01f) with get, set
                
                member this.Clone() = this.MemberwiseClone() :?> SpawnConfig
                
                static member GenLimit_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.GenLimit), (fun value x -> let y = x.Clone() in y.GenLimit <- value; y)
                static member PBothSides_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.PBothSides), (fun value x -> let y = x.Clone() in y.PBothSides <- value; y)
                static member LengthRange_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
                static member CurvRange_ : Lens<SpawnConfig, _> = 
                    (fun x -> x.CurvRange), (fun value x -> let y = x.Clone() in y.CurvRange <- value; y)
                
                override this.Evaluate(agent, pos) =
                    agent.GenerationLimit <- this.GenLimit
                    let mutable flipValues = [| true |]
                    let mutable config = agent.Config
                    if (agent.Random.NextFloat()) < this.PBothSides then flipValues <- [| true; false |]
                    for flip in flipValues do
                        let agentBlock (a : DworkinAgent) = if flip then a.Flip()
                        config <- config |> Optic.set totalDistance_ (agent.Random.NextInRange(this.LengthRange))
                        (this.CurvRange |> Seq.toArray).ShuffleBy(agent.Random)
                            |> Array.exists
                            (fun c ->
                                config <- config |> Optic.set (curvature_ >-> cast<CurvatureFormula, CurvatureFormula.Cosine> >-> a_) c
                                let probeData = agent.LaunchProbe(config, ProbeSpawnOptions(), agentBlock)
                                if probeData.S >= this.LengthRange.Min then 
                                    agent.Spawn(
                                        config |> Optic.set totalDistance_ probeData.S, 
                                        AgentSpawnOptions(), 
                                        fun a ->
                                            probeData.Id <- a.Id
                                            probeData.SpawnColliders()
                                            agentBlock a
                                            a.NoCollisions <- true)
                                    true
                                else 
                                    false)
                            |> ignore
                        

    let singleVineVarying = 
        { DworkinAgentConfig.Default with 
            Branching=BranchingMultivariant();
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.DirectionPreservingTurn(A=0.012f);
            Speed=AgentSpeedConstant();
            Termination=TerminationSpawn(SpawnConfig=SpawnConfig.ProbeSplitRecursive());
            TerminationConditions={ 
                Collision=Some(
                    TerminationConditionCollision(
                        Handler=Some(CollisionHandler.IgnoreSiblingUpTo(MinT=0.33f) :> CollisionHandler),
                        IgnoreParent=true,
                        MinDistance=1.0f,
                        SelfIntersection=false)
                    |> Optic.set (primaryDetector_ >-> angle_) 60.0f
                    |> Optic.set (primaryDetector_ >-> distance_) 15.0f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance.Instance)
             };
            Thickness=ThicknessFormula.Constant(C=2.0f);
            TotalDistance=1.0f
         }
            
    let vine1 = 
        { DworkinAgentConfig.Default with 
            Branching=BranchingMultivariant(Strategies=[| BranchingCriterionBased.vine1 |]);
            Brush=BrushStrategySolid();
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.DoubleLogSpiralSimple(A=0.0174f,D=0.182f,B=0.0751f);
            Speed=AgentSpeedConstant();
            TerminationConditions={ 
                Collision=Some(
                    TerminationConditionCollision(DetectorSize=DetectorSize.Absolute,MinDistance=2.5f,SelfIntersection=false)
                    |> Optic.set (primaryDetector_ >-> angle_) 90.0f
                    |> Optic.set (primaryDetector_ >-> distance_) 15.0f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance.Instance)
             };
            Thickness=ThicknessFormula.Constant(C=3.0f);
            TotalDistance=111.0f
         }

    let vine2 = 
        { DworkinAgentConfig.Default with 
            Branching=BranchingMultivariant(Strategies=[| BranchingCriterionBased.vine2 |]);
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.Magnetic(Alpha=0.75f);
            Speed=AgentSpeedConstant();
            TerminationConditions={ 
                Collision=Some(
                    TerminationConditionCollision(DetectorSize=DetectorSize.Absolute,MinDistance=5.0f,SelfIntersection=false)
                    |> Optic.set (primaryDetector_ >-> angle_) 60.0f
                    |> Optic.set (primaryDetector_ >-> distance_) 20.0f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance.Instance)
             };
            Thickness=ThicknessFormula.PointyTip(A=2.0f,K=0.05f)
         }

    let vine3 = 
        { DworkinAgentConfig.Default with 
            Branching=BranchingMultivariant(Strategies=[| BranchingCriterionBased.vine3 |]);
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.Quadratic(A= -1.189f,C=0.034f,B=0.116f,K=0.5f);
            Speed=AgentSpeedConstant();
            TerminationConditions={ 
                Collision=Some(
                    TerminationConditionCollision(DetectorSize=DetectorSize.Absolute,MinDistance=10.0f,SelfIntersection=false)
                    |> Optic.set (primaryDetector_ >-> angle_) 120.0f
                    |> Optic.set (primaryDetector_ >-> distance_) 20.0f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance.Instance)
             };
            Thickness=ThicknessFormula.Constant(C=2.0f)
         }

    let vineSplitting1 = 
        { DworkinAgentConfig.Default with 
            Branching=BranchingMultivariant();
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.Cosine(A=0.01f);
            Speed=AgentSpeedConstant();
            Termination=TerminationSpawn(SpawnConfig=DworkinAgentConfig.VineSplitting1.Termination.SpawnConfig());
            TerminationConditions={ 
                Collision=Some(
                    TerminationConditionCollision(DetectorSize=DetectorSize.Absolute,
                                                  Handler=Some(CollisionHandler.IgnoreSiblingUpTo(MinT=0.67f) :> CollisionHandler),
                                                  IgnoreParent=true,
                                                  MinDistance=1.0f,
                                                  SelfIntersection=false) 
                        |> Optic.set (primaryDetector_ >-> distance_) 35.0f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance.Instance)
             };
            Thickness=ThicknessFormula.Constant(C=2.0f);
            TotalDistance=40.0f
         }


module DworkinEnvironmentConfigCustom = 
    module Vines = 
        type LeafStrategy() = 
            inherit BranchingCustom()
            member val Strategies : BranchingStrategy[] = [| BranchingCriterionBased.leavesProbesCurved1_vines |] with get, set
            member val Scale = 1.0f with get, set
            member this.MaxCurvature = 0.06f
            
            member this.Clone() = this.MemberwiseClone() :?> LeafStrategy
            
            static member Strategies_ : Lens<LeafStrategy, _> = 
                (fun x -> x.Strategies), (fun value x -> let y = x.Clone() in y.Strategies <- value; y)
            static member Scale_ : Lens<LeafStrategy, _> = 
                (fun x -> x.Scale), (fun value x -> let y = x.Clone() in y.Scale <- value; y)
            
            override this.Evaluate(agent) =
                if (abs agent.Curvature) <= this.MaxCurvature then
                    let lf = agent.LengthFactor
                    let tf = agent.ThicknessFactor
                    agent.LengthFactor <- this.Scale
                    agent.ThicknessFactor <- this.Scale
                    for each in this.Strategies do each.Evaluate(agent)
                    agent.LengthFactor <- lf
                    agent.ThicknessFactor <- tf
 
 
type InitialPositions =
    {
        Top : bool
        Bottom : bool
        Left : bool
        Right : bool
    }
    static member None = { Top=false; Bottom=false; Left=false; Right=false }

    static member Top_ : Lens<InitialPositions, bool> =
        (fun x -> x.Top), (fun value x -> { x with Top=value })
    static member Bottom_ : Lens<InitialPositions, bool> =
        (fun x -> x.Bottom), (fun value x -> { x with Bottom=value })
    static member Left_ : Lens<InitialPositions, bool> =
        (fun x -> x.Left), (fun value x -> { x with Left=value })
    static member Right_ : Lens<InitialPositions, bool> =
        (fun x -> x.Right), (fun value x -> { x with Right=value })

        
type Vines() = 
    inherit DworkinEnvironmentConfigCustom()
    member val AgentConfig = DworkinAgentConfig.vine1 with get, set
    member val Alpha = 3.8f with get, set
    member val Delay = 120 with get, set
    member val Angle = 55 with get, set
    member val Sides = { InitialPositions.None with Left=true } with get, set
    member val Corners = true with get, set
    member val LeafStrategy = DworkinEnvironmentConfigCustom.Vines.LeafStrategy() with get, set
    member val Scale = 2.5f with get, set
    member val Speed = 1.0f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> Vines
    
    static member AgentConfig_ : Lens<Vines, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member Alpha_ : Lens<Vines, _> = 
        (fun x -> x.Alpha), (fun value x -> let y = x.Clone() in y.Alpha <- value; y)
    static member Delay_ : Lens<Vines, _> = 
        (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
    static member Angle_ : Lens<Vines, _> = 
        (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
    static member Sides_ : Lens<Vines, _> = 
        (fun x -> x.Sides), (fun value x -> let y = x.Clone() in y.Sides <- value; y)
    static member Corners_ : Lens<Vines, _> = 
        (fun x -> x.Corners), (fun value x -> let y = x.Clone() in y.Corners <- value; y)
    static member LeafStrategy_ : Lens<Vines, _> = 
        (fun x -> x.LeafStrategy), (fun value x -> let y = x.Clone() in y.LeafStrategy <- value; y)
    static member Scale_ : Lens<Vines, _> = 
        (fun x -> x.Scale), (fun value x -> let y = x.Clone() in y.Scale <- value; y)
    static member Speed_ : Lens<Vines, _> = 
        (fun x -> x.Speed), (fun value x -> let y = x.Clone() in y.Speed <- value; y)
    
    override this.Evaluate(env) =
        let width = env.Bounds.Width
        let height = env.Bounds.Height

        let nx, ny =
            let nMin = (this.Alpha / this.Scale) |> max 1.75f
            let nMax = nMin * (max width height) / (min width height)
            if width > height then 
                int(round nMax), int(round nMin)
            else 
                int(round nMin), int(round nMax)

        let xAngle = -this.Angle
        let yAngle = if this.Angle > 0 then 90 - this.Angle else 270 - this.Angle
        let xs = [| ((width * float32(1 - nx)) / (float32(2 * nx) + 2.0f)) .. (width / float32(nx + 1)) .. ((width / 2.0f) - 1.0f) |] 
        let ys = [| ((height * float32(1 - ny)) / (float32(2 * ny) + 2.0f)) .. (height / float32(ny + 1)) .. ((height / 2.0f) - 1.0f) |]

        let spawnAgent (pos : Point) (alpha : int) d =
            let agentBlock (a : DworkinAgent) =
                a.ScaleBy(this.Scale)
                a.NoCollisions <- true
            let spawnOptions = ProbeSpawnOptions(Position=pos+env.Bounds.Center, AngleDeg=float32 alpha)
            spawnOptions.DelayBy(d)
            (env.LaunchProbe(this.AgentConfig, spawnOptions, agentBlock)).SpawnColliders()
            env.Spawn(
                this.AgentConfig
                |> Optic.set (speed_ >-> cast<AgentSpeed, AgentSpeedConstant> >-> magnitude_) (this.Speed / this.Scale * 2.5f)
                |> Optic.map 
                    branching_ 
                    (fun (b : BranchingStrategy) -> 
                        let strats = 
                            [| for x in (b :?> BranchingMultivariant).Strategies ->
                                match x with
                                | :? BranchingCriterionBased as xcb ->
                                    let scaleRange_ : Lens<SpawnConfigWithScaleRange, NumberRange> =
                                        (fun object -> object.ScaleRange),
                                        (fun value object -> object.WithScaleRange(value))
                                    xcb 
                                        |> Optic.map 
                                            (spawnConfig_ >-> tryCast<SpawnConfig, SpawnConfigWithScaleRange> >?> scaleRange_) 
                                            (fun sr -> sr * this.Scale) 
                                        :> BranchingStrategy
                                | _ -> x  |]
                            |> Array.append [| this.LeafStrategy |> Optic.set scale_ this.Scale |]
                        BranchingMultivariant(Strategies=strats) :> BranchingStrategy), 
                spawnOptions, 
                agentBlock)
                |> ignore

        Seq.iter 
            (fun p ->
                let c = env.Bounds.Center
                let d = p - c
                env.AddCollider(SegmentCollider(None, p, Point(c.X + (float32(sign d.X) * 0.001f), p.Y)))
                env.AddCollider(SegmentCollider(None, p, Point(p.X, c.Y + (float32(sign d.Y) * 0.001f))))) 
            env.Bounds.Corners
        
        let spawns = [|
            for x in xs do
                if this.Sides.Top then yield spawnAgent (Point(x, 0.5f - (height / 2.0f))) (180 + xAngle)
                if this.Sides.Bottom then yield spawnAgent (Point(x, (height / 2.0f) - 0.5f)) (0 + xAngle) 
            for y in ys do
                if this.Sides.Left then yield spawnAgent (Point(0.5f - (width / 2.0f), y)) (-90 + yAngle)
                if this.Sides.Right then yield spawnAgent (Point((width / 2.0f) - 0.5f, y)) (90 + yAngle) 
            if this.Corners then 
                if this.Angle > 0 then 
                    if this.Sides.Left || this.Sides.Bottom then 
                        yield spawnAgent (Point(0.5f - (width / 2.0f), (height / 2.0f) - 0.5f)) (0 + xAngle)
                    if this.Sides.Right || this.Sides.Top then 
                        yield spawnAgent (Point((width / 2.0f) - 0.5f, 0.5f - (height / 2.0f))) (90 + yAngle)
                else 
                    if this.Sides.Left || this.Sides.Top then 
                        yield spawnAgent (Point(0.5f - (width / 2.0f), 0.5f - (height / 2.0f))) (180 + xAngle)
                    if this.Sides.Right || this.Sides.Bottom then 
                        yield spawnAgent (Point((width / 2.0f) - 0.5f, (height / 2.0f) - 0.5f)) (90 + yAngle)
            |]

        let delays = Array.init spawns.Length (fun i -> (this.Delay / (i+1)) - (this.Delay / spawns.Length))

        Array.iter2
            (fun spawnFunc delay -> spawnFunc delay)
            spawns
            delays