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
        
    let vine1 = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionCustom.Interval_MaxBranches(D=20.0f,N=3),
            SpawnConfig=DworkinAgentConfig.Vine1.Branching.SpawnConfig()
        )

    let leavesProbesCurved1_vines = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionInterval.ConstantBoundedT(D=25.0f,Start=0.05f,End=0.65f),
            Delay=30,
            SpawnConfig=LeavesProbesCurved1_vines.SpawnConfig()
        )


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
                member this.ScaleSteps = 5.0f

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
                
                override this.Evaluate(agent, pos) =
                    agent.GenerationLimit <- this.GenLimit
                    let angles = ResizeArray([| this.Angle |])
                    for delta in 0 .. (this.AngleSpread / this.AngleSteps) .. this.AngleSpread  do
                        angles.Add(this.Angle + delta)
                        angles.Add(this.Angle - delta)
                    angles.ShuffleBy(agent.Random) |> ignore
                    
                    seq { for phi in angles do for scale in this.ScaleRange.By(this.ScaleRange.Extent / this.ScaleSteps) |> Seq.rev do yield phi, scale }
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
            
            
    let vine1 = 
        { DworkinAgentConfig.Default with 
            Branching=BranchingMultivariant(Strategies=[| 
                BranchingCriterionBased.vine1 
                |> Optic.set spawnConfig_ (DworkinAgentConfig.Vine1.Branching.SpawnConfig() :> LibDworkin.Configs.SpawnConfig) |]);
            Brush=BrushStrategySolid();
            ColliderOptions=ColliderOptionsGeneral();
            Curvature=CurvatureFormula.DoubleLogSpiralSimple(A=0.0174f,D=0.182f,B=0.0751f);
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

        
type Vines() = 
    inherit DworkinEnvironmentConfigCustom()
    member val AgentConfig = DworkinAgentConfig.vine1 with get, set
    member val Nx = 2 with get, set
    member val Ny = 3 with get, set
    member val Delay = 120 with get, set
    member val Angle = 55 with get, set
    member val Sides = { InitialPositions.None with Left=true } with get, set
    member val Corners = true with get, set
    member val LeafStrategy = DworkinEnvironmentConfigCustom.Vines.LeafStrategy() with get, set
    member val Scale = 2.5f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> Vines
    
    static member AgentConfig_ : Lens<Vines, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member Nx_ : Lens<Vines, _> = 
        (fun x -> x.Nx), (fun value x -> let y = x.Clone() in y.Nx <- value; y)
    static member Ny_ : Lens<Vines, _> = 
        (fun x -> x.Ny), (fun value x -> let y = x.Clone() in y.Ny <- value; y)
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
    
    override this.Evaluate(env) =
        let width = env.Bounds.Width
        let height = env.Bounds.Height
        let xAngle = -this.Angle
        let yAngle = if this.Angle > 0 then 90 - this.Angle else 270 - this.Angle
        let xs = [| ((width * float32(1 - this.Nx)) / (float32(2 * this.Nx) + 2.0f)) .. (width / float32(this.Nx + 1)) .. ((width / 2.0f) - 1.0f) |] 
        let ys = [| ((height * float32(1 - this.Ny)) / (float32(2 * this.Ny) + 2.0f)) .. (height / float32(this.Ny + 1)) .. ((height / 2.0f) - 1.0f) |]

        let spawnAgent (pos : Point) (alpha : int) d =
            let agentBlock (a : DworkinAgent) =
                a.ScaleBy(this.Scale)
                a.NoCollisions <- true
            let spawnOptions = ProbeSpawnOptions(Position=pos+env.Bounds.Center, AngleDeg=float32 alpha)
            spawnOptions.DelayBy(d)
            (env.LaunchProbe(this.AgentConfig, spawnOptions, agentBlock)).SpawnColliders()
            env.Spawn(
                this.AgentConfig
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