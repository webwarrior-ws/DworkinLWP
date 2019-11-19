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



module BranchingCriterionIntervalRandomized =
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
    
 

module DworkinAgentConfig =
    open LibDworkin.Brushes

    let pCBTrace = 
        { DworkinAgentConfig.Default with 
            ColliderOptions=ColliderOptionsGeneral();
            Speed=AgentSpeedThickness();
            Termination=TerminationSpawn(SpawnConfig=SpawnConfig.PCBTraceLengthLimited());
            TerminationConditions=
                { Collision=Some(TerminationConditionCollision()
                              |> Optic.set ignoreParent_ true
                              |> Optic.set primaryDetector_ { CollisionDetectorConfig.Default with Angle=45.0f; Distance=5.0f }
                              |> Optic.set secondaryDetector_ { CollisionDetectorConfig.Default with Angle=180.0f; Distance=2.5f }
                              |> Optic.set selfIntersection_ false);
                Curvature=None;
                Distance=Some(TerminationConditionDistance()); };
            Thickness=ThicknessFormula.Constant(C=1.5f);
            TotalDistance=10.0f
         }
    
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
    
    let straightMazeRandomAngle_v2 = 
        { DworkinAgentConfig.Default with 
            Brush=BrushStrategySolidJoint(JointType=BrushJoint.Miter);
            ColliderOptions=ColliderOptionsGeneral(Strategy=ColliderStrategySegments());
            Speed=AgentSpeedConstant();
            Termination=TerminationSpawn(SpawnConfig=SpawnConfig.StraightMazeRandomAngle());
            TerminationConditions=
                { Collision=Some(TerminationConditionCollision(DetectorSize=DetectorSize.Absolute, IgnoreParent=true, MinDistance=5.0f, SelfIntersection=false)
                              |> Optic.set (primaryDetector_ >-> angle_) 180.0f
                              |> Optic.set (primaryDetector_ >-> distance_) 20.0f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance()); };
            Thickness=ThicknessFormula.Constant(C=5.0f);
            TotalDistance=30.0f
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
    
    
    type PCBBranchOffset() = 
        inherit SpawnConfig()
        member val RecursionLimit = 3 with get, set
        member val ThicknessScale = 1.0f with get, set
        member val Delay = 0 with get, set
        member val Flip = false with get, set
        member val BranchProb = 0.75f with get, set
        member val ColliderAngle = 45.0f with get, set
        member val ColliderSize = 10.0f with get, set
        member val Offset = 7.5f with get, set
        member val Direction = "termination angle" with get, set
        member val StartWithVias = true with get, set
        member val Alternate = false with get, set
        member val MinGeneration = 4 with get, set

        member this.Clone() = this.MemberwiseClone() :?> PCBBranchOffset

        static member RecursionLimit_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.RecursionLimit), (fun value x -> let y = x.Clone() in y.RecursionLimit <- value; y)
        static member ThicknessScale_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.ThicknessScale), (fun value x -> let y = x.Clone() in y.ThicknessScale <- value; y)
        static member Delay_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
        static member Flip_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.Flip), (fun value x -> let y = x.Clone() in y.Flip <- value; y)
        static member BranchProb_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.BranchProb), (fun value x -> let y = x.Clone() in y.BranchProb <- value; y)
        static member ColliderAngle_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.ColliderAngle), (fun value x -> let y = x.Clone() in y.ColliderAngle <- value; y)
        static member ColliderSize_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.ColliderSize), (fun value x -> let y = x.Clone() in y.ColliderSize <- value; y)
        static member Offset_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.Offset), (fun value x -> let y = x.Clone() in y.Offset <- value; y)
        static member Direction_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.Direction), (fun value x -> let y = x.Clone() in y.Direction <- value; y)
        static member StartWithVias_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.StartWithVias), (fun value x -> let y = x.Clone() in y.StartWithVias <- value; y)
        static member Alternate_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.Alternate), (fun value x -> let y = x.Clone() in y.Alternate <- value; y)
        static member MinGeneration_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.MinGeneration), (fun value x -> let y = x.Clone() in y.MinGeneration <- value; y)

        override this.Evaluate(agent, pos) =
            if agent.Attributes.CanBranch.GetOrSetIfAbsent(fun () -> (this.RecursionLimit >= 1) && ((agent.Generation >= this.MinGeneration) && (agent.Random.NextFloat() < this.BranchProb))) then
                let terminationConfig = (agent.Config.Termination :?> TerminationSpawn).SpawnConfig  :?> PCBTraceLengthLimited
                let angles : float32[] = terminationConfig.Angles
                let mutable angle = angles.AtRandom(agent.Random)
                if this.Direction = "perpendicular" then
                    angle <- -90.0f
                if this.Direction = "parallel" then
                    angle <- 0.0f
                if this.Alternate && (isOdd agent.TotalNumBranches) then
                    angle <- -angle
                if this.Flip then
                    angle <- -angle
                let collider = 
                    (CircleSectorCollider(
                        None, 
                        pos + (((agent.Direction.LeftRotated * agent.FlipFactor) * this.Offset) * (if this.Flip then 1.0f else -1.0f)), 
                        this.ColliderSize * (max agent.Thickness agent.Speed), 
                        agent.Direction.Theta - ((deg2rad angle) * agent.FlipFactor), 
                        deg2rad this.ColliderAngle))
                let exit = 
                    agent.DworkinEnvironment.QueryColliders(
                        (collider :> IHasBounds).Bounds, 
                        agent.Config.TerminationConditions.Collision.Value.Layers, 
                        fun layer each -> (each.Parent <> Some(agent :> IDworkinAgentState)) && (collide collider each) )
                
                if not exit then
                    let mutable config = 
                        agent.Config 
                        |> Optic.set 
                            (branching_ >-> BranchingCriterionBased.SpawnConfig_ >-> cast<SpawnConfig, PCBBranchOffset> >-> recursionLimit_) 
                            (this.RecursionLimit - 1)
                    try 
                        let lengthRange : NumberRange = terminationConfig.LengthRange
                        config <- config |> Optic.set totalDistance_ (agent.Random.NextInRange(lengthRange))
                    with | _ -> ()
                    let agentCopy = agent.Copy()
                    agent.DworkinEnvironment.Delay(
                        (fun _ -> 
                            agentCopy.Spawn(
                                config, 
                                AgentSpawnOptions(AngleDeg=angle, OffsetSideways=this.Offset), 
                                fun a -> 
                                    a.ScaleBy(Point(this.ThicknessScale, 1.0f))
                                    if this.StartWithVias then
                                        let opts = AgentSpawnOptions(Angle=Single.Pi)
                                        let terminalConfig : CircleAdvanced = 
                                            a.Config 
                                            |> Optic.get (termination_spawnConfig_ >-> cast<SpawnConfig, PCBTraceLengthLimited> >-> terminalConfig_)
                                        opts.DelayBy(int <| ((terminalConfig.Radius * a.Thickness) * 0.5f) / a.Speed)
                                        a.Spawn(
                                            a.Config
                                            |> Optic.set totalDistance_ 0.01f
                                            |> Optic.set (terminationConditions_ >-> collision_ >-> minDistance_) 1.0f, 
                                            opts, 
                                            fun x -> x.Parent <- Some(a)))
                            None), 
                        this.Delay)
    
    let termination_lengthLimit_ = 
        termination_spawnConfig_ >-> cast<SpawnConfig, PCBTraceLengthLimited> >-> lengthLimit_


    type PCBTraceLengthLimited() = 
        inherit SpawnConfig()
        member val Angles = [| -45.0f |] with get, set
        member val LengthRange = NumberRange(40.0f, 120.0f) with get, set
        member val TerminalConfig = SpawnConfig.CircleAdvanced() with get, set
        member val LengthLimit = 300.0f with get, set
        member val FlipProbability = 0.5f with get, set
        member val LengthAlternation = 5.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> PCBTraceLengthLimited

        static member Angles_ : Lens<PCBTraceLengthLimited, _> = 
            (fun x -> x.Angles), (fun value x -> let y = x.Clone() in y.Angles <- value; y)
        static member LengthRange_ : Lens<PCBTraceLengthLimited, _> = 
            (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
        static member TerminalConfig_ : Lens<PCBTraceLengthLimited, _> = 
            (fun x -> x.TerminalConfig), (fun value x -> let y = x.Clone() in y.TerminalConfig <- value; y)
        static member LengthLimit_ : Lens<PCBTraceLengthLimited, _> = 
            (fun x -> x.LengthLimit), (fun value x -> let y = x.Clone() in y.LengthLimit <- value; y)
        static member FlipProbability_ : Lens<PCBTraceLengthLimited, _> = 
            (fun x -> x.FlipProbability), (fun value x -> let y = x.Clone() in y.FlipProbability <- value; y)
        static member LengthAlternation_ : Lens<PCBTraceLengthLimited, _> = 
            (fun x -> x.LengthAlternation), (fun value x -> let y = x.Clone() in y.LengthAlternation <- value; y)

        override this.Evaluate(agent, pos) =
            let mutable nextTD = null
            let angle = this.Angles.AtRandom(agent.Random)
            let nextTD = agent.Random.NextInRange(this.LengthRange) * (((float32((agent.Generation + 1) % 2) * this.LengthAlternation) + 1.0f) / (this.LengthAlternation + 1.0f))
            if (agent.S <= agent.Speed * 2.0f) || ((float32 this.LengthLimit - nextTD) <= 1.0f) then
                this.TerminalConfig.Evaluate(agent)
            else
                let config = 
                    agent.Config
                    |> Optic.set totalDistance_ (min nextTD this.LengthLimit)
                    |> Optic.set termination_lengthLimit_ (this.LengthLimit - agent.Config.TotalDistance)
                match agent.Collision with
                | Some(value) -> 
                    match value.Obstacle.Parent with
                    | None -> this.TerminalConfig.Evaluate(agent)
                    | Some(p) when ((abs (min (agent.Direction.AngleWith(p.Direction)) (p.Direction.AngleWith(agent.Direction)))) > Single.HalfPi) ->
                        this.TerminalConfig.Evaluate(agent)
                    | Some(p) ->
                        agent.Spawn(
                            agent.Config, 
                            AgentSpawnOptions(Direction=p.Direction), 
                            fun a -> 
                                a.Attributes.DrawJoint.Set(true)
                                match a.Config.TerminationConditions.Collision with
                                | Some cond -> 
                                    if cond.Evaluate(a) then
                                        this.TerminalConfig.Evaluate(agent)
                                        a.Delete()
                                | None -> ())
                | None -> 
                    agent.Spawn(
                        config, 
                        AgentSpawnOptions(AngleDeg=angle), 
                        fun a -> 
                            a.Attributes.DrawJoint.Set(true)
                            if (angle <> 0.0f) && (agent.Random.NextFloat() < this.FlipProbability) then
                                a.Flip())
    

    type PCBTraceGroup2VarLen() = 
        inherit SpawnConfig()
        member val Width = 100 with get, set
        member val N = 8 with get, set
        member val Config = DworkinAgentConfig.pCBTrace with get, set
        member val Flip = 4 with get, set
        member val Delay = 2 with get, set
        member val Identical = false with get, set
        member val StartWithVias = false with get, set
        member val TraceLengthRange = NumberRange(100.0f, 300.0f) with get, set
        member val ShiftFactor = 0.71f with get, set

        member this.Clone() = this.MemberwiseClone() :?> PCBTraceGroup2VarLen

        static member Width_ : Lens<PCBTraceGroup2VarLen, _> = 
            (fun x -> x.Width), (fun value x -> let y = x.Clone() in y.Width <- value; y)
        static member N_ : Lens<PCBTraceGroup2VarLen, _> = 
            (fun x -> x.N), (fun value x -> let y = x.Clone() in y.N <- value; y)
        static member Config_ : Lens<PCBTraceGroup2VarLen, _> = 
            (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
        static member Flip_ : Lens<PCBTraceGroup2VarLen, _> = 
            (fun x -> x.Flip), (fun value x -> let y = x.Clone() in y.Flip <- value; y)
        static member Delay_ : Lens<PCBTraceGroup2VarLen, _> = 
            (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
        static member Identical_ : Lens<PCBTraceGroup2VarLen, _> = 
            (fun x -> x.Identical), (fun value x -> let y = x.Clone() in y.Identical <- value; y)
        static member StartWithVias_ : Lens<PCBTraceGroup2VarLen, _> = 
            (fun x -> x.StartWithVias), (fun value x -> let y = x.Clone() in y.StartWithVias <- value; y)
        static member TraceLengthRange_ : Lens<PCBTraceGroup2VarLen, _> = 
            (fun x -> x.TraceLengthRange), (fun value x -> let y = x.Clone() in y.TraceLengthRange <- value; y)
        static member ShiftFactor_ : Lens<PCBTraceGroup2VarLen, _> = 
            (fun x -> x.ShiftFactor), (fun value x -> let y = x.Clone() in y.ShiftFactor <- value; y)

        override this.Evaluate(agent, pos) =
            let seed = agent.Random.Next(System.Int32.MaxValue)
            let interval = 
                if isOdd this.N then
                    Seq.append 
                        (Seq.rev (seq { 0 .. (this.Width / (1 - this.N)) .. (this.Width / (-2)) })) 
                        (seq { (this.Width / (this.N - 1)) .. (this.Width / (this.N - 1)) .. (this.Width / 2) })
                else
                    seq { (this.Width / (-2)) .. (this.Width / (this.N - 1)) .. (this.Width / 2) }
            withIndexDo
                (fun dx i -> 
                    let dx = float32 dx
                    let bias = 
                        if isOdd this.N then
                            0.0f
                        else
                            -0.5f
                    let shift = (((max (float32(i - this.Flip) + bias) (float32(-i + this.Flip) - bias)) * (float32 this.Width)) / (float32 this.N)) * this.ShiftFactor
                    let opts = AgentSpawnOptions()
                    if this.Identical then
                        opts.Seed <- Some(seed)
                    opts.OffsetSideways <- dx
                    if this.Delay > 0 then
                        opts.DelayBy(((float32(this.Width) * 0.5f) - shift) * float32(this.Delay) / (this.Config.Speed :?> AgentSpeedThickness).Factor |> int)
                    agent.Spawn(
                        this.Config
                        |> Optic.map totalDistance_ (fun x -> (x + float32 this.Width) - shift)
                        |> Optic.set termination_lengthLimit_ (agent.Random.NextInRange(this.TraceLengthRange)), 
                        opts, 
                        fun a -> if i <= this.Flip then a.Flip())
                    if this.StartWithVias then
                        let termination_terminalConfig_ = termination_spawnConfig_ >-> cast<SpawnConfig, PCBTraceLengthLimited> >-> terminalConfig_
                        agent.Spawn(
                            this.Config
                                |> Optic.set (termination_terminalConfig_ >-> spawnCollider_) false
                                |> Optic.set totalDistance_ 0.0f, 
                            AgentSpawnOptions(
                                Seed=Some(1),
                                OffsetSideways=dx,
                                Angle=Single.Pi,
                                Offset=(this.Config |> Optic.get (termination_terminalConfig_ >-> radius_)) * 0.5f
                            ), 
                            fun a -> a.Terminate()))
                (Seq.rev interval)
    

    type PCBTracesFromChip() = 
        inherit SpawnConfig()
        member val GroupConfig = SpawnConfig.PCBTraceGroup2VarLen() with get, set
        member val Nx = 4 with get, set
        member val Ny = 6 with get, set
        member val Gap = 5.0f with get, set
        member val Octagonal = true with get, set
        member val Appearance = "contour" with get, set

        member this.Clone() = this.MemberwiseClone() :?> PCBTracesFromChip

        static member GroupConfig_ : Lens<PCBTracesFromChip, _> = 
            (fun x -> x.GroupConfig), (fun value x -> let y = x.Clone() in y.GroupConfig <- value; y)
        static member Nx_ : Lens<PCBTracesFromChip, _> = 
            (fun x -> x.Nx), (fun value x -> let y = x.Clone() in y.Nx <- value; y)
        static member Ny_ : Lens<PCBTracesFromChip, _> = 
            (fun x -> x.Ny), (fun value x -> let y = x.Clone() in y.Ny <- value; y)
        static member Gap_ : Lens<PCBTracesFromChip, _> = 
            (fun x -> x.Gap), (fun value x -> let y = x.Clone() in y.Gap <- value; y)
        static member Octagonal_ : Lens<PCBTracesFromChip, _> = 
            (fun x -> x.Octagonal), (fun value x -> let y = x.Clone() in y.Octagonal <- value; y)
        static member Appearance_ : Lens<PCBTracesFromChip, _> = 
            (fun x -> x.Appearance), (fun value x -> let y = x.Clone() in y.Appearance <- value; y)

        override this.Evaluate(agent, pos) =
            let startWithVias = this.Appearance = "vias"
            let offset = 
                if startWithVias then
                    this.GroupConfig.Config |> Optic.get (termination_spawnConfig_ >-> cast<SpawnConfig, PCBTraceLengthLimited> >-> terminalConfig_ >-> radius_)
                else
                    0.0f
            
            Seq.iter
                (fun angle -> 
                    agent.Spawn(
                        this.GroupConfig
                        |> Optic.set n_ this.Nx
                        |> Optic.set width_ (int <| this.Gap * float32(this.Nx - 1))
                        |> Optic.set flip_ (agent.Random.Next(this.Nx))
                        |> Optic.set startWithVias_ startWithVias, 
                        AgentSpawnOptions(Offset=((this.Gap * float32(this.Ny - 1)) * 0.5f) + offset, Angle=angle)))
                ([| 0.0f; Single.Pi; |])
            
            Seq.iter
                (fun angle -> 
                    agent.Spawn(
                        this.GroupConfig
                        |> Optic.set n_ (this.Ny - 2)
                        |> Optic.set width_ (int <| this.Gap * float32(this.Ny - 3))
                        |> Optic.set flip_ (agent.Random.Next(this.Ny - 2))
                        |> Optic.set startWithVias_ startWithVias, 
                        AgentSpawnOptions(
                            Offset= (((this.Gap * (float32 this.Nx - 1.0f)) * 0.5f) + offset) + (if this.Octagonal then this.Gap * 0.5f else 0.0f),
                            Angle=angle
                        )))
                ([| Single.HalfPi; Single.Pi + Single.HalfPi; |])
            
            let extent = Point(float32(this.Nx - 1), float32(this.Ny - 1)) * this.Gap
            let points = ResizeArray((Rectangle.FromOriginAndExtent(extent * -0.5f, extent)).Corners)
            if this.Octagonal then
                points.AddBeforeIndex(points.First + (Point(-this.Gap, this.Gap) * 0.5f), 2)
                points.AddAfterIndex(points.Third - (Point(this.Gap, this.Gap) * 0.5f), 2)
                points.AddBeforeIndex(points.Last + (Point(this.Gap, this.Gap) * 0.5f), 6)
                points.AddBeforeIndex(points.Fifth + (Point(this.Gap, -this.Gap) * 0.5f), 6)
            
            if Array.contains this.Appearance ([|"contour"; "filled"|]) then
                let shape = Polygon(points.ToArray()) 
                let paint = { Fill=this.Appearance = "filled"; StrokeWidth=this.GroupConfig.Config.Thickness.Evaluate(0.0f, 0.0f) * agent.ThicknessFactor }
                agent.DworkinEnvironment.SpawnShape(shape, ShapeSpawnOptions(Position=agent.Position, Direction=agent.Direction, Paint=paint))
            
            points.Add(points.First)
            points 
                |> Seq.pairwise 
                |> Seq.iter (
                    fun (a, b) -> 
                        agent.DworkinEnvironment.Delay(
                            (fun env -> 
                                (env :?> DworkinEnvironment).AddCollider(
                                    SegmentCollider(
                                        None, 
                                        ((a * 0.999f) + pos).Rotate(Single.HalfPi - agent.Direction.Theta, pos), 
                                        ((b * 0.999f) + pos).Rotate(Single.HalfPi - agent.Direction.Theta, pos)))
                                None),
                            1))
    
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
    

    type StraightMazeRandomAngle() = 
        inherit SpawnConfig()
        member val Angle = 90 with get, set
        member val P = 0.75f with get, set
        member val PossibleAngles = [| 90; 0; 90 |] with get, set
        member val CollisionAccuracy = 4 with get, set

        member this.Clone() = this.MemberwiseClone() :?> StraightMazeRandomAngle
        
        static member Angle_ : Lens<StraightMazeRandomAngle, _> = 
            (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
        static member P_ : Lens<StraightMazeRandomAngle, _> = 
            (fun x -> x.P), (fun value x -> let y = x.Clone() in y.P <- value; y)
        static member PossibleAngles_ : Lens<StraightMazeRandomAngle, _> = 
            (fun x -> x.PossibleAngles), (fun value x -> let y = x.Clone() in y.PossibleAngles <- value; y)
        static member CollisionAccuracy_ : Lens<StraightMazeRandomAngle, _> = 
            (fun x -> x.CollisionAccuracy), (fun value x -> let y = x.Clone() in y.CollisionAccuracy <- value; y)

        override this.Evaluate(agent, pos) =
            let angles = 
                if this.Angle = 0 then
                    [| this.Angle |]
                else
                    [| this.Angle; -this.Angle |]
            if agent.Random.NextFloat() < this.P then
                angles.ShuffleBy(agent.Random)
            angles |> withIndexDo
                (fun phi i -> 
                    let config =
                        agent.Config 
                        |> Optic.set 
                            (termination_ >-> TerminationSpawn.SpawnConfig_ >-> cast<_, StraightMazeRandomAngle> >-> angle_) 
                            (this.PossibleAngles.AtRandom(agent.Random))
                        |> Optic.set 
                            (curvature_ >-> tryCast<_, CurvatureFormula.SNoise> >?> d_) 
                            (agent.Random.NextFloat() * 10.0f)
                    let probeData = 
                        let s = 
                            config.TerminationConditions.Collision.Value.PrimaryDetector.Distance / agent.Speed / (float32 this.CollisionAccuracy)
                            |> floor
                            |> int
                        agent.LaunchProbe(config, ProbeSpawnOptions(AngleDeg=float32 phi, StepsBetweenCollisionCheck=s))
                    if Option.isNone probeData.Collision then
                        let delay = (round (agent.Config.TotalDistance / agent.Speed)) * float32(i-1) |> int
                        let spawnOptions = AgentSpawnOptions(AngleDeg=float32 phi)
                        spawnOptions.DelayBy(delay)
                        probeData.SpawnColliders()
                        agent.Spawn(
                            config, 
                            spawnOptions, 
                            fun a -> 
                                a.NoCollisions <- true
                                a.Attributes.DrawJoint.Set(true)))



