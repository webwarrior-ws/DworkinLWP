module rec GeneratedConfigs


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




module CurvatureFormula =
    type Tan_t_() = 
        inherit CurvatureFormula()
        member val C = 0.023f with get, set

        member this.Clone() = this.MemberwiseClone() :?> Tan_t_

        static member C_ : Lens<Tan_t_, _> = 
            (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)

        override this.Evaluate(s, t) =
            (tan ((t * Single.Pi) * 0.5f)) * this.C
    


module ThicknessFormula =
    type Constant() = 
        inherit ThicknessFormula()
        member val C = 1.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> Constant

        static member C_ : Lens<Constant, _> = 
            (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)

        override this.Evaluate(s, t) =
            this.C

    type LeafParabolic() = 
        inherit ThicknessFormula()
        member val C = 5.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> LeafParabolic

        static member C_ : Lens<LeafParabolic, _> = 
            (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)

        override this.Evaluate(s, t) =
            this.C * (1.0f - (squared ((t * 2.0f) - 1.0f)))
    
    type ConcaveParabolic() = 
        inherit ThicknessFormula()
        member val C = 5.0f with get, set
        member val Alpha = 0.5f with get, set

        member this.Clone() = this.MemberwiseClone() :?> ConcaveParabolic

        static member C_ : Lens<ConcaveParabolic, _> = 
            (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)
        static member Alpha_ : Lens<ConcaveParabolic, _> = 
            (fun x -> x.Alpha), (fun value x -> let y = x.Clone() in y.Alpha <- value; y)

        override this.Evaluate(s, t) =
            this.C * (1.0f - ((1.0f - this.Alpha) * (1.0f - (squared ((t * 2.0f) - 1.0f)))))
    


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
    
 

module DworkinAgentConfig =
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
                        (SpawnConfig.Probe1BranchRecByLen2()
                            |> Optic.set angle_ -15.0f
                            |> Optic.set genLimit_ 20
                            |> Optic.set lengthRange_ (NumberRange(50.0f, 300.0f))
                            |> Optic.set lengthSteps_ 3)
                );
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
    
    type Probe1BranchRecByLen2() = 
        inherit SpawnConfig()
        member val Angle = 0.0f with get, set
        member val GenLimit = 4 with get, set
        member val LengthRange = NumberRange(100.0f, 200.0f) with get, set
        member val LengthSteps = 4 with get, set

        member this.Clone() = this.MemberwiseClone() :?> Probe1BranchRecByLen2

        static member Angle_ : Lens<Probe1BranchRecByLen2, float32> = 
            (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
        static member GenLimit_ : Lens<Probe1BranchRecByLen2, _> = 
            (fun x -> x.GenLimit), (fun value x -> let y = x.Clone() in y.GenLimit <- value; y)
        static member LengthRange_ : Lens<Probe1BranchRecByLen2, _> = 
            (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
        static member LengthSteps_ : Lens<Probe1BranchRecByLen2, _> = 
            (fun x -> x.LengthSteps), (fun value x -> let y = x.Clone() in y.LengthSteps <- value; y)

        override this.Evaluate(agent, pos) =
            agent.GenerationLimit <- this.GenLimit
            let angles = ([| this.Angle; -this.Angle; |])
            angles.ShuffleBy(agent.Random)
            let lengths : float32[] = 
                (this.LengthRange.By(this.LengthRange.Extent / float32(this.LengthSteps)) |> Seq.toArray).TakeLast(agent.Random.Next(this.LengthSteps))
                |> Array.rev
            Seq.iter
                (fun (phi : float32) -> 
                    lengths 
                    |> Seq.iter
                       (fun length -> 
                            let scale = agent.RelativeThickness / agent.Config.Thickness.Evaluate(0.0f, 0.0f)
                            let agentBlock = 
                                fun (a : DworkinAgent) -> 
                                    if phi <= 0.0f then
                                        a.Flip()
                                    a.ScaleBy(scale)
                            let config = agent.Config|> Optic.set totalDistance_ (length / scale)
                            let probeData = 
                                agent.LaunchProbe(
                                    config, 
                                    ProbeSpawnOptions(AngleDeg=phi, StepsBetweenCollisionCheck=4),
                                    agentBlock)
                            match probeData.Collision with
                            | Some(_) -> ()
                            | None -> 
                                (*return*) 
                                agent.Spawn(
                                    config, 
                                    AgentSpawnOptions(AngleDeg=phi), 
                                    fun (a : DworkinAgent) -> 
                                        agentBlock a
                                        a.NoCollisions <- true
                                        probeData.Id <- a.Id
                                        probeData.SpawnColliders())))
                angles


type PCB_RandomPosChips() = 
    inherit DworkinEnvironmentConfigCustom()
    member val Angle = 0.0f with get, set
    member val AgentConfig = SpawnConfig.PCBTracesFromChip() |> Optic.set (groupConfig_ >-> config_ >-> brush_) (BrushStrategySolidJoint()) with get, set
    member val NxRange = NumberRange(2.0f, 8.0f) with get, set
    member val NyRange = NumberRange(4.0f, 8.0f) with get, set
    member val ThicknessVariance = 0.0f with get, set
    member val Density = 0.5f with get, set
    member val Endpoints = "hollow" with get, set
    member val Vias = false with get, set
    member val Angles = [| -45.0f |] with get, set
    member val Branch = false with get, set
    member val BranchingConfig = 
        BranchingCriterionBased(
            Criterion=(BranchingCriterionCustom.IntervalBoundedTRand() |> Optic.set d_ 8.0f),  
            SpawnConfig=(SpawnConfig.PCBBranchOffset() |> Optic.set recursionLimit_ 4))
        with get, set
    member val Speed = 0.5f with get, set
    member val Octagonal = true with get, set
    member val Size = 1.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> PCB_RandomPosChips

    static member Angle_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
    static member AgentConfig_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member NxRange_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.NxRange), (fun value x -> let y = x.Clone() in y.NxRange <- value; y)
    static member NyRange_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.NyRange), (fun value x -> let y = x.Clone() in y.NyRange <- value; y)
    static member ThicknessVariance_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.ThicknessVariance), (fun value x -> let y = x.Clone() in y.ThicknessVariance <- value; y)
    static member Density_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.Density), (fun value x -> let y = x.Clone() in y.Density <- value; y)
    static member Endpoints_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.Endpoints), (fun value x -> let y = x.Clone() in y.Endpoints <- value; y)
    static member Vias_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.Vias), (fun value x -> let y = x.Clone() in y.Vias <- value; y)
    static member Angles_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.Angles), (fun value x -> let y = x.Clone() in y.Angles <- value; y)
    static member Branch_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.Branch), (fun value x -> let y = x.Clone() in y.Branch <- value; y)
    static member BranchingConfig_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.BranchingConfig), (fun value x -> let y = x.Clone() in y.BranchingConfig <- value; y)
    static member Speed_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.Speed), (fun value x -> let y = x.Clone() in y.Speed <- value; y)
    static member Octagonal_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.Octagonal), (fun value x -> let y = x.Clone() in y.Octagonal <- value; y)
    static member Size_ : Lens<PCB_RandomPosChips, _> = 
        (fun x -> x.Size), (fun value x -> let y = x.Clone() in y.Size <- value; y)

    override this.Evaluate(env) =
        let spawn = CollisionLayer(99)
        let width = env.Bounds.Width
        let height = env.Bounds.Height
        let gap = this.AgentConfig.Gap
        let mutable n = ((((this.Density * (width * height)) / (squared 1.25f)) / ((this.NxRange.Average * this.NyRange.Average) * (squared gap))) / (squared this.Size)) / 9.0f |> int
        if this.Branch then
            n <- int <| round (float32 n / (sqrt 2.0f))
        let mutable nx = int (round <| env.Random.NextInRange(this.NxRange))
        let mutable ny = int (round <| env.Random.NextInRange(this.NyRange))
        let spawnSites = ResizeArray()
        let mutable i = 0
        let mutable ts = [| for k in 1..n -> (env.Random.NextGaussian() * this.ThicknessVariance) + 1.0f |]
        let tsAverage = ts |> Array.average
        ts <- ts |> Array.map (fun x -> (max (x / tsAverage) 1.0f) * this.Size)
        
        while (spawnSites.Length < n) && (i < 10000) do
            let radius = ((float32(max nx ny) * gap) * (ts.[spawnSites.Length])) * 1.25f
            let p = env.Random.NextInRectangle(env.Bounds.InsetBy(radius))
            let collider = CircleCollider(None, p, radius)
            if not <| env.TestForCollision(CollisionLayers([| spawn |], false), collider) then
                env.AddColliderToLayer(collider, spawn)
                spawnSites.Add(struct (collider.Center, Point(float32 nx, float32 ny)))
                nx <- int(round <| env.Random.NextInRange(this.NxRange))
                ny <- int(round <| env.Random.NextInRange(this.NyRange))
            i <- i + 1

        env.RemoveColliderLayer(spawn)
        let mutable configToSpawn = 
            this.AgentConfig
            |> Optic.set octagonal_ this.Octagonal
            |> Optic.set appearance_ 
                ((if this.Vias then
                    "vias"
                  else
                    if this.Endpoints = "hollow" then
                        "contour"
                    else
                        "filled"))
            |> Optic.set (groupConfig_ >-> config_ >-> termination_spawnConfig_ >-> cast >-> SpawnConfig.PCBTraceLengthLimited.TerminalConfig_ >-> hollowPart_) 
                ((if this.Endpoints = "hollow" then 0.5f else 0.0f))
            |> Optic.set (groupConfig_ >-> config_ >-> termination_spawnConfig_ >-> cast >-> SpawnConfig.PCBTraceLengthLimited.Angles_) this.Angles
            |> Optic.set (groupConfig_ >-> config_ >-> branching_) 
                (if this.Branch then
                    this.BranchingConfig |> Optic.set (spawnConfig_ >-> cast<SpawnConfig, SpawnConfig.PCBBranchOffset> >-> branchProb_) this.Density :> BranchingStrategy
                  else
                    BranchingNone() :> BranchingStrategy)
            |> Optic.set (groupConfig_ >-> config_ >-> speed_ >-> cast<AgentSpeed, AgentSpeedThickness> >-> factor_) this.Speed
        
        withIndexDo
            (fun struct (pos, size) k -> 
                let mutable dist = (min width height) * 0.5f
                for struct (sp, ss) in spawnSites do 
                    if sp <> pos then
                        let p = ((pos - sp).Abs()) - ((size + ss) * gap)
                        dist <- min dist (max p.X p.Y)
                if this.Branch then
                    let transform d = d * ts.[k-1]
                    configToSpawn <- 
                        configToSpawn
                        |> Optic.map 
                            (groupConfig_ >-> config_ >-> branching_) 
                            (fun branching -> 
                                branching
                                |> Optic.map (criterion_ >-> BranchingCriterionCustom.IntervalBoundedTRand.D_) transform
                                |> Optic.map (spawnConfig_ >-> cast >-> SpawnConfig.PCBBranchOffset.Offset_) transform
                                |> Optic.map (spawnConfig_ >-> cast >-> SpawnConfig.PCBBranchOffset.ColliderSize_) transform)
                let opts = AgentSpawnOptions(Position=pos, AngleDeg=this.Angle)
                if k > 1 then
                    opts.DelayBy(int ((env.Random.NextFloat() * dist) / this.Speed / this.Size))
                env.Spawn(
                    configToSpawn
                    |> Optic.set nx_ (int size.X)
                    |> Optic.set ny_ (int size.Y)
                    |> Optic.map gap_ (fun x -> x * ts.[k-1]), 
                    opts,
                    fun a -> a.ScaleBy(Point(ts.[k-1], 1.0f)))
                    |> ignore)
            spawnSites


type SpiralsProbesBranching1() = 
    inherit DworkinEnvironmentConfigCustom()
    member val Config = DworkinAgentConfig.swirls2_7_fast_ with get, set
    member val N = 2 with get, set
    member val LengthRange = NumberRange(50.0f, 200.0f) with get, set
    member val Delay = 100 with get, set

    member this.Clone() = this.MemberwiseClone() :?> SpiralsProbesBranching1

    static member Config_ : Lens<SpiralsProbesBranching1, _> = 
        (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
    static member N_ : Lens<SpiralsProbesBranching1, _> = 
        (fun x -> x.N), (fun value x -> let y = x.Clone() in y.N <- value; y)
    static member LengthRange_ : Lens<SpiralsProbesBranching1, _> = 
        (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
    static member Delay_ : Lens<SpiralsProbesBranching1, _> = 
        (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)

    override this.Evaluate(env) =
        let c = env.Bounds.Center
        Seq.iter
            (fun p -> 
                let d = p - c
                env.AddCollider(SegmentCollider(None, p, Point(c.X + (float32(sign d.X) * 0.001f), p.Y)))
                env.AddCollider(SegmentCollider(None, p, Point(p.X, c.Y + (float32(sign d.Y) * 0.001f)))))
            env.Bounds.Corners
        let d = env.Bounds.Width / float32(this.N + 1)
        Seq.iter
            (fun x -> 
                let finalConfig = this.Config|> Optic.set totalDistance_ (env.Random.NextInRange this.LengthRange)
                let spawnOptions = ProbeSpawnOptions(Position=Point(x, env.Bounds.Top + 1.0f), Direction=Point(0.0f, 1.0f))
                spawnOptions.DelayBy(int(env.Random.NextFloat() * float32(this.Delay)))
                let probeData = env.LaunchProbe(finalConfig, spawnOptions)
                env.Spawn(
                    finalConfig|> Optic.set totalDistance_ probeData.S, 
                    spawnOptions, 
                    fun (a : DworkinAgent) -> 
                        probeData.Id <- a.Id
                        probeData.SpawnColliders()
                        a.NoCollisions <- true)
                    |> ignore)
            (seq { (env.Bounds.Left + d) .. d .. (env.Bounds.Right - d) })
                