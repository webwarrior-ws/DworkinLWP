module rec PCB


open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Geometry
open LibDworkin.AgentAbstractions
open LibDworkin.Agents
open LibDworkin.Probes
open LibDworkin.Configs
open LibDworkin.Lenses
open LibDworkin.SpecializedCollections
open LibDworkin.PharoCompatibility
open LibDworkin.Colliders
open LibDworkin.SpatialIndex

open MiscConfigs
open BranchingCriteria


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


module SpawnConfig =
    type PCBBranchOffset() = 
        inherit SpawnConfig()
        member val RecursionLimit = 3 with get, set
        member val ThicknessScale = 1.0f with get, set
        member val Delay = 0 with get, set
        member val Flip = false with get, set
        member val BranchProb = 0.75f with get, set
        member val MinDistance = 20.0f with get, set
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
        static member MinDistance_ : Lens<PCBBranchOffset, _> = 
            (fun x -> x.MinDistance), (fun value x -> let y = x.Clone() in y.MinDistance <- value; y)
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
                
                let agentCopy = agent.Copy()
                agent.DworkinEnvironment.Delay(
                    (fun _ -> 
                        let spawnOptions = ProbeSpawnOptions(AngleDeg=angle, OffsetSideways=this.Offset) 
                        let probeData = agentCopy.LaunchProbe(agent.Config |> Optic.set totalDistance_ this.MinDistance, spawnOptions)
                        if probeData.ReachedEnd then
                            let mutable config = 
                                agent.Config 
                                |> Optic.set 
                                    (branching_ >-> BranchingCriterionBased.SpawnConfig_ >-> cast<SpawnConfig, PCBBranchOffset> >-> recursionLimit_) 
                                    (this.RecursionLimit - 1)
                            try 
                                let lengthRange : NumberRange = terminationConfig.LengthRange
                                config <- config |> Optic.set totalDistance_ (agent.Random.NextInRange(lengthRange))
                            with | _ -> ()
                            
                            agentCopy.Spawn(
                                config, 
                                spawnOptions, 
                                fun a -> 
                                    probeData.Id <- a.Id
                                    probeData.SpawnColliders()
                                    a.ScaleBy(Point(this.ThicknessScale, 1.0f))
                                    if this.StartWithVias then
                                        let terminalConfig : SpawnConfig.CircleAdvanced = 
                                            a.Config
                                            |> Optic.get (termination_spawnConfig_ >-> cast<SpawnConfig, PCBTraceLengthLimited> >-> terminalConfig_)
                                        let aCopy = a.Copy()
                                        aCopy.Direction <- -aCopy.Direction
                                        terminalConfig.Evaluate(aCopy) )
                        None), 
                    this.Delay)

    
    let termination_lengthLimit_ = 
        termination_spawnConfig_ >-> cast<SpawnConfig, PCBTraceLengthLimited> >-> lengthLimit_


    type PCBTraceLengthLimited() = 
        inherit SpawnConfig()
        member val Angles = [| -45.0f |] with get, set
        member val LengthRange = NumberRange(40.0f, 120.0f) with get, set
        member val TerminalConfig = SpawnConfig.CircleAdvanced(Offset=0.75f) with get, set
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
                        let spawnOptions = ProbeSpawnOptions(Direction=p.Direction)
                        let probeData = agent.LaunchProbe(agent.Config |> Optic.set totalDistance_ 10.0f, spawnOptions)
                        if probeData.ReachedEnd then
                            let numSteps = int(agent.ThicknessFactor / agent.Speed) |> max 1
                            Util.drawJoint(agent, p.Direction, Util.JointType.Bevel, numSteps, 0)
                            spawnOptions.DelayBy(numSteps)
                            agent.Spawn(agent.Config, spawnOptions)
                        else
                            this.TerminalConfig.Evaluate(agent)
                | None -> 
                    let spawnOptions = ProbeSpawnOptions(AngleDeg=angle)
                    let flip = (angle <> 0.0f) && (agent.Random.NextFloat() < this.FlipProbability) 
                    let agentBlock (a : DworkinAgent) = if flip then a.Flip()
                    if angle <> 0.0f && agent.LaunchProbe(config |> Optic.set totalDistance_ 10.0f, spawnOptions, agentBlock).S > 0.0f then
                        let numSteps = int(agent.ThicknessFactor / agent.Speed) |> max 1
                        Util.drawJoint(agent, agent.Direction.Rotate(deg2rad(angle) * agent.FlipFactor), Util.JointType.Bevel, numSteps, 0)
                        spawnOptions.DelayBy(numSteps)
                    agent.Spawn(config, spawnOptions, agentBlock)


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
            let interval : float32 seq = 
                let w = float32 this.Width
                let n = float32 this.N
                if isOdd this.N then
                    Seq.append 
                        (Seq.rev (seq { 0.0f .. (w / (1.0f - n)) .. (w / (-2.0f)) })) 
                        (seq { (w / (n - 1.0f)) .. (w / (n - 1.0f)) .. (w / 2.0f) })
                else
                    seq { (w / (-2.0f)) .. (w / (n - 1.0f)) .. (w / 2.0f) }

            withIndexDo
                (fun dx i -> 
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
                                Offset=(this.Config |> Optic.get (termination_terminalConfig_ >-> radius_)) * (agent.ThicknessFactor / agent.LengthFactor) * 0.25f
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


module BranchingCriterionBased = 
    module PCBRandomizedOffset = 
        type Criterion() = 
            inherit BranchingCriterionCustom()
            member val MinRange = NumberRange(0.2f, 0.5f) with get, set
            member val MaxRange = NumberRange(0.5f, 0.8f) with get, set
            member val D = 10.0f with get, set
            
            member this.Clone() = this.MemberwiseClone() :?> Criterion
            
            static member MinRange_ : Lens<Criterion, _> = 
                (fun x -> x.MinRange), (fun value x -> let y = x.Clone() in y.MinRange <- value; y)
            static member MaxRange_ : Lens<Criterion, _> = 
                (fun x -> x.MaxRange), (fun value x -> let y = x.Clone() in y.MaxRange <- value; y)
            static member D_ : Lens<Criterion, _> = 
                (fun x -> x.D), (fun value x -> let y = x.Clone() in y.D <- value; y)
            
            override this.Evaluate(agent, mark, tag) =
                let tInterval = 
                    agent.Attributes.BranchInterval.GetOrSetIfAbsent(
                        fun () -> NumberRange([| agent.Random.NextInRange(this.MinRange); agent.Random.NextInRange(this.MaxRange) |]))
                (mark.S + this.D < agent.S) && (tInterval.Contains agent.T)


module DworkinEnvironmentConfigCustom = 
    module PCB_GridTraces = 
        module AgentConfig = 
            module Branching = 
                type Criterion() = 
                    inherit BranchingCriterionCustom()
                    member val NRange = NumberRange(1.0f, 3.0f) with get, set
                    member val TRange = NumberRange(0.2f, 0.8f) with get, set
                    
                    member this.Clone() = this.MemberwiseClone() :?> Criterion
                    
                    static member NRange_ : Lens<Criterion, _> = 
                        (fun x -> x.NRange), (fun value x -> let y = x.Clone() in y.NRange <- value; y)
                    static member TRange_ : Lens<Criterion, _> = 
                        (fun x -> x.TRange), (fun value x -> let y = x.Clone() in y.TRange <- value; y)
                    
                    override this.Evaluate(agent, mark, tag) =
                        let n = agent.Attributes.BranchesN.GetOrSetIfAbsent(fun () -> int(round(agent.Random.NextInRange(this.NRange)))) 
                        agent.S >= (mark.S + agent.TotalDistance * (this.TRange.Min * float32(n) + this.TRange.Max) / float32(n + 1)) && agent.TotalNumBranches < n



type Endpoints =
    | Hollow = 1
    | Filled = 2


type PCB_RandomPosChips() = 
    inherit DworkinEnvironmentConfigCustom()
    member val Angle = 0.0f with get, set
    member val AgentConfig = SpawnConfig.PCBTracesFromChip() with get, set
    member val NxRange = NumberRange(2.0f, 8.0f) with get, set
    member val NyRange = NumberRange(4.0f, 8.0f) with get, set
    member val ThicknessVariance = 0.0f with get, set
    member val Density = 0.5f with get, set
    member val Endpoints = Endpoints.Hollow with get, set
    member val Vias = false with get, set
    member val Angles = [| -45.0f |] with get, set
    member val Branch = false with get, set
    member val BranchingConfig = 
        BranchingCriterionBased(
            Criterion=BranchingCriterionCustom.IntervalBoundedTRand(D=10.0f),  
            SpawnConfig=SpawnConfig.PCBBranchOffset(RecursionLimit=4))
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
        let mutable ts = [| for k in 1..n -> ((env.Random.NextGaussian() |> min 1.0f) * this.ThicknessVariance) + 1.0f |]
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
                    match this.Endpoints with
                    | Endpoints.Hollow -> "contour"
                    | Endpoints.Filled -> "filled"))
            |> Optic.set (groupConfig_ >-> config_ >-> termination_spawnConfig_ >-> cast >-> SpawnConfig.PCBTraceLengthLimited.TerminalConfig_ >-> hollowPart_) 
                ((match this.Endpoints with | Endpoints.Hollow -> 0.5f | Endpoints.Filled -> 0.0f))
            |> Optic.set (groupConfig_ >-> config_ >-> termination_spawnConfig_ >-> cast >-> SpawnConfig.PCBTraceLengthLimited.Angles_) this.Angles
            |> Optic.set (groupConfig_ >-> config_ >-> branching_) 
                (if this.Branch then
                    this.BranchingConfig |> Optic.set (spawnConfig_ >-> cast<SpawnConfig, SpawnConfig.PCBBranchOffset> >-> branchProb_) this.Density :> BranchingStrategy
                  else
                    BranchingNone() :> BranchingStrategy)
            |> Optic.set (groupConfig_ >-> config_ >-> speed_ >-> cast<AgentSpeed, AgentSpeedThickness> >-> factor_) this.Speed
        
        withIndexDo
            (fun struct (pos, size) k -> 
                let currThickness = ts.[k-1]
                let mutable dist = (min width height) * 0.5f
                for struct (sp, ss) in spawnSites do 
                    if sp <> pos then
                        let p = ((pos - sp).Abs()) - ((size + ss) * gap)
                        dist <- min dist (max p.X p.Y)
                if this.Branch then
                    configToSpawn <- 
                        configToSpawn
                        |> Optic.map 
                            (groupConfig_ >-> config_ >-> branching_) 
                            (fun branching -> 
                                branching
                                |> Optic.map (criterion_ >-> BranchingCriterionCustom.IntervalBoundedTRand.D_) (fun d ->  d * currThickness / sqrt(this.Size))
                                |> Optic.map (spawnConfig_ >-> cast >-> SpawnConfig.PCBBranchOffset.Offset_) (fun d -> d * sqrt(currThickness)) )
                let opts = AgentSpawnOptions(Position=pos, AngleDeg=this.Angle)
                if k > 1 then
                    opts.DelayBy(int ((env.Random.NextFloat() * dist) / this.Speed / this.Size))
                env.Spawn(
                    configToSpawn
                    |> Optic.set nx_ (int size.X)
                    |> Optic.set ny_ (int size.Y)
                    |> Optic.map gap_ (fun x -> x * currThickness), 
                    opts,
                    fun a -> a.ScaleBy(Point(currThickness, 1.0f)))
                    |> ignore)
            spawnSites


type PCB_RandomPosTraces() = 
    inherit DworkinEnvironmentConfigCustom()
    
    static let terminarion_spawnConfig_ = termination_<TerminationSpawn> >-> spawnConfig_ >-> cast<_, SpawnConfig.PCBTraceLengthLimited>

    member val AgentConfig = 
        DworkinAgentConfig.pCBTrace
        |> Optic.set branching_ (
            BranchingCriterionBased(
                Criterion=BranchingCriterionBased.PCBRandomizedOffset.Criterion(),
                SpawnConfig=(SpawnConfig.PCBBranchOffset(RecursionLimit=6,BranchProb=0.8f,MinDistance=15.0f,MinGeneration=1)))
            )
        |> Optic.set brush_ (BrushStrategySolid())
        |> Optic.set (terminarion_spawnConfig_ >-> lengthRange_) (NumberRange(35.0f, 75.0f))
        |> Optic.set (terminarion_spawnConfig_ >-> lengthLimit_) 400.0f
        |> Optic.set (terminationConditions_ >-> collision_ >-> primaryDetector_ >-> distance_) 7.5f with get, set
    member val Angles = [| -45.0f |] with get, set
    member val InitialLengthRange = NumberRange(25.0f, 75.0f) with get, set
    member val ThicknessVariance = 0.5f with get, set
    member val Size = 1.5f with get, set
    member val Speed = 1.0f with get, set
    member val Endpoints = Endpoints.Hollow with get, set
    member val SitesDensity = 0.5f with get, set
    member val BranchDensity = 0.5f with get, set
    member val MinThickness = 1.0f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> PCB_RandomPosTraces
    
    static member AgentConfig_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member Angles_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.Angles), (fun value x -> let y = x.Clone() in y.Angles <- value; y)
    static member InitialLengthRange_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.InitialLengthRange), (fun value x -> let y = x.Clone() in y.InitialLengthRange <- value; y)
    static member ThicknessVariance_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.ThicknessVariance), (fun value x -> let y = x.Clone() in y.ThicknessVariance <- value; y)
    static member Size_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.Size), (fun value x -> let y = x.Clone() in y.Size <- value; y)
    static member Speed_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.Speed), (fun value x -> let y = x.Clone() in y.Speed <- value; y)
    static member Endpoints_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.Endpoints), (fun value x -> let y = x.Clone() in y.Endpoints <- value; y)
    static member SitesDensity_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.SitesDensity), (fun value x -> let y = x.Clone() in y.SitesDensity <- value; y)
    static member BranchDensity_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.BranchDensity), (fun value x -> let y = x.Clone() in y.BranchDensity <- value; y)
    static member MinThickness_ : Lens<PCB_RandomPosTraces, _> = 
        (fun x -> x.MinThickness), (fun value x -> let y = x.Clone() in y.MinThickness <- value; y)
    
    override this.Evaluate(env) =
        let width = env.Bounds.Width
        let height = env.Bounds.Height
        let radius = 10.0f * (this.Size + (this.ThicknessVariance / 2.0f))
        let spawnSites = ResizeArray()
        let mutable n = ((this.SitesDensity * (width * height)) / (squared radius)) / 25.0f |> floor |> int
        let ts = 
            Array.init 
                n 
                (fun _ -> 
                    let x = max ((((env.Random.NextGaussian()) * this.ThicknessVariance) + 1.0f) * this.Size) this.MinThickness
                    x 
                    |> min ((this.Size * (this.ThicknessVariance + 1.0f)) * 2.0f)
                    |> max ((this.Size / (this.ThicknessVariance + 1.0f)) / 2.0f) )
        let spawnAngles = 
            this.Angles
            |> Seq.collect (fun x -> seq { 0.0f .. (abs x) .. 359.0f })
            |> Seq.distinct
            |> Seq.toArray

        let spawn = CollisionLayer(33) // layer id, any unique number is ok
        while spawnSites.Length < n do
            let p = env.Random.NextInRectangle(env.Bounds.InsetBy(radius))
            let collider = CircleCollider(None, p, radius)
            let collides = env.QueryColliders(collider.BoundingBox, CollisionLayers([| spawn |], false), fun _ each -> collide each collider)
            if not <| collides then 
                env.AddColliderToLayer(collider, spawn)
                spawnSites.Add(p)
        env.RemoveColliderLayer(spawn)
        spawnSites |> withIndexDo 
            (fun p i ->
                let mutable dist = (min width height) / 2.0f
                spawnSites
                |> Seq.except [| p |]
                |> Seq.iter
                    (fun each ->
                        let d = (p - each).Abs()
                        dist <- min dist ((max d.X d.Y) + ((min d.X d.Y) * ((sqrt 2.0f) - 1.0f))) )
                let thickness = ts.[i-1]
                let multByThickness = fun x -> x * thickness
                let delay = ((env.Random.NextFloat()) * dist) / (this.AgentConfig.Speed :?> AgentSpeedThickness).Factor
                let spawnOptions = AgentSpawnOptions(Position=p,AngleDeg=spawnAngles.AtRandom(env.Random))
                spawnOptions.DelayBy(int delay)
                let branching_spawnConfig_ = branching_<BranchingCriterionBased> >-> spawnConfig_ >-> cast<_,SpawnConfig.PCBBranchOffset>
                env.Spawn(
                    this.AgentConfig
                    |> Optic.set totalDistance_ (env.Random.NextInRange(this.InitialLengthRange))
                    |> Optic.set (speed_<AgentSpeedThickness> >-> factor_) this.Speed
                    |> Optic.set (terminarion_spawnConfig_ >-> terminalConfig_ >-> hollowPart_) (match this.Endpoints with | Endpoints.Hollow -> 0.5f | _ -> 0.0f)
                    |> Optic.set (terminarion_spawnConfig_ >-> angles_) this.Angles
                    |> Optic.map (branching_ >-> criterion_<BranchingCriterionBased.PCBRandomizedOffset.Criterion> >-> d_) multByThickness
                    |> Optic.map (branching_spawnConfig_ >-> offset_) multByThickness
                    |> Optic.set (branching_spawnConfig_ >-> branchProb_) this.BranchDensity, 
                    spawnOptions, 
                    fun (a : DworkinAgent) ->
                        a.ThicknessFactor <- thickness
                        let temp = a.Copy()
                        temp.Direction <- -a.Direction
                        temp.Position <- 
                            let r = ((this.AgentConfig.Termination :?> TerminationSpawn).SpawnConfig :?> SpawnConfig.PCBTraceLengthLimited).TerminalConfig.Radius
                            a.Position - (((a.Direction * r) * thickness) * 0.25f)
                        a.Config.Termination.Evaluate(temp) )
                |> ignore ) 
            

type PCB_GridTraces() = 
    inherit DworkinEnvironmentConfigCustom()
    member val AgentConfig = 
        { DworkinAgentConfig.Default with 
            Branching=
                BranchingCriterionBased(
                    Criterion=DworkinEnvironmentConfigCustom.PCB_GridTraces.AgentConfig.Branching.Criterion(),
                    SpawnConfig=SpawnConfig.PCBBranchOffset(
                        RecursionLimit=2,Delay=10,MinDistance=20.0f,Offset=0.0f,
                        Direction="perpendicular",StartWithVias=false,Alternate=true,MinGeneration=1)
                );
            Brush=BrushStrategySolid();
            Speed=AgentSpeedThickness(Factor=0.25f);
            ColliderOptions=ColliderOptionsGeneral();
            Termination=TerminationSpawn(SpawnConfig=SpawnConfig.PCBTraceLengthLimited(LengthLimit=150.0f));
            Thickness=ThicknessFormula.Constant(C=2.0f);
            TerminationConditions={ 
                Collision=
                    Some(
                        TerminationConditionCollision(
                            IgnoreParent=true,
                            SecondaryDetector=Some({ CollisionDetectorConfig.Default with Angle=180.0f;Distance=2.5f }),
                            SelfIntersection=false
                    )
                    |> Optic.set (primaryDetector_ >-> angle_) 45.0f
                    |> Optic.set (primaryDetector_ >-> distance_) 5.0f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance())
             };
             TotalDistance=15.0f;
         } with get, set
    member val PosSpread = 0.0f with get, set
    member val DelayFactor = 1.0f with get, set
    member val ThicknessVariance = 0.0f with get, set
    member val Branch = false with get, set
    member val Angles = [| -45.0f |] with get, set
    member val Speed = 0.5f with get, set
    member val Size = 1.0f with get, set
    member val Endpoints = Endpoints.Hollow with get, set
    member val Density = 0.5f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> PCB_GridTraces
    
    static member AgentConfig_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member PosSpread_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.PosSpread), (fun value x -> let y = x.Clone() in y.PosSpread <- value; y)
    static member DelayFactor_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.DelayFactor), (fun value x -> let y = x.Clone() in y.DelayFactor <- value; y)
    static member ThicknessVariance_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.ThicknessVariance), (fun value x -> let y = x.Clone() in y.ThicknessVariance <- value; y)
    static member Branch_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.Branch), (fun value x -> let y = x.Clone() in y.Branch <- value; y)
    static member Angles_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.Angles), (fun value x -> let y = x.Clone() in y.Angles <- value; y)
    static member Speed_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.Speed), (fun value x -> let y = x.Clone() in y.Speed <- value; y)
    static member Size_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.Size), (fun value x -> let y = x.Clone() in y.Size <- value; y)
    static member Endpoints_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.Endpoints), (fun value x -> let y = x.Clone() in y.Endpoints <- value; y)
    static member Density_ : Lens<PCB_GridTraces, _> = 
        (fun x -> x.Density), (fun value x -> let y = x.Clone() in y.Density <- value; y)
    
    override this.Evaluate(env) =
        let width = env.Bounds.Width
        let height = env.Bounds.Height
        let n = int(floor(sqrt(width * height * this.Density) / this.Size)) / (if this.Branch then 60 else 30)
        let nx, ny =
            if width > height then 
                int(floor ((float32(n) * width) / height)), n
            else 
                n, int(floor ((float32(n) * height) / width))
        let dist = min (width / float32(nx)) (height / float32(ny))
        
        let mutable config = 
            let termination_spawnConfig_ = termination_<TerminationSpawn> >-> spawnConfig_ >-> cast<_, SpawnConfig.PCBTraceLengthLimited>
            this.AgentConfig
            |> Optic.set 
                (termination_spawnConfig_ >-> terminalConfig_ >-> hollowPart_) 
                (match this.Endpoints with | Endpoints.Hollow -> 0.5f | _ -> 0.0f)
            |> Optic.set (termination_spawnConfig_ >-> angles_) this.Angles
            |> Optic.set (speed_<AgentSpeedThickness> >-> factor_) this.Speed
        if this.Branch then 
            config <- 
                config 
                |> Optic.set 
                    (branching_<BranchingCriterionBased> >-> spawnConfig_ >-> cast<_,SpawnConfig.PCBBranchOffset> >-> branchProb_) 
                    (sqrt this.Density)
        else 
            config <- config |> Optic.set branching_ BranchingNone.Instance
        
        let spawnAngles = 
            this.Angles
            |> Seq.collect (fun x -> seq { 0.0f .. float32(abs x) .. 359.0f })
            |> Seq.distinct
            |> Seq.toArray
        let thicknessRange = NumberRange(lerp 2.5f 1.5f this.ThicknessVariance, lerp 2.5f 3.5f this.ThicknessVariance)

        for y in (height * float32(1 - ny)) / (float32(2 * ny) + 2.0f) .. height / float32(ny + 1) .. (height / 2.0f) - 1.0f do
            for x in (width * float32(1 - nx)) / (float32(2 * nx) + 2.0f) .. width / float32(nx + 1) .. (width / 2.0f) - 1.0f do
                let shift = (((Point(env.Random.NextGaussian(), env.Random.NextGaussian())) * this.PosSpread) * dist).Round()
                let p = (Point(x, y) + env.Bounds.Origin + Point(width/2.0f, height/2.0f)) + shift
                assert(env.Bounds.ContainsPoint(p))
                let delay = int(floor(env.Random.NextFloat() * dist * this.DelayFactor / this.Speed))
                let theta = spawnAngles.AtRandom(env.Random)
                if env.Random.NextFloat() < this.Density then 
                    let trySpawnAgent _ =
                        let thickness = env.Random.NextInRange(thicknessRange)
                        let r = ((config.Termination :?> TerminationSpawn).SpawnConfig :?> SpawnConfig.PCBTraceLengthLimited).TerminalConfig.Radius
                        let collider = CircleCollider(None, p, r * thickness * this.Size * 2.0f)
                        if not <| env.TestForCollision(config.TerminationConditions.Collision.Value.Layers, collider) then 
                            for thetaPrime in [| theta; theta + 180.0f |] do
                                env.Spawn(
                                    config |> Optic.set (thickness_<ThicknessFormula.Constant> >-> c_) thickness,
                                    AgentSpawnOptions(Position=p,AngleDeg=thetaPrime),
                                    fun a -> a.ScaleBy(this.Size)) |> ignore
                        None
                    env.Delay(trySpawnAgent, delay)
