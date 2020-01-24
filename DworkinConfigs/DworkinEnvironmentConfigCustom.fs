module DworkinEnvironmentConfigCustom


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
open LibDworkin.SimplexNoise

open MiscConfigs
open LibDworkin



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

    
type MagneticSpiralsBunchBranching2_3() = 
    inherit DworkinEnvironmentConfigCustom()
    member val Config = SpawnConfig.ProbeBunchMagneticSpirals2Branch3() with get, set
    member val Scale = 1.25f with get, set

    member this.Clone() = this.MemberwiseClone() :?> MagneticSpiralsBunchBranching2_3

    static member Config_ : Lens<MagneticSpiralsBunchBranching2_3, _> = 
        (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
    
    static member Scale_ : Lens<MagneticSpiralsBunchBranching2_3, _> = 
        (fun x -> x.Scale), (fun value x -> let y = x.Clone() in y.Scale <- value; y)

    override this.Evaluate(env) =
        let bounds = env.Bounds
        let c = bounds.Center
        for p in bounds.Corners do
            let d = p - c
            env.AddCollider(SegmentCollider(None, p, Point(c.X + (float32(sign d.X) * 0.001f), p.Y)))
            env.AddCollider(SegmentCollider(None, p, Point(p.X, c.Y + (float32(sign d.Y) * 0.001f))))
            
        env.Spawn(this.Config, AgentSpawnOptions(Position=env.Bounds.BottomCenter - Point(0.0f, 1.0f)), fun a -> a.ScaleBy(this.Scale))


type LWPMaze3() = 
    inherit DworkinEnvironmentConfigCustom()
    member val AgentConfig = DworkinAgentConfig.straightMazeRandomAngle_v2 with get, set
    member val Delay = 180 with get, set
    member val Scale = 1.0f with get, set
    member val NumAgents = 6 with get, set
    member val Mirror = true with get, set
    member val InitialAngle = 0 with get, set
    member val AngleIncrement = 90 with get, set
    member val GridSize = 60.0f with get, set
    member val ThicknessVariance = 0.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> LWPMaze3

    static member AgentConfig_ : Lens<LWPMaze3, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member Delay_ : Lens<LWPMaze3, _> = 
        (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
    static member Scale_ : Lens<LWPMaze3, _> = 
        (fun x -> x.Scale), (fun value x -> let y = x.Clone() in y.Scale <- value; y)
    static member NumAgents_ : Lens<LWPMaze3, _> = 
        (fun x -> x.NumAgents), (fun value x -> let y = x.Clone() in y.NumAgents <- value; y)
    static member AngleIncrement_ : Lens<LWPMaze3, _> = 
        (fun x -> x.AngleIncrement), (fun value x -> let y = x.Clone() in y.AngleIncrement <- value; y)
    static member Mirror_ : Lens<LWPMaze3, _> = 
        (fun x -> x.Mirror), (fun value x -> let y = x.Clone() in y.Mirror <- value; y)
    static member InitialAngle_ : Lens<LWPMaze3, _> = 
        (fun x -> x.InitialAngle), (fun value x -> let y = x.Clone() in y.InitialAngle <- value; y)
    static member GridSize_ : Lens<LWPMaze3, _> = 
        (fun x -> x.GridSize), (fun value x -> let y = x.Clone() in y.GridSize <- value; y)
    static member ThicknessVariance_ : Lens<LWPMaze3, _> = 
        (fun x -> x.ThicknessVariance), (fun value x -> let y = x.Clone() in y.ThicknessVariance <- value; y)

    override this.Evaluate(env) =
        let positions = ResizeArray()
        let cellSize = this.GridSize * this.Scale
        let delay = int(float32(this.Delay) / (this.AgentConfig.Speed :?> AgentSpeedConstant).Magnitude)
        
        for x in (env.Bounds.Left + cellSize) .. cellSize .. (env.Bounds.Right - cellSize) do
            for y in (env.Bounds.Top + cellSize) .. cellSize .. (env.Bounds.Bottom - cellSize) do
                positions.Add(Point(x, y))
        positions.ShuffleBy(env.Random)

        let agentBlock (a : DworkinAgent) = 
            a.NoCollisions <- true
            a.ScaleBy(this.Scale)
        let mutable config = this.AgentConfig |> Optic.map (speed_ >-> magnitude_) (fun x -> x / this.Scale)

        let spawnAgent pos angle delayed = 
            let angle = float32 angle
            let spawnOpts = ProbeSpawnOptions(Position=pos, AngleDeg=angle)
            if delayed then
                spawnOpts.DelayBy(delay)
            env.LaunchProbe(config, spawnOpts, agentBlock).SpawnColliders()
            env.Spawn(config, spawnOpts, agentBlock) |> ignore
        
        let angles = [| 0 .. this.AngleIncrement .. 360 |]
        for i in 1 .. this.NumAgents do
            let d = (i - 1) * delay
            env.Delay(
                (fun _ ->
                    let mutable triesLeft = 100
                    while triesLeft > 0 && positions.Count > 0 do
                        let p = positions.RemoveLast()
                        if not <| env.TestForCollision(CollisionLayers.All, CircleCollider(None, p, cellSize)) then
                            triesLeft <- 0
                            let c = (this.AgentConfig.Thickness :?> ThicknessFormula.Constant).C
                            config <- 
                                config
                                |> Optic.set 
                                    (thickness_ >-> cast<_,  ThicknessFormula.Constant> >-> c_) 
                                    (env.Random.NextInRange(c * (1.0f - this.ThicknessVariance), c * (1.0f + this.ThicknessVariance)))
                            let angle = this.InitialAngle + angles.AtRandom(env.Random)
                            let offset = Point(float32(sign p.X) * 0.5f, float32(sign p.Y) * 0.5f)
                            spawnAgent (p - offset) angle false
                            if this.Mirror then
                                spawnAgent (p - offset) (180 + angle) true
                        triesLeft <- triesLeft - 1
                    None),
                d)
            
    
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
            |> Optic.map (terminationConditions_ >-> collision_ >-> primaryDetector_ >-> distance_) (fun x -> x / this.Density)
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