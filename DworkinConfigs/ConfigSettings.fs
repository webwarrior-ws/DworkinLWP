module ConfigSettings

open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Lenses

open Settings

type EnvironmentConfig = LibDworkin.Configs.DworkinEnvironmentConfig


type MagneticSpiralsBunch() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()

    [<FloatRange(1.5f, 3.0f, Step=0.1f)>]
    member val Scale = Defined(2.0f) with get, set

    [<IntRange(2, 5)>]
    member val MaxInitialAgents = Defined(3) with get, set

    override this.Build(seed) =
        let random = System.Random(seed)
        let getValue expr = Randomizeable.getValue random expr
        let config = DworkinEnvironmentConfigCustom.MagneticSpiralsBunchBranching2_3(Scale=getValue <@ this.Scale @>) 
        config.Config.MaxInitialAgents <- getValue <@ this.MaxInitialAgents @>
        config :> EnvironmentConfig


type PCB_RandomPosChips() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()

    [<FloatRange(0.0f, 90.0f, Step=45.0f)>]
    member val Angle = Defined(0.0f) with get, set

    [<FloatRange(0.75f, 1.5f)>]
    member val Size = Defined(1.0f) with get, set
    
    [<Bool>]
    member val Vias = Defined(false) with get, set

    [<Enum([| "hollow"; "filled" |])>]
    member val Endpoints = Defined("hollow") with get, set

    [<FloatRange(0.2f, 0.8f, Step=0.05f)>]
    member val Density = Defined(0.5f) with get, set

    override this.Build(seed) =
        let random = System.Random(seed)
        let getValue expr = Randomizeable.getValue random expr
        PCB.PCB_RandomPosChips(
            RandomSeed=seed,
            Angle=getValue <@ this.Angle @>,
            Size=getValue <@ this.Size @>,
            Vias=getValue <@ this.Vias @>,
            Endpoints=getValue <@ this.Endpoints @>,
            Density=getValue <@ this.Density @>) 
        :> EnvironmentConfig


type Maze() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()

    [<Enum([| "Square"; "FortyFive"; "FortyFiveSharp"; "Hexagonal"; "Qbert"; "Arbitrary"; "Arbitrary2" |])>]
    member val Angles = Defined("Square") with get, set

    [<FloatRange(0.5f, 2.0f)>]
    member val WallThickness = Defined(1.0f) with get, set

    [<FloatRange(0.0f, 0.5f)>]
    member val ThicknessVariance = Defined(0.0f) with get, set

    [<Bool>]
    member val Curvy = Defined(false) with get, set

    [<Enum([| 0; 45 |])>]
    member val Orientation = Defined(0) with get, set

    [<FloatRange(0.5f, 1.5f)>]
    member val Scale = Defined(1.0f) with get, set

    [<FloatRange(0.5f, 3.0f)>]
    member val Speed = Defined(1.0f) with get, set

    override this.Build(seed) =
        let random = System.Random(seed)
        let getValue expr = Randomizeable.getValue random expr

        let agentConfig, gridSize, angleIncr =
            match getValue <@ this.Angles @> with
            | "Square" -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v2, 60, 90
            | "FortyFive" -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v3, 42, 45
            | "FortyFiveSharp" -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v1, 42, 45
            | "Hexagonal" -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v4, 52, 60
            | "Qbert" -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v5, 52, 60
            | "Arbitrary" -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v6, 15, 60
            | "Arbitrary2" -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v7, 30, 60

        let mutable agentConfig = 
            agentConfig 
            |> Optic.set (speed_ >-> magnitude_) (getValue <@ this.Speed @>)
            |> Optic.map (thickness_ >-> cast<_, ThicknessFormula.Constant> >-> c_) (fun c -> c * (getValue <@ this.WallThickness @>))
        if getValue <@ this.Curvy @> then
            agentConfig <- agentConfig |> Optic.set (curvature_) (CurvatureFormula.SNoise(A=0.075f, B=5.0f, D=0.61f))

        let config =
            Maze.LWPMaze3(
                Scale=getValue <@ this.Scale @>,
                InitialAngle=getValue <@ this.Orientation @>,
                ThicknessVariance= getValue <@ this.ThicknessVariance @>,
                AgentConfig=agentConfig,
                GridSize=float32(gridSize),
                AngleIncrement=angleIncr,
                RandomSeed=seed) 

        config :> EnvironmentConfig


type FlowingLeavesExperiment() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()

    [<FloatRange(1.0f, 2.0f, Step=0.05f)>]
    member val Scale = Defined(1.5f) with get, set

    [<FloatRange(0.5f, 2.0f, Step=0.05f)>]
    member val Density = Defined(1.0f) with get, set

    [<FloatRange(0.0f, 1.0f, Step=0.05f)>]
    member val ScaleVariance = Defined(0.0f) with get, set
    
    [<FloatRange(0.0f, 2.0f, Step=0.1f)>]
    member val Curviness = Defined(0.0f) with get, set
    
    [<FloatRange(2.5f, 10.0f, Step=0.5f)>]
    member val LeafThickness = Defined(5.0f) with get, set
    
    [<FloatRange(40.0f, 90.0f, Step=5.0f)>]
    member val LeafLength = Defined(50.0f) with get, set
    
    [<FloatRange(0.5f, 0.8f, Step=0.05f)>]
    member val LeafShape = Defined(0.5f) with get, set

    [<FloatRange(0.25f, 1.0f, Step=0.05f)>]
    member val Speed = Defined(0.5f) with get, set

    override this.Build(seed) =
        let random = System.Random(seed)
        let getValue expr = Randomizeable.getValue random expr
        FlowingLeaves.FlowingLeaves_WIP(
            RandomSeed=seed,
            Scale=getValue <@ this.Scale @>,
            Density=(getValue <@ this.Density @>),
            ScaleVariance=getValue <@ this.ScaleVariance @>,
            Curviness=getValue <@ this.Curviness @>,
            LeafThickness=getValue <@ this.LeafThickness @>,
            LeafLength=getValue <@ this.LeafLength @>,
            LeafShape=getValue <@ this.LeafShape @>,
            Speed=getValue <@ this.Speed @>) 
        :> EnvironmentConfig


let allEnvironmentSettingsTypes = [| typeof<MagneticSpiralsBunch>; typeof<PCB_RandomPosChips>; typeof<Maze>; typeof<FlowingLeavesExperiment> |]
