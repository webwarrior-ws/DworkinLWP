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
        DworkinEnvironmentConfigCustom.PCB_RandomPosChips(
            RandomSeed=seed,
            Angle=getValue <@ this.Angle @>,
            Size=getValue <@ this.Size @>,
            Vias=getValue <@ this.Vias @>,
            Endpoints=getValue <@ this.Endpoints @>,
            Density=getValue <@ this.Density @>) 
        :> EnvironmentConfig


type Maze() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()

    [<FloatRange(0.5f, 2.0f)>]
    member val WallThickness = Defined(1.0f) with get, set

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

        let mutable agentConfig = MiscConfigs.DworkinAgentConfig.straightMazeRandomAngle_v2
        // TODO: different angles
        agentConfig <- 
            agentConfig 
            |> Optic.set (speed_ >-> magnitude_) (getValue <@ this.Speed @>)
            |> Optic.map (thickness_ >-> cast<_, ThicknessFormula.Constant> >-> c_) (fun c -> c * (getValue <@ this.WallThickness @>))
        if getValue <@ this.Curvy @> then
            agentConfig <- agentConfig |> Optic.set (curvature_) (CurvatureFormula.SNoise(A=0.075f, B=5.0f, D=0.61f))

        let config =
            DworkinEnvironmentConfigCustom.LWPMaze2(
                Scale=getValue <@ this.Scale @>,
                InitialAngle=getValue <@ this.Orientation @>,
                AgentConfig=agentConfig,
                RandomSeed=seed) 

        config :> EnvironmentConfig



let allEnvironmentSettingsTypes = [| typeof<MagneticSpiralsBunch>; typeof<PCB_RandomPosChips>; typeof<Maze> |]
