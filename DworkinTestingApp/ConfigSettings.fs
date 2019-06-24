module ConfigSettings

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
        let config = GeneratedConfigs.MagneticSpiralsBunchBranching2_3(Scale=getValue <@ this.Scale @>) 
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
        GeneratedConfigs.PCB_RandomPosChips(
            RandomSeed=seed,
            Angle=getValue <@ this.Angle @>,
            Size=getValue <@ this.Size @>,
            Vias=getValue <@ this.Vias @>,
            Endpoints=getValue <@ this.Endpoints @>,
            Density=getValue <@ this.Density @>) 
        :> EnvironmentConfig


let allEnvironmentSettingsTypes = [| typeof<MagneticSpiralsBunch>; typeof<PCB_RandomPosChips>; |]
