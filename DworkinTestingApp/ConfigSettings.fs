module ConfigSettings

open Settings


type EnvironmentConfig = LibDworkin.Configs.DworkinEnvironmentConfig


type PCB_RandomPosChips() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()

    override this.Build(seed) =
        GeneratedConfigs.PCB_RandomPosChips(RandomSeed=seed) :> EnvironmentConfig


let allEnvironmentSettingsTypes = [| typeof<PCB_RandomPosChips>; |]
