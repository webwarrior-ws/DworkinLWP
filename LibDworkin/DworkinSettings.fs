module LibDworkin.DworkinSettings

open RendererComponents

open Settings


type SettingsHolder =
    {
        EnvironmentConfig : Configs.DworkinEnvironmentConfig
        Effect : Effect
        Foreground : Material
        Background : Material
    }


module CurrentSettings =
    let mutable private currSettings : SettingsHolder option = None

    let get() : SettingsHolder = currSettings.Value
    let set(value : SettingsHolder) = currSettings <- Some(value)
    let isSet() = currSettings.IsSome


[<AbstractClass>] 
type EnvironmentSettings() =
    inherit Settings()
    abstract member Build : unit -> Configs.DworkinEnvironmentConfig

[<AbstractClass>] 
type MaterialSettings() =
    inherit Settings()
    abstract member Build : unit -> Material
