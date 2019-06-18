module LibDworkin.DworkinSettings

open RendererComponents

open Settings


[<AbstractClass>] 
type EnvironmentSettings() =
    inherit Settings()
    abstract member Build : int -> Configs.DworkinEnvironmentConfig

[<AbstractClass>] 
type MaterialSettings() =
    inherit Settings()
    abstract member Build : AbstractPlatform * System.Random -> Material

[<AbstractClass>] 
type EffectSettings() =
    inherit Settings()
    abstract member Build : AbstractPlatform * System.Random -> Effect


type SolidColorMaterialSettings() =
    inherit MaterialSettings()

    [<Color>]
    member val Color = Defined(OpenTK.Graphics.Color4.White) with get, set

    override this.Build(platform, random) =
        SolidColorMaterial(platform, Randomizeable.getValue random <@ this.Color @>) :> Material


type PlainEffectSettings() =
    inherit EffectSettings()
    override this.Build(platform, random) = PlainEffect(platform) :> Effect


type SettingsHolder =
    {
        Environment : EnvironmentSettings
        Effect : EffectSettings
        Foreground : MaterialSettings
        Background : MaterialSettings
        Seed : int
    }


module CurrentSettings =
    let mutable private currSettings : SettingsHolder option = None

    let get() : SettingsHolder = currSettings.Value
    let set(value : SettingsHolder) = currSettings <- Some(value)
    let isSet() = currSettings.IsSome

