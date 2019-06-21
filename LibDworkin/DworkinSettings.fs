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

type OutlineEffectSettings() =
    inherit EffectSettings()

    [<FloatRange(0.01f, 0.5f)>]
    member val OutlineWidth = Defined(0.1f) with get, set

    override this.Build(platform, random) = 
        OutlineEffect(platform, Randomizeable.getValue random <@ this.OutlineWidth @>) :> Effect


type SettingsHolder =
    {
        Environment : EnvironmentSettings
        Effect : EffectSettings
        Foreground : MaterialSettings
        Background : MaterialSettings
        Seed : int
    }

type SettingsChange =
    | EnvironmentChanged = 0b0001
    | EffectChanged      = 0b0010
    | ForegroundChanged  = 0b0100
    | BackgroundChanged  = 0b1000


module CurrentSettings =
    let mutable private currSettings : SettingsHolder option = None
    let mutable private changes = 0

    let get() : SettingsHolder = currSettings.Value
    let set(value : SettingsHolder) = currSettings <- Some(value)
    let isSet() = currSettings.IsSome

    let getChanges() = changes
    let registerChange(change : SettingsChange) = changes <- changes ||| (int change)
    let internal resetChanges() = changes <- 0

