module LibDworkin.DworkinSettings

open RendererComponents
open Geometry

open Settings


type OpenTK.Graphics.Color4 with
    member this.AsRGBTriple = this.R, this.G, this.B
    static member FromRGBTriple(r : float32, g : float32, b : float32) =
        OpenTK.Graphics.Color4(r, g, b, 1.0f)



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


module Materials =
    [<Name("solid color")>]
    type SolidColor() =
        inherit MaterialSettings()

        [<Color>]
        member val Color = Defined(OpenTK.Graphics.Color4.White.AsRGBTriple) with get, set

        override this.Build(platform, random) =
            SolidColorMaterial(
                platform, 
                OpenTK.Graphics.Color4.FromRGBTriple(Randomizeable.getValue random <@ this.Color @>)) 
            :> Material
    
    let allTypes = [| typeof<SolidColor>; |]


module Effects =
    [<Name("plain")>]    
    type Plain() =
        inherit EffectSettings()
        override this.Build(platform, random) = PlainEffect(platform) :> Effect

    [<Name("outline")>]
    type Outline() =
        inherit EffectSettings()

        [<FloatRange(0.01f, 0.5f)>]
        member val OutlineWidth = Defined(0.1f) with get, set

        override this.Build(platform, random) = 
            OutlineEffect(platform, Randomizeable.getValue random <@ this.OutlineWidth @>) :> Effect
    
    [<Name("shadow")>]    
    type Shadow() =
        inherit EffectSettings()

        [<FloatRange(0.0f, 360.0f, Step=5.0f, Name="Direction")>]
        member val Dir = Defined(0.0f) with get, set

        [<FloatRange(0.0f, 20.0f, Step=1.0f)>]
        member val Offset = Defined(5.0f) with get, set

        [<FloatRange(0.0f, 1.0f)>]
        member val Size = Defined(0.5f) with get, set

        [<FloatRange(0.005f, 1.0f, Step=0.05f)>]
        member val Softness = Defined(1.0f) with get, set

        [<FloatRange(0.1f, 1.0f, Step=0.05f)>]
        member val Strength = Defined(1.0f) with get, set

        [<Bool(Name="Is inner")>]
        member val Inner = Defined(false) with get, set
        
        override this.Build(platform, random) = 
            let getValue expr = Randomizeable.getValue random expr
            let shadowDir = Point.One.Rotate(deg2rad <| getValue <@ this.Dir @>)
            ShadowEffect(
                platform,
                shadowDir,
                getValue <@ this.Offset @>,
                getValue <@ this.Size @>,
                getValue <@ this.Softness @>,
                getValue <@ this.Strength @>,
                getValue <@ this.Inner @>) 
            :> Effect
     
    let allTypes = [| typeof<Plain>; typeof<Outline>; typeof<Shadow>; |]


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

