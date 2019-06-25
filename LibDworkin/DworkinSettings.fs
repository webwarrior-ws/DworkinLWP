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
    [<AbstractClass>]
    type ColoredMaterial() =
        inherit MaterialSettings()
        [<Color>]
        member val Color = Defined(OpenTK.Graphics.Color4.White.AsRGBTriple) with get, set

    [<Name("Solid color")>]
    type SolidColor() =
        inherit ColoredMaterial()

        override this.Build(platform, random) =
            SolidColorMaterial(
                platform, 
                OpenTK.Graphics.Color4.FromRGBTriple(Randomizeable.getValue random <@ this.Color @>)) 
            :> Material
    
    [<Name("Rough surface")>]
    type RoughSurface() =
        inherit ColoredMaterial()

        [<FloatRange(0.0f, 0.25f, Step=0.05f, Name="Bump height")>]
        member val BumpHeight = Defined(0.1f) with get, set

        [<FloatRange(0.1f, 1.0f, Step=0.05f)>]
        member val Dim = Defined(0.1f) with get, set

        [<FloatRange(10.0f, 100.0f, Step=1.0f, Name="Feature scale")>]
        member val FeatureScale = Defined(50.0f) with get, set

        [<FloatRange(0.5f, 2.5f, Step=0.1f, Name="Flat area")>]
        member val FlatArea = Defined(0.5f) with get, set

        override this.Build(platform, random) =
            let getValue expr = Randomizeable.getValue random expr
            RoughSurfaceMaterial(
                platform, 
                getValue <@ this.Color @> |> OpenTK.Graphics.Color4.FromRGBTriple,
                getValue <@ this.BumpHeight @>,
                getValue <@ this.Dim @>,
                getValue <@ this.FeatureScale @>,
                getValue <@ this.FlatArea @>) 
            :> Material
    
    type Wood() =
        inherit ColoredMaterial()

        [<FloatRange(0.0f, 180.0f, Step=5.0f)>]
        member val Angle = Defined(0.0f) with get, set

        [<FloatRange(2.0f, 5.0f, Step=0.1f, Name="Rings scale")>]
        member val RingsScale = Defined(2.7f) with get, set

        [<FloatRange(0.0f, 50.0f, Step=5.0f, Name="Rings stretch")>]
        member val RingsStretch = Defined(15.0f) with get, set

        [<FloatRange(0.0f, 0.5f, Step=0.05f)>]
        member val Contrast = Defined(0.25f) with get, set

        override this.Build(platform, random) =
            let getValue expr = Randomizeable.getValue random expr
            WoodMaterial(
                platform, 
                getValue <@ this.Color @> |> OpenTK.Graphics.Color4.FromRGBTriple,
                getValue <@ this.Angle @>,
                getValue <@ this.RingsScale @>,
                getValue <@ this.RingsStretch @>,
                getValue <@ this.Contrast @>) 
            :> Material
    
    type Leather() =
        inherit ColoredMaterial()

        [<FloatRange(0.0f, 1.0f)>]
        member val Contrast = Defined(0.5f) with get, set

        [<FloatRange(1.0f, 3.0f)>]
        member val Scale = Defined(2.0f) with get, set

        override this.Build(platform, random) =
            let getValue expr = Randomizeable.getValue random expr
            let lightHeight = 3.0f - 2.0f * (getValue <@ this.Contrast @>)
            let scaleFactor = 3.0f / (getValue <@ this.Scale @>)
            NormalmapAndAOMaterial(
                platform, 
                getValue <@ this.Color @> |> OpenTK.Graphics.Color4.FromRGBTriple,
                "TexturesCom_Leather_Plain_512_normal.png",
                Point(1.0f, 1.0f),
                lightHeight,
                scaleFactor,
                None) 
            :> Material
    
    let allTypes = [| typeof<SolidColor>; typeof<RoughSurface>; typeof<Wood>; typeof<Leather>; |]


module Effects =
    type Plain() =
        inherit EffectSettings()
        override this.Build(platform, random) = PlainEffect(platform) :> Effect

    type Outline() =
        inherit EffectSettings()

        [<FloatRange(0.01f, 0.5f)>]
        member val OutlineWidth = Defined(0.1f) with get, set

        override this.Build(platform, random) = 
            OutlineEffect(platform, Randomizeable.getValue random <@ this.OutlineWidth @>) :> Effect
    
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
    
    type Bevel() =
        inherit EffectSettings()

        [<FloatRange(1.0f, 10.0f, Step=1.0f)>]
        member val Contrast = Defined(5.0f) with get, set

        [<FloatRange(0.0f, 1.0f, Name="Max. distance")>]
        member val MaxDistance = Defined(1.0f) with get, set

        [<Enum([| BevelProfile.Linear; BevelProfile.Hyperbolic; BevelProfile.Parabola1; BevelProfile.Parabola2; BevelProfile.Exp |], 
               ItemType=typeof<BevelProfile>, 
               Name="Bevel profile")>]
        member val Profile = Defined(BevelProfile.Linear) with get, set

        [<Enum([| BevelMode.Inner; BevelMode.Outer; BevelMode.Ridge; BevelMode.Trench |], ItemType=typeof<BevelMode>)>]
        member val Mode = Defined(BevelMode.Outer) with get, set

        override this.Build(platform, random) =
            let getValue expr = Randomizeable.getValue random expr
            BevelEffect(
                platform,
                getValue <@ this.Contrast @>,
                getValue <@ this.MaxDistance @>,
                Point(1.0f, 1.0f),
                getValue <@ this.Profile @>,
                getValue <@ this.Mode @>)
            :> Effect
    
    type Glow() =
        inherit EffectSettings()

        [<Color>]
        member val Color = Defined(OpenTK.Graphics.Color4.Aquamarine.AsRGBTriple) with get, set

        [<FloatRange(0.05f, 1.0f, Step=0.05f)>]
        member val Strength = Defined(1.0f) with get, set

        [<Enum([| GlowType.Outer; GlowType.Inner |], ItemType=typeof<GlowType>)>]
        member val Type = Defined(GlowType.Outer) with get, set

        override this.Build(platform, random) =
            let getValue expr = Randomizeable.getValue random expr
            GlowEffect(
                platform,
                getValue <@ this.Color @> |> OpenTK.Graphics.Color4.FromRGBTriple,
                getValue <@ this.Strength @>,
                getValue <@ this.Type @>)
            :> Effect
    
    type Highlight() =
        inherit EffectSettings()

        [<FloatRange(0.05f, 1.0f, Step=0.05f)>]
        member val LightIntensity = Defined(0.5f) with get, set

        [<FloatRange(0.0f, 1.0f)>]
        member val LightPosition = Defined(0.75f) with get, set

        override this.Build(platform, random) =
            HighlightEffect(
                platform, 
                Geometry.Point(0.0f, Randomizeable.getValue random <@ this.LightPosition @>), 
                Randomizeable.getValue random <@ this.LightIntensity @>)
            :> Effect

    let allTypes = [| typeof<Plain>; typeof<Outline>; typeof<Shadow>; typeof<Bevel>; typeof<Glow>; typeof<Highlight>; |]


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

