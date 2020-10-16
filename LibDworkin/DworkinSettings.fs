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
    abstract member Build : AbstractPlatform * System.Random * OpenTK.Graphics.Color4 -> Material

[<AbstractClass>] 
type EffectSettings() =
    inherit Settings()
    abstract member Build : AbstractPlatform * System.Random -> Effect


module Materials =
    [<Name("Solid color")>]
    type SolidColor() =
        inherit MaterialSettings()
        static do Settings.RegisterClass<SolidColor>()

        override this.Build(platform, random, color) =
            SolidColorMaterial(platform, color) :> Material
    
    type Paper() =
        inherit MaterialSettings()
        static do Settings.RegisterClass<Paper>()

        override this.Build(platform, random, color) =
            UniversalMaterial(
                platform, 
                color, 
                Some("paper/Watercolor_Paper_001_OCC.jpg"), 
                Some("paper/Watercolor_Paper_001_NORM.jpg"),
                (None, 0.92f),
                1.5f,
                1.5f) :> Material
    
    type Metal() =
        inherit MaterialSettings()
        static do Settings.RegisterClass<Metal>()

        override this.Build(platform, random, color) =
            // smooth
            UniversalMaterial(
                platform, 
                color, 
                None, 
                None,
                (None, 0.5f),
                1.0f,
                1.5f,
                0.85f) :> Material
    
    type Wood() =
        inherit MaterialSettings()
        static do Settings.RegisterClass<Wood>()
        
        //[<FloatRange(0.0f, 0.5f, Step=0.05f)>]
        //member val Contrast = Defined(0.25f) with get, set

        [<Bool>]
        member val Painted = Defined(false) with get, set

        override this.Build(platform, random, color) =
            let getValue expr = Randomizeable.getValue random expr
            if getValue <@ this.Painted @> then
                UniversalMaterial(
                    platform, 
                    color,
                    Some("wood/TexturesCom_Wood_Worn2_1K_ao.png"),
                    Some("wood/TexturesCom_Wood_Worn2_1K_normal.png"),
                    (None, 0.92f),
                    0.35f,
                    1.0f) 
                :> Material
            else
                TwoColorMaterial(
                    platform, 
                    color,
                    "wood/TexturesCom_Wood_Worn2_1K_colordiff.png",
                    Some("wood/TexturesCom_Wood_Worn2_1K_ao.png"),
                    Some("wood/TexturesCom_Wood_Worn2_1K_normal.png"),
                    (Some("wood/Wood_roughness.png"), 0.96f),
                    0.5f,
                    1.0f) 
                :> Material
    
    let allTypes = [| typeof<SolidColor>; typeof<Paper>; typeof<Wood>; typeof<Metal>; |]


module Effects =
    type Plain() =
        inherit EffectSettings()
        static do Settings.RegisterClass<Plain>()
        override this.Build(platform, random) = PlainEffect(platform) :> Effect

    type Outline() =
        inherit EffectSettings()
        static do Settings.RegisterClass<Outline>()

        [<FloatRange(0.01f, 0.5f)>]
        member val OutlineWidth = Defined(0.1f) with get, set

        override this.Build(platform, random) = 
            OutlineEffect(platform, Randomizeable.getValue random <@ this.OutlineWidth @>) :> Effect
    
    type Shadow() =
        inherit EffectSettings()
        static do Settings.RegisterClass<Shadow>()

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
        static do Settings.RegisterClass<Bevel>()

        [<FloatRange(1.0f, 3.0f, Step=0.25f)>]
        member val Contrast = Defined(1.5f) with get, set

        [<FloatRange(0.0f, 0.5f, Name="Max. distance")>]
        member val MaxDistance = Defined(0.5f) with get, set

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
                getValue <@ this.Profile @>,
                getValue <@ this.Mode @>)
            :> Effect
    
    type Glow() =
        inherit EffectSettings()
        static do Settings.RegisterClass<Glow>()

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
        static do Settings.RegisterClass<Highlight>()

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
        ForegroundColor : Randomizeable<float32*float32*float32>
        BackgroundColor : Randomizeable<float32*float32*float32>
        Foreground : MaterialSettings
        Background : MaterialSettings
        Seed : int
    }
    member this.GetColors(random : System.Random) : OpenTK.Graphics.Color4 * OpenTK.Graphics.Color4 =
        //TODO: generate random colors using some kind of strategy and taking into account defined color, if any
        let getColor(value : Randomizeable<float32*float32*float32>) = 
            match value with
            | Defined(x) -> x
            | Randomized -> ColorAttribute().RandomValue(random) |> unbox<float32*float32*float32>
            |> OpenTK.Graphics.Color4.FromRGBTriple
        getColor this.ForegroundColor, getColor this.BackgroundColor


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

