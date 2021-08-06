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
    
    
    type PaperType =
        | Smooth = 1
        | Watercolor = 2

    type Paper() =
        inherit MaterialSettings()
        static do Settings.RegisterClass<Paper>()

        [<Enum([| PaperType.Smooth; PaperType.Watercolor |], ItemType=typeof<PaperType>)>]
        member val Type = Defined(PaperType.Smooth) with get, set

        override this.Build(platform, random, color) =
            match Randomizeable.getValue random <@ this.Type @> with
            | PaperType.Smooth ->
                UniversalMaterial(platform, color, 
                                  Some("paper/Paper_Texture_1_ambientOcclusion.png"), 
                                  Some("paper/Paper_Texture_1_normal.png"),
                                  (None, 0.92f),
                                  1.0f, 1.0f) :> Material
            | PaperType.Watercolor ->
                UniversalMaterial(platform, color, 
                                  Some("paper/Watercolor_Paper_001_OCC.jpg"), 
                                  Some("paper/Watercolor_Paper_001_NORM.jpg"),
                                  (None, 0.92f),
                                  1.5f, 1.5f) :> Material
    
    
    type Fabric() = 
        inherit MaterialSettings()
        static do Settings.RegisterClass<Fabric>()

        override this.Build(platform, random, color) =
            UniversalMaterial(platform, color, None, Some("fabric/Fabric_01_normal.png"), (None, 0.92f), 2.0f, 1.5f) :> Material
    

    type Leather() =
        inherit MaterialSettings()
        static do Settings.RegisterClass<Leather>()

        override this.Build(platform, random, color) =
            UniversalMaterial(platform, color, 
                              None, 
                              Some("leather/TexturesCom_Leather_Plain11_008x008_512_normal.png"), 
                              (Some("leather/Leather_roughness.png"), 0.92f), 
                              0.5f, 2.0f) :> Material

    
    type StuccoType =
        | Rough = 1
        | Combed = 2

    type Stucco() =
        inherit MaterialSettings()
        static do Settings.RegisterClass<Stucco>()

        [<Enum([| StuccoType.Combed; StuccoType.Rough |], ItemType=typeof<StuccoType>)>]
        member val Type = Defined(StuccoType.Combed) with get, set

        override this.Build(platform, random, color) =
            match Randomizeable.getValue random <@ this.Type @> with
            | StuccoType.Rough -> 
                UniversalMaterial(platform, color, 
                                  Some("stucco/Stucco_1_ao.png"), 
                                  Some("stucco/Stucco_1_normal.png"), 
                                  (Some("stucco/Stucco_1_roughness.png"), 0.86f), 
                                  1.5f, 1.0f) :> Material
            | StuccoType.Combed -> 
                UniversalMaterial(platform, color, 
                                  None, 
                                  Some("stucco/TexturesCom_Plaster_StuccoCombed_1K_normal.png"), 
                                  (Some("stucco/Stucco_3_roughness.png"), 0.89f), 
                                  1.5f, 1.0f) :> Material

    type MetalType =
        | Smooth = 0
        | Brushed = 1
        | Bumpy = 2

    type Metal() =
        inherit MaterialSettings()
        static do Settings.RegisterClass<Metal>()

        [<Enum([| MetalType.Smooth; MetalType.Brushed; MetalType.Bumpy |], ItemType=typeof<MetalType>)>]
        member val Type = Defined(MetalType.Smooth) with get, set

        override this.Build(platform, random, color) =
            let h = 1.0f // lightHeight
            let m = 0.85f // metalness
            match Randomizeable.getValue random <@ this.Type @> with
            | MetalType.Smooth ->
                UniversalMaterial(platform,  color, None, None, (None, 0.5f), h, 1.5f, m) 
                    :> Material
            | MetalType.Brushed ->
                UniversalMaterial(platform, color, None, 
                                  Some("metal/Metal_normal_brushed.png"), 
                                  (Some("metal/Metal_roughness_brushed.png"), 0.4f), 
                                  h, 
                                  4.0f, 
                                  m) 
                    :> Material
            | MetalType.Bumpy ->
                UniversalMaterial(platform, color, None, Some("metal/Metal_normal_bumpy.png"), (None, 0.6f), h, 2.0f, m) 
                    :> Material
    
    
    type Wood() =
        inherit MaterialSettings()
        static do Settings.RegisterClass<Wood>()
        
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
    
    
    let allTypes = [| typeof<SolidColor>; typeof<Paper>; typeof<Leather>; typeof<Fabric>; typeof<Stucco>; typeof<Wood>; typeof<Metal>; |]


module Effects =
    type Plain() =
        inherit EffectSettings()
        static do Settings.RegisterClass<Plain>()
        override this.Build(platform, random) = PlainEffect(platform) :> Effect

    type Outline() =
        inherit EffectSettings()
        static do Settings.RegisterClass<Outline>()

        [<FloatRange(0.02f, 0.1f)>]
        member val OutlineWidth = Defined(0.05f) with get, set

        override this.Build(platform, random) = 
            OutlineEffect(platform, Randomizeable.getValue random <@ this.OutlineWidth @>) :> Effect
    
    type Shadow() =
        inherit EffectSettings()
        static do Settings.RegisterClass<Shadow>()

        [<FloatRange(0.0f, 10.0f, Step=1.0f)>]
        member val Offset = Defined(5.0f) with get, set

        [<FloatRange(0.1f, 1.0f)>]
        member val Size = Defined(0.5f) with get, set

        [<FloatRange(0.005f, 1.0f, Step=0.05f)>]
        member val Softness = Defined(1.0f) with get, set

        [<FloatRange(0.1f, 1.0f, Step=0.05f)>]
        member val Strength = Defined(1.0f) with get, set

        [<Bool(Name="Is inner")>]
        member val Inner = Defined(false) with get, set
        
        override this.Build(platform, random) = 
            // https://iquilezles.org/www/articles/functions/functions.htm
            let almostIdentity x m n =
                if x > m then 
                    x
                else
                    let a = 2.0f * n - m
                    let b = 2.0f * m - 3.0f * n
                    let t = x / m
                    (a * t + b) * t*t + n
            let getValue expr = Randomizeable.getValue random expr
            let shadowDir = Point(0.5f - Helpers.lightPosition.X, Helpers.lightPosition.Y - 0.5f)
            let softness = getValue <@ this.Softness @>
            let size = almostIdentity (getValue <@ this.Size @>) softness 0.2f
            ShadowEffect(
                platform,
                shadowDir,
                getValue <@ this.Offset @>,
                size,
                softness,
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

        [<Enum([| BevelProfile.Linear; BevelProfile.Circular; BevelProfile.InverseCircular; BevelProfile.Sigmoid |], 
               ItemType=typeof<BevelProfile>, 
               Name="Bevel profile")>]
        member val Profile = Defined(BevelProfile.Linear) with get, set

        [<Enum([| BevelMode.DownDown; BevelMode.DownLevel; BevelMode.DownUp; BevelMode.LevelDown; BevelMode.LevelUp; 
                  BevelMode.UpDown; BevelMode.UpLevel; BevelMode.UpUp |], 
                  ItemType=typeof<BevelMode>)>]
        member val Mode = Defined(BevelMode.UpLevel) with get, set

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

        [<FloatRange(0.1f, 1.0f, Step=0.05f)>]
        member val Size = Defined(1.0f) with get, set

        [<FloatRange(0.1f, 0.3f, Step=0.025f)>]
        member val Intensity = Defined(0.2f) with get, set

        [<Enum([| GlowInsideType.Empty; GlowInsideType.Glow; GlowInsideType.Foreground |], ItemType=typeof<GlowInsideType>)>]
        member val Inside = Defined(GlowInsideType.Empty) with get, set

        override this.Build(platform, random) =
            let getValue expr = Randomizeable.getValue random expr
            let bloomPower = 1.0f / (getValue <@ this.Intensity @>)
            GlowEffect(
                platform,
                getValue <@ this.Size @>,
                bloomPower,
                getValue <@ this.Inside @>)
            :> Effect
    
    type Highlight() =
        inherit EffectSettings()
        static do Settings.RegisterClass<Highlight>()

        [<FloatRange(0.05f, 1.0f, Step=0.05f)>]
        member val LightIntensity = Defined(0.5f) with get, set

        override this.Build(platform, random) =
            HighlightEffect(
                platform, 
                Geometry.Point(0.0f, 0.75f), 
                Randomizeable.getValue random <@ this.LightIntensity @>)
            :> Effect
    
    type FuzzyTextureType =
        | Ink = 0
        | Chalk = 1

    [<Name("Ink/chalk")>]
    type FuzzyTexture() = 
        inherit EffectSettings()
        static do Settings.RegisterClass<FuzzyTexture>()

        [<Enum([| FuzzyTextureType.Ink; FuzzyTextureType.Chalk |], ItemType=typeof<FuzzyTextureType>, Name="Type")>]
        member val TextureType = Defined(FuzzyTextureType.Ink) with get, set
        
        [<FloatRange(0.05f, 0.25f, Step=0.025f, Name="Strength")>]
        member val Distortion = Defined(0.15f) with get, set

        [<FloatRange(0.05f, 0.25f, Step=0.025f)>]
        member val Softness = Defined(0.10f) with get, set
        
        [<FloatRange(0.25f, 0.75f, Step=0.05f)>]
        member val Fill = Defined(0.60f) with get, set

        [<FloatRange(0.0f, 0.20f, Step=0.025f)>]
        member val Washout = Defined(0.0f) with get, set

        override this.Build(platform, random) =
            let getValue expr = Randomizeable.getValue random expr
            let textureName, textureScale =
                match getValue <@ this.TextureType @> with
                | FuzzyTextureType.Ink   -> "fuzzy_texture/FuzzyTexture__fbm.png",   2.0f
                | FuzzyTextureType.Chalk -> "fuzzy_texture/FuzzyTexture__grain.png", 4.0f
            FuzzyTextureEffect(
                platform, 
                textureName,
                textureScale,
                getValue <@ this.Distortion @>,
                getValue <@ this.Softness @>,
                getValue <@ this.Fill @>,
                getValue <@ this.Washout @>)
            :> Effect

    let allTypes = [| typeof<Plain>; typeof<Outline>; typeof<Shadow>; typeof<Bevel>; typeof<Glow>; typeof<Highlight>; typeof<FuzzyTexture> |]


/// Get colors given color settings and RNG. Return (foregroundColor, backgroundColor)
let getColors (random : System.Random) fgColorSetting bgColorSetting : OpenTK.Graphics.Color4 * OpenTK.Graphics.Color4 =
    //TODO: generate random colors using some kind of strategy and taking into account defined color, if any
    let getColor(value : Randomizeable<float32*float32*float32>) = 
        match value with
        | Defined(x) -> x
        | Randomized -> ColorAttribute().RandomValue(random) |> unbox<float32*float32*float32>
        |> OpenTK.Graphics.Color4.FromRGBTriple
    getColor fgColorSetting, getColor bgColorSetting


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
    member this.GetColors(random) =
        getColors random this.ForegroundColor this.BackgroundColor


/// Module that holds current application settings.
/// GUI and renderer communicate through this module, and it's also used to save settings between launches.
module CurrentSettings =
#if __ANDROID__
    open System.Json
    open Android.Content
#endif

    let mutable private currSettings : SettingsHolder option = None

    let get() : SettingsHolder = currSettings.Value
    let set(value : SettingsHolder) = currSettings <- Some(value)
    let isSet() = currSettings.IsSome

    let mutable speedFactor = 1.0f
    
#if __ANDROID__
    // save/load
    let private saveHooks = ResizeArray<Context -> JsonObject -> unit>()
    let private loadHooks = ResizeArray<Context -> JsonObject -> unit>()

    let save(context : Context) : unit =
        let filePath = context.FilesDir.Path + "/settings"
        match currSettings with
        | Some(settings) ->
            let json = JsonObject()
            json.["settingsHolder"] <- Serialization.toJsonValue(settings)
            json.["speedFactor"] <- Serialization.(!>) speedFactor
            for action in saveHooks do action context json
            use ostream = new System.IO.FileStream(filePath, System.IO.FileMode.Create)
            json.Save(ostream)
        | None -> ()

    let load(context : Context) : unit =
        let filePath = context.FilesDir.Path + "/settings"
        use file = new Java.IO.File(filePath)
        if file.Exists() then
            try
                use stream = new System.IO.FileStream(filePath, System.IO.FileMode.Open)
                let json = JsonValue.Load(stream) :?> JsonObject
                let settings : SettingsHolder = Serialization.fromJsonValue(json.["settingsHolder"])
                for action in loadHooks do action context json
                currSettings <- Some(settings)
                speedFactor <- Serialization.(!>) json.["speedFactor"]
            with | e ->
#if DEBUG
                raise e
#else
                ()
#endif
    // Save/load hooks. Argument is a function that takes context and current JSON object and preforms some actions or adds new properties to the object.
    let addSaveHook(action : Context -> JsonObject -> unit) : unit =
        saveHooks.Add action

    let addLoadHook(action : Context -> JsonObject -> unit) : unit =
        loadHooks.Add action
#endif

