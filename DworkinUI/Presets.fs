namespace DworkinUI


open LibDworkin.DworkinSettings
open Settings
open Settings.Presets


type AppearancePreset(effect, fgColor, bgColor, fgMaterial, bgMaterial) = 
    inherit Preset()

    static do // register serializer/deserializer
        let toJson (preset : AppearancePreset) =
            let d = dict [| 
                "effect", Serialization.toJsonValue preset.Effect
                "fgColor", Serialization.toJsonValue preset.ForegroundColor
                "bgColor", Serialization.toJsonValue preset.BackgroundColor
                "fgMaterial", Serialization.toJsonValue preset.Foreground
                "bgMaterial", Serialization.toJsonValue preset.Background 
            |]
            System.Json.JsonObject(d) :> System.Json.JsonValue
        let fromJson (json : System.Json.JsonValue) =
            let jsonDict = json :?> System.Json.JsonObject
            AppearancePreset(
                Serialization.fromJsonValue jsonDict.["effect"],
                Serialization.fromJsonValue jsonDict.["fgColor"],
                Serialization.fromJsonValue jsonDict.["bgColor"],
                Serialization.fromJsonValue jsonDict.["fgMaterial"],
                Serialization.fromJsonValue jsonDict.["bgMaterial"]
            )
        Serialization.Converters.getOrSetIfAbsent
            typeof<AppearancePreset>
            (fun () -> Serialization.Converter(toJson, fromJson) :> Serialization.IConverter)
            |> ignore

    member val Effect : EffectSettings = effect with get, set
    member val ForegroundColor : Randomizeable<float32*float32*float32> = fgColor with get, set
    member val BackgroundColor : Randomizeable<float32*float32*float32> = bgColor with get, set
    member val Foreground : MaterialSettings = fgMaterial with get, set
    member val Background : MaterialSettings = bgMaterial with get, set
    
    /// Return size in pixels of preview based on screen size
    static member GetPreviewSize(context : Android.Content.Context) : (int * int) =
        let displayMetrics = context.Resources.DisplayMetrics
        let smallerSide = min displayMetrics.WidthPixels displayMetrics.HeightPixels
        let spacing = (context.Resources.GetDimension(Resources.Dimension.preset_preview_spacing)) / displayMetrics.Density // in dp
        let targetSize = displayMetrics.Density * (150.0f + float32(spacing * 2.0f))
        let n : int = int <| round(float32(smallerSide) / targetSize)
        let n = max n 3
        let padding = context.Resources.GetDimension(Resources.Dimension.preset_preview_padding) // in pixels
        let numPixels = (smallerSide - int(spacing * displayMetrics.Density)*(n+1) - int(padding * 2.0f)) / n
        let numPixels = numPixels - (numPixels%2) // make it even
        (numPixels, numPixels)

    static member DirectoryName = "appearance"
    override this.DirName = AppearancePreset.DirectoryName

    override this.GeneratePreview(context, w, h) =
        let platform = LibDworkin.RendererComponents.AndroidPlatform(context)
        let renderer = PreviewRenderers.AppearancePresetRenderer.RequestInstance(platform, w, h)

        let getRandom() = System.Random(42)
        let fgColor, bgColor = getColors (getRandom()) fgColor bgColor

        renderer.RenderPreview(
            this.Background.Build(platform, getRandom(), bgColor), 
            this.Foreground.Build(platform, getRandom(), fgColor), 
            this.Effect.Build(platform, getRandom()))

    /// Return true if provided settings have same effect, materials and colors as receiver
    member this.IsUsedInSettings(settings : SettingsHolder) : bool =
        this.Effect = settings.Effect && 
        this.BackgroundColor = settings.BackgroundColor &&
        this.ForegroundColor = settings.ForegroundColor &&
        this.Background = settings.Background &&
        this.Foreground = settings.Foreground


/// This module contains references to built-in presets.
/// Now they are hardcoded, but in future they will be lazily initialized from assets or something similar.
module BuiltinPresets =
    open LibDworkin.DworkinSettings
    open DworkinConfigs
    
    let private appearancePresets : AppearancePreset[] = 
        [|
            AppearancePreset(
                Effects.Plain(), Defined(0.0f, 0.0f, 0.0f), Defined(0.5f, 0.5f, 1.0f), Materials.SolidColor(), Materials.SolidColor(), 
                Name="Basic", IsBuiltin=true, IsSaved=true);
            AppearancePreset(
                Effects.Shadow(), Defined(0.7f, 0.5f, 0.4f), Defined(0.3f, 0.2f, 0.2f), Materials.Wood(Painted=Defined(true)), Materials.Wood(Painted=Defined(true)), 
                Name="Painted wood", IsBuiltin=true, IsSaved=true);
        |]

    let getAppearancePresets(context : Android.Content.Context) : AppearancePreset[] =
        appearancePresets


module CurrentPresetSelection =
    let mutable appearance : AppearancePreset option = None
    let getAppearancePresetDisplayText() = 
        match appearance with
        | Some(preset) when preset.IsUsedInSettings(CurrentSettings.get()) -> 
            preset.Name
        | _ -> "custom"
