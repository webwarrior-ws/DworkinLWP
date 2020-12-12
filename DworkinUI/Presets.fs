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
    
    static member DirectoryName = "appearance"
    override this.DirName = AppearancePreset.DirectoryName

    // temporary stub
    override this.GeneratePreview(h : int, w : int) =
        let bmp = Android.Graphics.Bitmap.CreateBitmap(h, w, Android.Graphics.Bitmap.Config.Argb8888)
        let c = 
            match this.BackgroundColor with
            | Defined(r, g, b) -> Android.Graphics.Color.Rgb(int(r * 255.0f), int(g * 255.0f), int(b * 255.0f))
            | Randomized -> Android.Graphics.Color.Transparent
        bmp.EraseColor(c.ToArgb())
        bmp


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
