namespace DworkinUI


open LibDworkin.DworkinSettings
open Settings
open Settings.Presets

type Resources = DworkinUI.Resource


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

    override this.HasRandomizedValues =
        effect.HasRandomizedValues || fgMaterial.HasRandomizedValues || bgMaterial.HasRandomizedValues
         || fgColor.IsRandomized || bgColor.IsRandomized

    override this.GeneratePreview(context, w, h) =
        let platform = LibDworkin.RendererComponents.AndroidPlatform(context)
        let renderer = PreviewRenderers.AppearancePresetRenderer.RequestInstance(platform, w, h)

        let getRandom() = System.Random(69)
        let fgColor, bgColor = Colors.getColors (getRandom()) fgColor bgColor
        let background = this.Background.Build(platform, getRandom(), bgColor)
        let foreground =
            match this.Foreground with
            | :? Materials.SameAsBackground -> this.Background.Build(platform, getRandom(), fgColor)
            | foreground -> foreground.Build(platform, getRandom(), fgColor)
        let effect = this.Effect.Build(platform, getRandom())

        renderer.RenderPreview(bgColor, fgColor, background, foreground, effect)

    /// Return true if provided settings have same effect, materials and colors as receiver
    member this.IsUsedInSettings(settings : SettingsHolder) : bool =
        this.Effect = settings.Effect && 
        this.BackgroundColor = settings.BackgroundColor &&
        this.ForegroundColor = settings.ForegroundColor &&
        this.Background = settings.Background &&
        this.Foreground = settings.Foreground

    override this.UsedClasses =
        [| effect.GetType(); fgMaterial.GetType(); bgMaterial.GetType() |]


type OverallPreset(settings : SettingsHolder) =
    inherit Preset()

    static do // register serializer/deserializer
        let toJson (preset : OverallPreset) = Serialization.toJsonValue preset.Settings
        let fromJson (json : System.Json.JsonValue) = OverallPreset(Serialization.fromJsonValue json)
        Serialization.Converters.getOrSetIfAbsent
            typeof<OverallPreset>
            (fun () -> Serialization.Converter(toJson, fromJson) :> Serialization.IConverter)
            |> ignore

    static member DirectoryName = "overall"
    override this.DirName = OverallPreset.DirectoryName

    override this.HasRandomizedValues =
        settings.Background.HasRandomizedValues || settings.Foreground.HasRandomizedValues 
         || settings.Effect.HasRandomizedValues || settings.Environment.HasRandomizedValues
         || settings.BackgroundColor.IsRandomized || settings.ForegroundColor.IsRandomized

    member this.Settings = settings

    /// Return size in pixels of preview based on screen size
    static member GetPreviewSize(context : Android.Content.Context) : (int * int) =
        let displayMetrics = context.Resources.DisplayMetrics
        // https://stackoverflow.com/questions/9279111/determine-if-the-device-is-a-smartphone-or-tablet
        let isTablet = context.Resources.GetBoolean(Resources.Boolean.isTablet)
        let smallerSide = min displayMetrics.WidthPixels displayMetrics.HeightPixels
        let largerSide = max displayMetrics.WidthPixels displayMetrics.HeightPixels 
        let width = if isTablet then largerSide else smallerSide
        let height = if isTablet then smallerSide else largerSide

        let spacing = (context.Resources.GetDimension(Resources.Dimension.preset_preview_spacing)) / displayMetrics.Density // in dp
        let padding = context.Resources.GetDimension(Resources.Dimension.preset_preview_padding) // in pixels
        let n = 2
        
        let numPixelsX = (width - int(spacing * displayMetrics.Density)*(n+1) - int(padding * 2.0f)) / n
        let numPixelsX = numPixelsX - (numPixelsX%2) // make it even
        let numPixelsY = numPixelsX * height / width
        let numPixelsY = numPixelsY - (numPixelsY%2) // make it even
        
        (numPixelsX, numPixelsY)

    override this.GeneratePreview(context, w, h) =
        let platform = LibDworkin.RendererComponents.AndroidPlatform(context)
        let renderer = PreviewRenderers.OverallPresetRenderer.RequestInstance(platform, w, h)
        renderer.RenderPreview(settings)

    override this.UsedClasses =
        [| settings.Effect.GetType(); settings.Foreground.GetType(); settings.Background.GetType(); settings.Environment.GetType() |]


/// This module contains references to built-in presets.
module BuiltinPresets =
    open LibDworkin.DworkinSettings
    open DworkinConfigs
    open Settings.Presets.PresetManagement
    
    let mutable private appearancePresets : AppearancePreset[] = null
    
    let mutable private overallPresets : OverallPreset[] = null

    let getAppearancePresets(context : Android.Content.Context) : AppearancePreset[] =
        if isNull appearancePresets then
            appearancePresets <- loadPresetsFromAssets<AppearancePreset> context AppearancePreset.DirectoryName
        appearancePresets

    let getOverallPresets(context : Android.Content.Context) : OverallPreset[] =
        if isNull overallPresets then
            overallPresets <- loadPresetsFromAssets<OverallPreset> context OverallPreset.DirectoryName
        overallPresets


module CurrentPresetSelection =
    open System.Json

    let mutable appearance : AppearancePreset option = None
    let getAppearancePresetDisplayText() = 
        match appearance with
        | Some(preset) when preset.IsUsedInSettings(CurrentSettings.get()) -> 
            preset.Name
        | _ -> "custom"

    let mutable overall : OverallPreset option = None
    let getOverallPresetDisplayText() = 
        match overall with
        | Some(preset) when preset.Settings = CurrentSettings.get() -> 
            preset.Name
        | _ -> "custom"

    /// Install settings save/load hooks. Must be called before loading settings (CurrentSettings.load)
    let installHooks() =
        CurrentSettings.addSaveHook <| fun _ json ->
            match overall with
            | Some(preset) -> json.["overallPresetName"] <- JsonPrimitive(preset.Name)
            | None -> ()
            match appearance with
            | Some(preset) -> json.["appearancePresetName"] <- JsonPrimitive(preset.Name)
            | None -> ()

        CurrentSettings.addLoadHook <| fun context json ->
            match json.TryGetValue("overallPresetName") with
            | true, value -> 
                let name : string = Serialization.(!>) value
                let file = new Java.IO.File(PresetManagement.getFilePath context OverallPreset.DirectoryName name)
                if file.Exists() then
                    overall <- Some(PresetManagement.loadPreset context file.AbsolutePath)
            | false, _ -> ()
            match json.TryGetValue("appearancePresetName") with
            | true, value -> 
                let name : string = Serialization.(!>) value
                let file = new Java.IO.File(PresetManagement.getFilePath context AppearancePreset.DirectoryName name)
                if file.Exists() then
                    appearance <- Some(PresetManagement.loadPreset context file.AbsolutePath)
            | false, _ -> ()