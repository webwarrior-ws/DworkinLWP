namespace Settings

type Resources = Settings.Resource
type ColorPickerResources = Top.Defaults.ColorPickerLib.Resource


type RGBTriple = float32 * float32 * float32


module AdditionalResourceValues =
    let resourceTypesToBeUpdated = [ typeof<ColorPickerResources> ]
