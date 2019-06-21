namespace Settings

type Resources = Settings.Resource
type ColorPickerResources = Top.Defaults.ColorPickerLib.Resource


type RGBTriple = float32 * float32 * float32


module AdditionalResourceValues =
    let resourceTypesToBeUpdated = [ typeof<ColorPickerResources> ]


module XamarinCrutch =
    /// Transfer values of resource ids from app resources class to library resources class.
    let rec updateLibraryResourceIds (appResources : System.Type) (libraryResources : System.Type) =
        let ids = 
            libraryResources.GetFields()
            |> Array.filter (fun fi -> fi.IsStatic && fi.IsPublic)
        for field in ids do
            let appResField = appResources.GetField(field.Name)
            if isNull appResField |> not then 
                let correctValue = appResField.GetValue(null)
                field.SetValue(null, correctValue)
        
        let appResourcesInnerClassNames = 
            appResources.GetNestedTypes() 
            |> Array.map (fun cls -> cls.Name) 
            |> Set.ofArray
        let innerClasses = 
            libraryResources.GetNestedTypes() 
            |> Array.filter (fun cls -> appResourcesInnerClassNames.Contains cls.Name)
        
        for cls in innerClasses do
            updateLibraryResourceIds (appResources.GetNestedType(cls.Name)) cls
