namespace UITestingApp

open Settings

type Resources = UITestingApp.Resource


type Endpoint = 
    | Hollow = 0
    | Filled = 1


type AllKindsOfSettings() =
    inherit Settings()
    static do 
        // TODO: see if this is no longer needed
        XamarinCrutch.updateLibraryResourceIds typeof<Resources> typeof<Settings.Resources>
        for each in Settings.AdditionalResourceValues.resourceTypesToBeUpdated do
            XamarinCrutch.updateLibraryResourceIds typeof<Resources> each

        Settings.RegisterClass<AllKindsOfSettings>()
        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.PCBEndpoints, 
            Views.RandomizedSpinnerWithImages<Endpoint>.GetFactoryForMapping(
                function 
                | Endpoint.Hollow -> { ImageID=Resources.Drawable.endpoint_hollow; Label="hollow" } 
                | Endpoint.Filled -> { ImageID=Resources.Drawable.endpoint_filled; Label="filled" }) )
    
    [<Enum([| Endpoint.Hollow; Endpoint.Filled |], ItemType=typeof<Endpoint>, Name="Custom Spinner", 
      Representation=EnumRepresentation.PCBEndpoints)>]
    member val CustomEnum = Defined(Endpoint.Hollow) with get, set

    [<Enum([| 1; 2; 3; 4; |], Name="Integer Spinner")>]
    member val IntEnum = Defined(1) with get, set

    [<IntRange(2, 5)>]
    member val IntRange = Defined(3) with get, set

    [<FloatRange(1.0f, 2.0f, Step=0.1f)>]
    member val FloatRange = Defined(1.2f) with get, set

    [<Color>]
    member val Color = Defined(0.1f, 0.5f, 0.5f) with get, set

    [<Bool>]
    member val Bool = Defined(false) with get, set

    static member AllRandom = AllKindsOfSettings(CustomEnum=Randomized, IntEnum=Randomized, IntRange=Randomized, FloatRange=Randomized, Color=Randomized, Bool=Randomized)
