module SerializationTests

open Expecto

open Settings


type SettingsWithDifferentTypes() =
    inherit Settings()
    static do Settings.RegisterClass<SettingsWithDifferentTypes>()
    [<SettingsClassTests.StubSetting>]
    member val Int = 0 with get, set
    [<SettingsClassTests.StubSetting>]
    member val Float = 0.0f with get, set
    [<SettingsClassTests.StubSetting>]
    member val String = "lol" with get, set
    [<SettingsClassTests.StubSetting>]
    member val RandInt = Defined(0) with get, set
    [<SettingsClassTests.StubSetting>]
    member val RandBool = Defined(false) with get, set
    [<SettingsClassTests.StubSetting>]
    member val Color = (0.0f, 0.0f, 0.0f) with get, set
    [<SettingsClassTests.StubSetting>]
    member val RandColor = Defined(0.0f, 0.0f, 0.0f) with get, set


type SettingsWithExplicitlySetName() =
    inherit Settings()
    static do Settings.RegisterClass<SettingsWithExplicitlySetName>("AlternativeName")
    [<SettingsClassTests.StubSetting>]
    member val Int = 0 with get, set


type SampleEnum =
    | One = 1
    | Two = 2


type SettingsSubclass() =
    inherit SettingsWithDifferentTypes()
    static do Settings.RegisterClass<SettingsSubclass>()


type SampleRecord =
    {
        Foo : int
        Bar : SettingsWithDifferentTypes
    }


[<Tests>]
let serializationTests =
    let roundtrip (x : 'T) = 
        let json = (Serialization.toJsonValue x).ToString()
        let x2 = Serialization.fromJsonValue (System.Json.JsonValue.Parse json)
        Expect.equal x x2 "equal"

    testList "Serialization" [
        testCase "Primitive values roundtrip" <| fun _ ->
            roundtrip 2
            roundtrip 2.5f
            roundtrip true
            roundtrip "lol"

        testCase "Randomizeable values roundtrip" <| fun _ ->
            roundtrip <| Defined 2
            roundtrip <| Defined 2.5f
            roundtrip <| Defined true
            roundtrip <| Defined "lol"
            roundtrip<Randomizeable<int>> <| Randomized

        testCase "Enum values roundtrip" <| fun _ ->
            roundtrip SampleEnum.One
            roundtrip SampleEnum.Two
            roundtrip<Randomizeable<SampleEnum>> <| Defined(SampleEnum.One)
            // values that are not in enum
            Expect.throws (fun () -> Serialization.toJsonValue (enum<SampleEnum> 100) |> ignore) "invalid enum value"

        testCase "Record values roundtrip" <| fun _ ->
            roundtrip { Foo = 1; Bar=SettingsWithDifferentTypes(Int=1) }
            roundtrip { Foo = 2; Bar=SettingsSubclass(Float=2.0f, RandInt=Defined(2)) }
        
        testCase "Settings objects" <| fun _ ->
            roundtrip <| 
                SettingsWithDifferentTypes(Int=45, Float=7.9f, String="foo", RandInt=Randomized, RandBool=Defined(true), 
                                           Color=(1.0f, 0.5f, 0.2f), RandColor=Defined(0.5f, 0.75f, 0.75f))
            roundtrip <| SettingsWithExplicitlySetName(Int=23)
    ]