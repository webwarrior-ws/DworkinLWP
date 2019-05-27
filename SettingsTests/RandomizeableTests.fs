module RandomizeableTests

open Expecto

open Settings


type SettingsWithRandomizeableProperty() =
    inherit Settings()
    [<IntRange(0, 10)>]
    member val X = Defined(-1) with get, set


[<Tests>]
let randomizeableTests =
    testList "Randomizeable.getValue tests" [
        testCase "getValue on Defined property should return its value" <| fun _ ->
            let a = SettingsWithRandomizeableProperty()
            Expect.equal (Randomizeable.getValue (System.Random()) <@ a.X @>) -1 "-1"

        testCase "getValue on Randomized property should return value of attribute's RandomValue()" <| fun _ ->
            let a = SettingsWithRandomizeableProperty(X=Randomized)
            let attr = a.SettingProperties.[0].GetCustomAttributes(false).[0] :?> SettingAttribute
            let seed = 42
            Expect.equal (Randomizeable.getValue (System.Random(seed)) <@ a.X @>) (attr.RandomValue(System.Random(seed)) |> unbox<int>) "random"

        testCase "getValue should fail on expressions that are not property access" <| fun _ ->
            Expect.throws (fun () -> Randomizeable.getValue (System.Random()) <@ Randomized @>) "not a property access"
    ]