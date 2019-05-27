module Tests

open Expecto

open Settings


type StubSettingAttribute() = 
    inherit SettingAttribute()
    override this.RandomValue(r) = failwith "stub"

type A() =
    inherit Settings()
    [<StubSetting>]
    member val Foo = 0 with get, set
    [<StubSetting>]
    member val Bar = 1 with get, set
    
type B() =
    inherit Settings()
    [<StubSetting>]
    member val Baz = 10 with get, set


[<Tests>]
let equalityTests =
    testList "Settings equality" [
        testCase "Two Settings objects created with default constructor should equal" <| fun _ ->
            Expect.equal (A()) (A()) "equal"
        
        testCase "Settings object should be equal to itself" <| fun _ ->
            let a = A(Foo=5, Bar=33)
            Expect.equal a a "equal"

        testCase "Settings objects with different values shouldn't equal" <| fun _ ->
            Expect.notEqual (A(Foo=1)) (A(Foo=2)) "have different values"

        testCase "Settings objects of different types shouldn't equal" <| fun _ ->
            Expect.notEqual (A() :> Settings) (B() :> Settings) "different types"
    ]


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