module SettingsClassTests

open Expecto

open Settings


type StubSettingAttribute() = 
    inherit SettingAttribute()
    override this.RandomValue(r) = failwith "stub"
    override this.BuildUIControl(builder) = failwith "stub"

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

type WithRand() =
    inherit Settings()
    [<StubSetting>]
    member val Foo = Defined(0) with get, set
    [<StubSetting>]
    member val Bar = Defined(1) with get, set
    [<StubSetting>]
    member val Baz = 42 with get, set


[<Tests>]
let equalityTests =
    testList "Settings equality" [
        testCase "Two Settings objects created with default constructor should equal" <| fun _ ->
            Expect.equal (A()) (A()) "equal"
        
        testCase "Equal Settings objects should equal if cast to superclass" <| fun _ ->
            Expect.equal (A() :> Settings) (A() :> Settings) "equal"
        
        testCase "Settings object should be equal to itself" <| fun _ ->
            let a = A(Foo=5, Bar=33)
            Expect.equal a a "equal"

        testCase "Settings objects with different values shouldn't equal" <| fun _ ->
            Expect.notEqual (A(Foo=1)) (A(Foo=2)) "have different values"

        testCase "Settings objects of different types shouldn't equal" <| fun _ ->
            Expect.notEqual (A() :> Settings) (B() :> Settings) "different types"
    ]

[<Tests>]
let hasRandomizedValuesTests =
    testList "HasRandomizedValues property" [
        testCase "HasRandomizedValues on object with no Randomizeable<'T> properties should return false" <| fun _ ->
            Expect.isFalse (A().HasRandomizedValues) "no Randomizeable<'T> properties"
        
        testCase "HasRandomizedValues on object with Randomizeable<'T> properties with Defined values should return false" <| fun _ ->
            let x = WithRand()
            Expect.isFalse x.HasRandomizedValues "no randomized values"
        
        testCase "HasRandomizedValues on object with Randomizeable<'T> properties with Randomized values should return true" <| fun _ ->
            let x = WithRand(Bar=Randomized)
            Expect.isTrue x.HasRandomizedValues "randomized values"
            let y = WithRand(Foo=Randomized, Bar=Randomized)
            Expect.isTrue y.HasRandomizedValues "randomized values"
    ]