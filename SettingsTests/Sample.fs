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
    testList "samples" [
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
