module LibDworkin.Configs


// Concrete config classes

open Aether
open Geometry
open PharoCompatibility
open AgentAbstractions
open Colliders
open Agents
open Brushes


// Brush strategies

type BrushStrategyCircles() = 
    inherit BrushStrategy()
    override this.GetBrushFor(anAgent) =
        BrushCircles() :> Brush
    
(*    
type BrushStrategyCirclesPacked() = 
    inherit BrushStrategy()
    member val Gap = 0.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> BrushStrategyCirclesPacked

    static member Gap_ : Lens<BrushStrategy, float32> = 
        (fun x -> (x :?> BrushStrategyCirclesPacked).Gap), (fun value x -> let y = (x :?> BrushStrategyCirclesPacked).Clone() in y.Gap <- value; y :> BrushStrategy)

    override this.GetBrushFor(anAgent) =
        BrushCirclesPacked(anAgent, Gap=this.Gap)
*)

type BrushStrategyDefault() = 
    inherit BrushStrategy()
    override this.IsFinal = false

    member this.FallbackBrush =
        BrushSolid(Cap=BrushCap.NoCap, AlignWithParent=false)    

    override this.GetBrushFor(anAgent) : Brush =
        let mutable curr = anAgent.Parent
        while curr |> Option.exists (fun a -> a.Config.Brush.IsFinal) do
            curr <- curr.Value.Parent
        match curr with 
        | Some(a) -> a.Config.Brush.GetBrushFor(anAgent) 
        | None -> this.FallbackBrush :> Brush


type BrushStrategyNone() = 
    inherit BrushStrategy()
    override this.GetBrushFor(anAgent) =
        BrushNone() :> Brush

    member this.IsFinal() = false
    

type BrushStrategySolid() = 
    inherit BrushStrategy()
    member val AlignWithParent = false with get, set
    member val Cap : BrushCap = BrushCap.NoCap with get, set

    member this.Clone() = this.MemberwiseClone() :?> BrushStrategySolid

    static member AlignWithParent_ : Lens<BrushStrategy, bool> = 
        (fun x -> (x :?> BrushStrategySolid).AlignWithParent), (fun value x -> let y = (x :?> BrushStrategySolid).Clone() in y.AlignWithParent <- value; y :> BrushStrategy)

    static member Cap_ : Lens<BrushStrategy, BrushCap> = 
        (fun x -> (x :?> BrushStrategySolid).Cap), (fun value x -> let y = (x :?> BrushStrategySolid).Clone() in y.Cap <- value; y :> BrushStrategy)

    override this.GetBrushFor(anAgent) =
        BrushSolid(Cap=this.Cap, AlignWithParent=this.AlignWithParent) :> Brush

    
type BrushStrategySolidJoint() = 
    inherit BrushStrategy()
    member val JointType : BrushJoint = BrushJoint.Triangle with get, set

    member this.Clone() = this.MemberwiseClone() :?> BrushStrategySolidJoint

    static member JointType_ : Lens<BrushStrategy, BrushJoint> = 
        (fun x -> (x :?> BrushStrategySolidJoint).JointType), (fun value x -> let y = (x :?> BrushStrategySolidJoint).Clone() in y.JointType <- value; y :> BrushStrategy)

    override this.GetBrushFor(anAgent) =
        BrushSolidJoint(JointType=this.JointType) :> Brush
    




