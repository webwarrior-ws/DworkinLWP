module LibDworkin.Brushes

open AgentAbstractions
open Agents
open PharoCompatibility
open Geometry
open SpecializedCollections

open System


type BrushCap =
    | NoCap
    | Round


type BrushJoint =
    | Bevel
    | Miter


type BrushNone() =
    inherit Brush()
    override this.Evaluate(agent) = ()


type BrushCircles() =
    inherit Brush()
    override this.Evaluate(agent) =
        agent.DworkinEnvironment.AddShape(Shape.Circle(agent.Position, agent.Thickness * 0.5f), ShapePaint.Default)
 

[<AbstractClass>]
type BrushSolidBase() =
    inherit Brush()

    let mutable prevPoints : struct(Point * Point) voption = ValueNone

    abstract member GetAngle : DworkinAgent -> float32
    default this.GetAngle(_) = 0.0f

    override this.Evaluate(agent) =
        let mutable perpendicular = agent.Direction.LeftRotated * (agent.Thickness * 0.5f)
        let angle = this.GetAngle(agent)
        if angle <> 0.0f then
            perpendicular <- (perpendicular / (max (cos angle) 0.25f)).Rotate(angle)
        let lhs = agent.Position + perpendicular
        let rhs = agent.Position - perpendicular
        match prevPoints with
        | ValueSome(struct (prevLhs, prevRhs)) -> 
            agent.DworkinEnvironment.AddShape(Shape.Quad(prevLhs, lhs, rhs, prevRhs), ShapePaint.Default)
        | ValueNone -> ()
        prevPoints <- ValueSome(struct(lhs, rhs))


type BrushSolid() =
    inherit BrushSolidBase()
    let mutable lastBrushAngle : float32 = Single.NaN
    member val Cap = BrushCap.NoCap with get, set
    member val AlignWithParent = false with get, set

    override this.GetAngle(agent : DworkinAgent) : float32 =
        if not this.AlignWithParent then
            0.0f
        else
            match agent.Parent with
            | Some(parent) -> 
                if lastBrushAngle <> Single.NaN then
                    if lastBrushAngle = 0.0f then
                        0.0f
                    else
                        let angle = (lastBrushAngle / (squared (1.0f + ((agent.Speed / (max agent.Thickness 0.001f)) * 0.5f)))) |> roundTo 0.01f
                        lastBrushAngle <- angle
                        angle
                else
                    let mutable angle = ((agent.Direction.Angle - parent.Direction.Angle) - Single.HalfPi) %% Single.Pi
                    if angle < (-Single.HalfPi) then
                        angle <- Single.Pi + angle
                    if angle > Single.HalfPi then
                        angle <- angle - Single.Pi
                    if (abs ((angle %% Single.Pi) - Single.HalfPi)) < (Single.HalfPi / 6.0f) then
                        angle <- 0.0f
                    lastBrushAngle <- angle
                    angle
            | None -> 0.0f
    
    override this.Evaluate(agent) =
        base.Evaluate(agent)
        match this.Cap with
        | NoCap -> ()
        | Round -> 
            // TODO: draw half-circle
            agent.DworkinEnvironment.AddShape(Shape.Circle(agent.Position, agent.Thickness * 0.5f), ShapePaint.Default)


type BrushSolidJoint() =
    inherit BrushSolidBase()
    let mutable jointDrawn = false
    member val JointType = BrushJoint.Miter with get, set

    override this.Evaluate(agent) =
        if jointDrawn then
            base.Evaluate(agent)
        else
            match agent.Parent with
            | None -> ()
            | Some(_) when not (agent.Attributes.DrawJoint.Get() |> Option.exists id) -> ()
            | Some(parent) ->
                let p = parent.Position
                let turnFactor = -(sign <| agent.Direction.Cross(parent.Direction))
                if turnFactor <> 0 then
                    let parentPerp = (parent.Direction.LeftRotated * parent.Thickness) * 0.5f * (float32 turnFactor)
                    let agentPerp = (agent.Direction.LeftRotated * agent.Thickness) * 0.5f * (float32 turnFactor)
                    let a = p + parentPerp
                    let b = p + agentPerp

                    match this.JointType with
                    | Bevel -> 
                        agent.DworkinEnvironment.AddShape(Shape.Triangle(a, b, p), ShapePaint.Default)
                    | Miter ->
                        let angle = agentPerp.AngleWith(parentPerp)
                        let c = p + (parentPerp + agentPerp) * (0.5f / abs (tan ((angle + Single.Pi) * 0.5f)) + 0.5f)
                        agent.DworkinEnvironment.AddShape(Shape.Quad(a, c, b, p), ShapePaint.Default)
                    
            jointDrawn <- true
            base.Evaluate(agent)

