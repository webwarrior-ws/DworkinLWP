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

    member val PrevPoints : struct(Point * Point) voption = ValueNone with get, set

    abstract member GetAngle : DworkinAgent -> float32
    default this.GetAngle(_) = 0.0f

    override this.Evaluate(agent) =
        let mutable perpendicular = agent.Direction.LeftRotated * (agent.Thickness * 0.5f)
        let angle = this.GetAngle(agent)
        if angle <> 0.0f then
            perpendicular <- (perpendicular / (max (cos angle) 0.25f)).Rotate(angle)
        let lhs = agent.Position + perpendicular
        let rhs = agent.Position - perpendicular
        match this.PrevPoints with
        | ValueSome(struct (prevLhs, prevRhs)) -> 
            agent.DworkinEnvironment.AddShape(Shape.Quad(prevLhs, lhs, rhs, prevRhs), ShapePaint.Default)
        | ValueNone -> ()
        this.PrevPoints <- ValueSome(struct(lhs, rhs))


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
        if not jointDrawn then
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
                    let numSteps = int((parent.Thickness + agent.Thickness) / 2.0f / agent.Speed) |> max 1
                    match this.JointType with
                    | Bevel -> 
                        agent.DworkinEnvironment.AddAgent(GrowingShapeDrawingAgent(Shape.Triangle(a, b, p), p, numSteps))
                    | Miter ->
                        let c =
                            // find intersection between lines on which point c lies
                            let a' = a + parentPerp.PerpendicularRight
                            let b' = b + agentPerp.PerpendicularLeft

                            let a1 = a'.Y - a.Y 
                            let b1 = a.X - a'.X 
                            let c1 = a1 * a.X + b1 * a.Y; 
                            let a2 = b'.Y - b.Y
                            let b2 = b.X - b'.X
                            let c2 = a2 * b.X + b2 * b.Y

                            let determinant = a1 * b2 - a2 * b1

                            if determinant = 0.0f then 
                                // The lines are parallel
                                a
                            else
                                let x = (b2 * c1 - b1 * c2) / determinant
                                let y = (a1 * c2 - a2 * c1) / determinant
                                Point(x, y)
                        
                        agent.DworkinEnvironment.AddAgent(GrowingShapeDrawingAgent(Shape.Quad(a, c, b, p), p, numSteps))
            jointDrawn <- true
        
        base.Evaluate(agent)


type DworkinBrushSolidSmoother(smoothness) =
    inherit BrushSolidBase()

    let mutable lastThickness = 0.0f
    let mutable lastPosition = Point()
    let mutable lastDirection = Point()
    let mutable lastS = 0.0f

    member this.NumStepsFor(k) =
        floor (sqrt (k * smoothness)) |> int

    override this.Evaluate(agent) =
        let mutable perpendicular = agent.Direction.LeftRotated * (agent.Thickness * 0.5f)
        
        let lhs = agent.Position + perpendicular
        let rhs = agent.Position - perpendicular
        match this.PrevPoints with
        | ValueSome(struct (prevLhs, prevRhs)) -> 
            let mutable prevLhs = prevLhs
            let mutable prevRhs = prevRhs
            let k = (abs (agent.Thickness - lastThickness)) / (agent.DistanceCovered - lastS)
            let n = this.NumStepsFor(k)
            for i=0 to n do
                let t = float32(i) / (float32 (n + 1))
                let pos = lerp lastPosition agent.Position t
                let thickness = 
                    let s = lerp lastS agent.DistanceCovered t
                    agent.Config.Thickness.Evaluate(s * DworkinAgentFormula.Factor, s / agent.TotalDistance)
                let perp = (lerp lastDirection agent.Direction t).LeftRotated * ((thickness * agent.ThicknessFactor) * 0.5f)
                let currLhs = pos + perp
                let currRhs = pos - perp
                agent.DworkinEnvironment.AddShape(Shape.Quad(prevLhs, currLhs, currRhs, prevRhs), ShapePaint.Default)
                prevLhs <- currLhs
                prevRhs <- currRhs
            agent.DworkinEnvironment.AddShape(Shape.Quad(prevLhs, lhs, rhs, prevRhs), ShapePaint.Default)
        | ValueNone -> ()
        this.PrevPoints <- ValueSome(struct(lhs, rhs))
        lastDirection <- agent.Direction
        lastThickness <- agent.Thickness
        lastPosition <- agent.Position
        lastS <- agent.DistanceCovered

