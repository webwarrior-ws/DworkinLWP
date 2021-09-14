module Util


open LibDworkin.Geometry
open LibDworkin.Agents
open LibDworkin.Colliders


/// Build walls made of segment colliders along environment bounds
let buildWalls (env : DworkinEnvironment) =
    let c = env.Bounds.Center
    for p in env.Bounds.Corners do
        let d = p - c
        env.AddCollider(SegmentCollider(None, p, Point(c.X + (float32(sign d.X) * 0.001f), p.Y)))
        env.AddCollider(SegmentCollider(None, p, Point(p.X, c.Y + (float32(sign d.Y) * 0.001f))))


type JointType =
    | Bevel
    | Miter
    | BevelForLargeAngles

let drawJoint (parent: DworkinAgent, newDirection: Point, jointType: JointType, numSteps: int, delay: int) =
    let p = parent.Position
    let turnFactor = -(sign <| newDirection.Cross(parent.Direction))
    if turnFactor <> 0 then
        let parentPerp = (parent.Direction.LeftRotated * parent.Thickness) * 0.5f * (float32 turnFactor)
        let agentPerp = (newDirection.LeftRotated * parent.Thickness) * 0.5f * (float32 turnFactor)
        let a = p + parentPerp
        let b = p + agentPerp
        match jointType with
        | Bevel -> 
            parent.DworkinEnvironment.AddAgent(GradualTriangleDrawingAgent(p, a, b, numSteps, delay))
        | BevelForLargeAngles ->
            let t1NumSteps = numSteps / 2
            let t2NumSteps = numSteps - t1NumSteps
            let a2 = p - parentPerp
            let cosAplha = Point.Dot(parent.Direction, newDirection)
            let c = p + parent.Direction * sqrt(1.0f - (cosAplha*cosAplha)) * parent.Thickness * 0.5f
            parent.DworkinEnvironment.AddAgent(GradualTriangleDrawingAgent(a2, a, c, t1NumSteps, delay))
            parent.DworkinEnvironment.AddAgent(GradualTriangleDrawingAgent(a2, c, b, t2NumSteps, t1NumSteps+delay))
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
                        
            let t1NumSteps = numSteps / 2
            let t2NumSteps = numSteps - t1NumSteps
            parent.DworkinEnvironment.AddAgent(GradualTriangleDrawingAgent(p, a, c, t1NumSteps, delay))
            parent.DworkinEnvironment.AddAgent(GradualTriangleDrawingAgent(p, c, b, t2NumSteps, delay+t1NumSteps))