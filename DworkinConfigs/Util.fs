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
