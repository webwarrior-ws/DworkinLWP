module Geometry


type Point = OpenTK.Vector2

type Rectangle = OpenTK.Box2


/// Constants for single-precision float (float32)
module Single = 
    let Pi = float32 System.Math.PI
    let HalfPi = float32 (System.Math.PI * 0.5)
    let TwoPi = float32 (System.Math.PI * 2.0)


type OpenTK.Vector2 with
    // Angle in polar coordinates (in radians)
    member this.Theta =
        if this.X = 0.0f then
            if this.Y >= 0.0f then 
                Single.HalfPi 
            else 
                Single.Pi + Single.HalfPi
        else
            let theta = atan (this.Y / this.X)
            if this.X >= 0.0f then
                if this.Y >= 0.0f then 
                    theta 
                else 
                    Single.TwoPi + theta
            else
                Single.Pi + theta

    // Radius in polar coordiantes
    member this.R = sqrt(this.X * this.X + this.Y * this.Y)

    /// Return 0..3 indicating relative direction to otherPoint.
    /// 0 is downRight, 1=downLeft, 2=upLeft, 3=upRight
    member inline this.QuadrantOf(otherPoint : Point) =
        this.QuadrantOf(otherPoint.X, otherPoint.Y)

    member this.QuadrantOf(x : float32, y : float32) =
        if this.X <= x then
            if this.Y < y then 0 else 3
        else
            if this.Y <= y then 1 else 2
    
    member this.DistanceTo(other : Point) =
        let dx = this.X - other.X
        let dy = this.Y - other.Y
        sqrt (dx*dx + dy*dy)


type OpenTK.Box2 with
    member this.Origin = Point(this.Left, this.Top)
    
    member this.Center = Point((this.Left + this.Right) * 0.5f, (this.Top + this.Bottom) * 0.5f)

    member this.IsEmpty = 
        this.Height = 0.0f && this.Width = 0.0f

    member this.Contains(rect : Rectangle) =
        rect.Left >= this.Left && rect.Top >= this.Top && rect.Right <= this.Right && rect.Bottom <= this.Bottom

    member inline this.IntersectsWith(rect : Rectangle) = 
        rect.Right > this.Left && rect.Bottom > this.Top && rect.Left < this.Right && rect.Top < this.Bottom
    
    member inline this.IntersectsWith(rect : byref<Rectangle>) = 
        rect.Right > this.Left && rect.Bottom > this.Top && rect.Left < this.Right && rect.Top < this.Bottom
    
    member this.Merge(rect : Rectangle) = 
        Rectangle.FromTLRB(min this.Top rect.Top, min this.Left rect.Left, max this.Right rect.Right, max this.Bottom rect.Bottom)



let inline deg2rad degrees =
    degrees / 180.0f * Single.Pi

/// linear interpolation between a and by at t [0, 1]
let inline lerp a b t =
    a + ((b - a) * t)
