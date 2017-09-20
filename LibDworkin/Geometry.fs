module LibDworkin.Geometry

open System.Collections.Generic


type Point = OpenTK.Vector2

type Rectangle = OpenTK.Box2


/// Constants for single-precision float (float32)
module Single = 
    let Pi = float32 System.Math.PI
    let HalfPi = float32 (System.Math.PI * 0.5)
    let TwoPi = float32 (System.Math.PI * 2.0)

    let radiansInDegree = Pi / 180.0f


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

    /// Calculate cross product between this and other point
    member this.Cross(other : Point) =
        (this.X * other.Y) - (this.Y * other.X)

    /// Return 0..3 indicating relative direction to otherPoint.
    /// 0 is downRight, 1=downLeft, 2=upLeft, 3=upRight
    member inline this.QuadrantOf(otherPoint : Point) =
        this.QuadrantOf(otherPoint.X, otherPoint.Y)

    member this.QuadrantOf(x : float32, y : float32) =
        if this.X <= x then
            if this.Y < y then 0 else 3
        else
            if this.Y <= y then 1 else 2
    
    member this.SquaredDistanceTo(other : Point) =
        let dx = this.X - other.X
        let dy = this.Y - other.Y
        dx*dx + dy*dy

    member inline this.DistanceTo(other : Point) =
        sqrt (this.SquaredDistanceTo other)
    
    member this.NearestPointAlongLineFromTo(p1 : Point, p2 : Point) =
        if p1.X = p2.X then
            Point(p1.X, this.Y)
        elif p1.Y = p2.Y then
            Point(this.X, p1.Y)
        else
            let x1 = p1.X
            let y1 = p1.Y
            let x21 = p2.X - x1
            let y21 = p2.Y - y1
            let t = (((this.Y - y1) / x21) + ((this.X - x1) / y21)) / ((x21 / y21) + (y21 / x21))
            Point(x1 + (t * x21), y1 + (t * y21))
       
    member this.NearestPointOnLineFromTo(p1, p2) =
        this.NearestPointAlongLineFromTo(p1, p2).AdhereTo(Rectangle(p1, p2))
    
    member inline this.AdhereTo(aRectangle : Rectangle) =
        Point(min (max this.X aRectangle.Left) aRectangle.Right, min (max this.Y aRectangle.Top) aRectangle.Bottom)
 


type OpenTK.Box2 with
    member this.Origin = Point(this.Left, this.Top)
    
    member this.Center = Point((this.Left + this.Right) * 0.5f, (this.Top + this.Bottom) * 0.5f)

    member this.IsEmpty = 
        this.Height = 0.0f && this.Width = 0.0f

    member this.Contains(rect : Rectangle) =
        rect.Left >= this.Left && rect.Top >= this.Top && rect.Right <= this.Right && rect.Bottom <= this.Bottom
    
    member this.ContainsPoint(p : Point) =
        this.Left <= p.X && this.Top <= p.Y && this.Right > p.X && this.Bottom > p.Y 

    member inline this.IntersectsWith(rect : Rectangle) = 
        rect.Right > this.Left && rect.Bottom > this.Top && rect.Left < this.Right && rect.Top < this.Bottom
    
    member inline this.IntersectsWith(rect : byref<Rectangle>) = 
        rect.Right > this.Left && rect.Bottom > this.Top && rect.Left < this.Right && rect.Top < this.Bottom
    
    member this.Merge(rect : Rectangle) = 
        Rectangle.FromTLRB(min this.Top rect.Top, min this.Left rect.Left, max this.Right rect.Right, max this.Bottom rect.Bottom)
    
    member this.TranslateBy(p : Point) =
        Rectangle.FromTLRB(this.Top + p.Y, this.Left + p.X, this.Right + p.X, this.Bottom + p.Y)
    
    static member Encompassing(points: #IList<Point>) =
        let mutable result = Rectangle(points.[0], points.[0])
        for i=1 to points.Count-1 do
            let p = points.[i]
            result.Left <- min result.Left p.X
            result.Top <- min result.Top p.Y
            result.Bottom <- max result.Bottom p.Y
            result.Right <- max result.Right p.X
        result



let inline deg2rad degrees =
    degrees * Single.radiansInDegree

/// linear interpolation between a and by at t [0, 1]
let inline lerp a b t =
    a + ((b - a) * t)
