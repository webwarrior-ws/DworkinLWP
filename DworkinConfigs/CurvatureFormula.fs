module CurvatureFormula


open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Geometry
open LibDworkin.Agents
open LibDworkin.PharoCompatibility
open LibDworkin.SimplexNoise


type Tan_t_() = 
    inherit CurvatureFormula()
    member val C = 0.023f with get, set

    member this.Clone() = this.MemberwiseClone() :?> Tan_t_

    static member C_ : Lens<Tan_t_, _> = 
        (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)

    override this.Evaluate(s, t) =
        (tan (t * Single.HalfPi)) * this.C
 
 
type MagneticWithFlip() = 
    inherit CurvatureFormula()
    member val Alpha = 0.72f with get, set
    member val Midpoint = 0.16f with get, set
    member val I = 20.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> MagneticWithFlip

    static member Alpha_ : Lens<MagneticWithFlip, _> = 
        (fun x -> x.Alpha), (fun value x -> let y = x.Clone() in y.Alpha <- value; y)
    static member Midpoint_ : Lens<MagneticWithFlip, _> = 
        (fun x -> x.Midpoint), (fun value x -> let y = x.Clone() in y.Midpoint <- value; y)
    static member I_ : Lens<MagneticWithFlip, _> = 
        (fun x -> x.I), (fun value x -> let y = x.Clone() in y.I <- value; y)

    override this.Evaluate(s, t) =
        (((1.0f / (1.0f + (exp (-((this.Midpoint - t) * this.I))))) - 0.5f) * -2.0f) * ((((s / t) - s) * 100.0f) ** (-this.Alpha))


type SNoise() = 
    inherit CurvatureFormula()
    member val A = 0.15f with get, set
    member val B = 16.9f with get, set
    member val C = 0.0f with get, set
    member val D = 0.16f with get, set

    member this.Clone() = this.MemberwiseClone() :?> SNoise

    static member A_ : Lens<SNoise, _> = 
        (fun x -> x.A), (fun value x -> let y = x.Clone() in y.A <- value; y)
    static member B_ : Lens<SNoise, _> = 
        (fun x -> x.B), (fun value x -> let y = x.Clone() in y.B <- value; y)
    static member C_ : Lens<SNoise, _> = 
        (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)
    static member D_ : Lens<SNoise, _> = 
        (fun x -> x.D), (fun value x -> let y = x.Clone() in y.D <- value; y)

    override this.Evaluate(s, t) =
        this.A * ((s + this.D) * this.B).SNoise() + this.C


type Quadratic() = 
    inherit CurvatureFormula()
    member val A = -0.64f with get, set
    member val C = -0.24f with get, set
    member val B = 0.742f with get, set
    member val K = 0.45f with get, set

    member this.Clone() = this.MemberwiseClone() :?> Quadratic

    static member A_ : Lens<Quadratic, _> = 
        (fun x -> x.A), (fun value x -> let y = x.Clone() in y.A <- value; y)
    static member C_ : Lens<Quadratic, _> = 
        (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)
    static member B_ : Lens<Quadratic, _> = 
        (fun x -> x.B), (fun value x -> let y = x.Clone() in y.B <- value; y)
    static member K_ : Lens<Quadratic, _> = 
        (fun x -> x.K), (fun value x -> let y = x.Clone() in y.K <- value; y)

    override this.Evaluate(s, t) =
        let x = s * this.K
        this.A * x*x + this.B * x + this.C


type Linear() = 
    inherit CurvatureFormula()
    member val A = 0.023f with get, set
    member val B = -0.109f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> Linear
    
    static member A_ : Lens<Linear, _> = 
        (fun x -> x.A), (fun value x -> let y = x.Clone() in y.A <- value; y)
    static member B_ : Lens<Linear, _> = 
        (fun x -> x.B), (fun value x -> let y = x.Clone() in y.B <- value; y)
    
    override this.Evaluate(s, t) =
        (this.A * s) + this.B


type DoubleLogSpiralSimple() = 
    inherit CurvatureFormula()
    member val A = 0.0265f with get, set
    member val D = 0.01f with get, set
    member val B = 0.0265f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> DoubleLogSpiralSimple
    
    static member A_ : Lens<DoubleLogSpiralSimple, _> = 
        (fun x -> x.A), (fun value x -> let y = x.Clone() in y.A <- value; y)
    static member D_ : Lens<DoubleLogSpiralSimple, _> = 
        (fun x -> x.D), (fun value x -> let y = x.Clone() in y.D <- value; y)
    static member B_ : Lens<DoubleLogSpiralSimple, _> = 
        (fun x -> x.B), (fun value x -> let y = x.Clone() in y.B <- value; y)
    
    override this.Evaluate(s, t) =
        (this.A / (t + this.D)) + (this.B / ((t - this.D) - 1.0f))


type DirectionPreservingTurn() = 
    inherit CurvatureFormula()
    member val A = 0.051f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> DirectionPreservingTurn
    
    static member A_ : Lens<DirectionPreservingTurn, _> = 
        (fun x -> x.A), (fun value x -> let y = x.Clone() in y.A <- value; y)
    
    override this.Evaluate(s, t) =
        ((this.A * t) / s) * (sin (t * Single.TwoPi))


type Magnetic() = 
    inherit CurvatureFormula()
    member val Alpha = 0.68f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> Magnetic
    
    static member Alpha_ : Lens<Magnetic, _> = 
        (fun x -> x.Alpha), (fun value x -> let y = x.Clone() in y.Alpha <- value; y)
    
    override this.Evaluate(s, t) =
        (((s / t) - s) * 100.0f) ** (-this.Alpha)


type Cosine() = 
    inherit CurvatureFormula()
    member val A = 0.05f with get, set
    member val B = 1.0f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> Cosine
    
    static member A_ : Lens<Cosine, _> = 
        (fun x -> x.A), (fun value x -> let y = x.Clone() in y.A <- value; y)
    static member B_ : Lens<Cosine, _> = 
        (fun x -> x.B), (fun value x -> let y = x.Clone() in y.B <- value; y)
    
    override this.Evaluate(s, t) =
        this.A * (cos ((s + 0.00485f) * this.A * 100.0f * this.B))
