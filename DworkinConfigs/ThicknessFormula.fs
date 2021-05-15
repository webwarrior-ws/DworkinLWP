module ThicknessFormula


open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Geometry
open LibDworkin.Agents
open LibDworkin.PharoCompatibility



type Constant() = 
    inherit ThicknessFormula()
    member val C = 1.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> Constant

    static member C_ : Lens<Constant, _> = 
        (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)

    override this.Evaluate(s, t) =
        this.C


type LeafParabolic() = 
    inherit ThicknessFormula()
    member val C = 5.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> LeafParabolic

    static member C_ : Lens<LeafParabolic, _> = 
        (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)

    override this.Evaluate(s, t) =
        this.C * (1.0f - (squared ((t * 2.0f) - 1.0f)))

    
type ConcaveParabolic() = 
    inherit ThicknessFormula()
    member val C = 5.0f with get, set
    member val Alpha = 0.5f with get, set

    member this.Clone() = this.MemberwiseClone() :?> ConcaveParabolic

    static member C_ : Lens<ConcaveParabolic, _> = 
        (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)
    static member Alpha_ : Lens<ConcaveParabolic, _> = 
        (fun x -> x.Alpha), (fun value x -> let y = x.Clone() in y.Alpha <- value; y)

    override this.Evaluate(s, t) =
        this.C * (1.0f - ((1.0f - this.Alpha) * (1.0f - (squared ((t * 2.0f) - 1.0f)))))
  

type LeafPower_Base() = 
    inherit ThicknessFormula()
    
    let mutable a = 2.0f
    let mutable p = 0.74f
    let mutable pcurve = getSpecificPCurve (a*p) (a * (1.0f-p))

    member this.A with get() = a and set(value) = a <- value; this.UpdatePCurve()
    member this.P with get() = p and set(value) = p <- value; this.UpdatePCurve()
    member val C = 2.5f with get, set
    member val Base = 0.25f with get, set

    member this.Clone() = this.MemberwiseClone() :?> LeafPower_Base

    static member A_ : Lens<LeafPower_Base, _> = 
        (fun x -> x.A), (fun value x -> let y = x.Clone() in y.A <- value; y)
    static member P_ : Lens<LeafPower_Base, _> = 
        (fun x -> x.P), (fun value x -> let y = x.Clone() in y.P <- value; y)
    static member C_ : Lens<LeafPower_Base, _> = 
        (fun x -> x.C), (fun value x -> let y = x.Clone() in y.C <- value; y)
    static member Base_ : Lens<LeafPower_Base, _> = 
        (fun x -> x.Base), (fun value x -> let y = x.Clone() in y.Base <- value; y)
    
    member private this.UpdatePCurve() =
        pcurve <- getSpecificPCurve (a*p) (a * (1.0f-p))

    override this.Evaluate(s, t) =
        this.C * (pcurve t + this.Base)


type LeafPower() = 
    inherit LeafPower_Base(A=8.0f, P=0.15f, C=5.0f, Base=0.0f)


type LeafPowerWithStem() = 
    inherit LeafPower_Base(A=3.0f, P=0.4f, C=5.0f, Base=0.0f)
    member val Sl = 0.25f with get, set
    member val St = 0.2f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> LeafPowerWithStem
    
    static member Sl_ : Lens<LeafPowerWithStem, _> = 
        (fun x -> x.Sl), (fun value x -> let y = x.Clone() in y.Sl <- value; y)
    static member St_ : Lens<LeafPowerWithStem, _> = 
        (fun x -> x.St), (fun value x -> let y = x.Clone() in y.St <- value; y)
    
    override this.Evaluate(s, t) =
        let t2 = (max ((t - this.Sl) / (1.0f - this.Sl)) 0.0f)
        base.Evaluate(s, t2)


type PointyTip() = 
    inherit ThicknessFormula()
    member val A = 5.0f with get, set
    member val K = 0.008f with get, set
    
    member this.Clone() = this.MemberwiseClone() :?> PointyTip
    
    static member A_ : Lens<PointyTip, _> = 
        (fun x -> x.A), (fun value x -> let y = x.Clone() in y.A <- value; y)
    static member K_ : Lens<PointyTip, _> = 
        (fun x -> x.K), (fun value x -> let y = x.Clone() in y.K <- value; y)
    
    override this.Evaluate(s, t) =
        min this.A (((s / t) / this.K) - (s / this.K))
