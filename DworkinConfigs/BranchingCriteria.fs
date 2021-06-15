module BranchingCriteria


open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Geometry
open LibDworkin.AgentAbstractions
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.SpecializedCollections
open LibDworkin.PharoCompatibility



module BranchingCriterionCustom =
    type IntervalBoundedTRand() = 
        inherit BranchingCriterionCustom()
        member val MinRange = NumberRange(0.2f, 0.5f) with get, set
        member val MaxRange = NumberRange(0.5f, 0.8f) with get, set
        member val D = 10.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> IntervalBoundedTRand

        static member MinRange_ : Lens<IntervalBoundedTRand, _> = 
            (fun x -> x.MinRange), (fun value x -> let y = x.Clone() in y.MinRange <- value; y)
        static member MaxRange_ : Lens<IntervalBoundedTRand, _> = 
            (fun x -> x.MaxRange), (fun value x -> let y = x.Clone() in y.MaxRange <- value; y)
        static member D_ : Lens<IntervalBoundedTRand, _> = 
            (fun x -> x.D), (fun value x -> let y = x.Clone() in y.D <- value; y)

        override this.Evaluate(agent, mark, tag) =
            let tInterval = 
                agent.Attributes.BranchInterval.GetOrSetIfAbsent(
                    fun () -> NumberRange(agent.Random.NextInRange(this.MinRange), agent.Random.NextInRange(this.MaxRange)))
            ((mark.S + this.D) < agent.S) && (tInterval.Contains agent.T)


    type IntervalRandBoundedT() = 
        inherit BranchingCriterionCustom()
        member val DRange = NumberRange(40.0f, 60.0f) with get, set
        member val TInterval = NumberRange(0.0f, 0.6f) with get, set
    
        member this.Clone() = this.MemberwiseClone() :?> IntervalRandBoundedT
    
        static member DRange_ : Lens<IntervalRandBoundedT, _> = 
            (fun x -> x.DRange), (fun value x -> let y = x.Clone() in y.DRange <- value; y)
        static member TInterval_ : Lens<IntervalRandBoundedT, _> = 
            (fun x -> x.TInterval), (fun value x -> let y = x.Clone() in y.TInterval <- value; y)
    
        override this.Evaluate(agent, mark, tag) =
            let d = mark.GetNextIntervalOrPut(fun () -> agent.Random.NextInRange(this.DRange))
            (mark.S + d) < agent.S && (this.TInterval.Contains agent.T)


    type Interval_MaxBranches() = 
        inherit BranchingCriterionCustom()
        member val D = 18.0f with get, set
        member val N = 2 with get, set
    
        member this.Clone() = this.MemberwiseClone() :?> Interval_MaxBranches
    
        static member D_ : Lens<Interval_MaxBranches, _> = 
            (fun x -> x.D), (fun value x -> let y = x.Clone() in y.D <- value; y)
        static member N_ : Lens<Interval_MaxBranches, _> = 
            (fun x -> x.N), (fun value x -> let y = x.Clone() in y.N <- value; y)
    
        override this.Evaluate(agent, mark, tag) =
            ((agent.NumBranchesFor(tag)) < this.N) && ((mark.S + this.D) < agent.S)


    type RandEachStepMinInt() = 
        inherit BranchingCriterionCustom()
        member val P = 0.125f with get, set
        member val MinInterval = 2.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> RandEachStepMinInt

        static member P_ : Lens<RandEachStepMinInt, _> = 
            (fun x -> x.P), (fun value x -> let y = x.Clone() in y.P <- value; y)
        static member MinInterval_ : Lens<RandEachStepMinInt, _> = 
            (fun x -> x.MinInterval), (fun value x -> let y = x.Clone() in y.MinInterval <- value; y)

        override this.Evaluate(agent, mark, tag) =
            (agent.Random.NextFloat() < (1.0f - ((1.0f - this.P) ** (agent.Speed / agent.LengthFactor)))) && ((agent.S - mark.S) >= this.MinInterval)



module BranchingCriterionInterval =
    type ConstantBoundedT() = 
        inherit BranchingCriterionInterval()
        member val D = 10.0f with get, set
        member val Start = 0.2f with get, set
        member val End = 0.8f with get, set

        member this.Clone() = this.MemberwiseClone() :?> ConstantBoundedT

        static member D_ : Lens<ConstantBoundedT, _> = 
            (fun x -> x.D), (fun value x -> let y = x.Clone() in y.D <- value; y)
        static member Start_ : Lens<ConstantBoundedT, _> = 
            (fun x -> x.Start), (fun value x -> let y = x.Clone() in y.Start <- value; y)
        static member End_ : Lens<ConstantBoundedT, _> = 
            (fun x -> x.End), (fun value x -> let y = x.Clone() in y.End <- value; y)

        override this.Evaluate(s, t) =
            if between this.Start this.End t then this.D else 1000000.0f



module BranchingCriterionIntervalRandomized =
    type Uniform() = 
        inherit BranchingCriterionIntervalRandomized()
        member val IntervalRange = NumberRange(10.0f, 30.0f) with get, set

        member this.Clone() = this.MemberwiseClone() :?> Uniform

        static member IntervalRange_ : Lens<Uniform, _> = 
            (fun x -> x.IntervalRange), (fun value x -> let y = x.Clone() in y.IntervalRange <- value; y)

        override this.Evaluate(s, t, random) =
            random.NextInRange(this.IntervalRange)


    type UniformBounded() = 
        inherit BranchingCriterionIntervalRandomized()
        member val IntervalRange = NumberRange(10.0f, 60.0f) with get, set
        member val Margin = 5.0f with get, set

        member this.Clone() = this.MemberwiseClone() :?> UniformBounded

        static member IntervalRange_ : Lens<UniformBounded, _> = 
            (fun x -> x.IntervalRange), (fun value x -> let y = x.Clone() in y.IntervalRange <- value; y)
        static member Margin_ : Lens<UniformBounded, _> = 
            (fun x -> x.Margin), (fun value x -> let y = x.Clone() in y.Margin <- value; y)

        override this.Evaluate(s, t, random) =
            let x = random.NextInRange(this.IntervalRange)
            if (s + x) > ((s / t) - this.Margin) then 100000.0f else x


    type Gaussian() = 
        inherit BranchingCriterionIntervalRandomized()
        member val Mu = 20.0f with get, set
        member val Sigma = 5.0f with get, set
    
        member this.Clone() = this.MemberwiseClone() :?> Gaussian
    
        static member Mu_ : Lens<Gaussian, _> = 
            (fun x -> x.Mu), (fun value x -> let y = x.Clone() in y.Mu <- value; y)
        static member Sigma_ : Lens<Gaussian, _> = 
            (fun x -> x.Sigma), (fun value x -> let y = x.Clone() in y.Sigma <- value; y)
    
        override this.Evaluate(s, t, random) =
            random.NextGaussian(this.Mu, this.Sigma)
