module LibDworkin.Lenses

open Aether
open Aether.Operators

open Agents
open Configs

// utility functions: 

/// Isomorphism between two types. Unfortunately it's not possible to add subtype parameter constraint, so use cautiously.
let cast<'a, 'b> : Isomorphism<'a, 'b> =
    ((fun object -> box object :?> 'b), (fun object -> box object :?> 'a))

/// Epimorphism between broader and narrower types. Unfortunately it's not possible to add subtype parameter constraint, so use cautiously.
let tryCast<'a, 'b> : Epimorphism<'a, 'b> =
    (fun object -> 
        match box object with
        | :? 'b as result -> Some(result)
        | _ -> None),
    (fun object -> box object :?> 'a)

/// utility function for defining generic lenses using static type constraints
let makeGenericLens lensGetter =
    (fun object -> (lensGetter object) |> fst <| object), (fun value object -> (lensGetter object) |> snd <| value <| object)

// that's how you make generic (duck-typed) lens:
//let inline c_< ^U , ^V when ^U : (static member C_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
//    makeGenericLens (fun (_ : ^U) -> (^U : (static member C_ : Lens< ^U, ^V>) ()))


let nth_<'T> (index : int) : Lens<'T[], 'T> =
    (fun arr -> arr.[index]), (fun value a -> let a2 = Array.copy a in Array.set a2 index value; a2)


// lenses for configs:

// DworkinEnvironmentConfig:
let randomSeed_<'a when 'a :> DworkinEnvironmentConfig> : Lens<'a, int> = 
    (fun x -> x.RandomSeed), (fun value x -> let y = x.Clone() in y.RandomSeed <- value; y :?> 'a)

// DworkinAgentConfig:
let branching_<'a when 'a :> BranchingStrategy> : Lens<DworkinAgentConfig, 'a> = 
    DworkinAgentConfig.Branching_ >-> cast

let brush_<'a when 'a :> BrushStrategy> : Lens<DworkinAgentConfig, 'a> = 
    DworkinAgentConfig.Brush_ >-> cast

let curvature_<'a when 'a :> CurvatureFormula> : Lens<DworkinAgentConfig, 'a> = 
    DworkinAgentConfig.Curvature_ >-> cast

let speed_<'a when 'a :> AgentSpeed> : Lens<DworkinAgentConfig, 'a> = 
    DworkinAgentConfig.Speed_ >-> cast

let terminationConditions_ = DworkinAgentConfig.TerminationConditions_

let termination_<'a when 'a :> TerminationStrategy> : Lens<DworkinAgentConfig, 'a> = 
    DworkinAgentConfig.Termination_ >-> cast

let thickness_<'a when 'a :> ThicknessFormula> : Lens<DworkinAgentConfig, 'a> = 
    DworkinAgentConfig.Thickness_ >-> cast

let totalDistance_ = DworkinAgentConfig.TotalDistance_

let colliderOptions_<'a when 'a :> ColliderOptions> : Lens<DworkinAgentConfig, 'a> = 
    DworkinAgentConfig.ColliderOptions_ >-> cast
    // maybe a little bit faster variant:
    //DworkinAgentConfig.ColliderOptions_ >-> ((fun (object : ColliderOptions) -> object :?> 'a), (fun object -> object :> ColliderOptions))


// TerminationConditionsCollision:
let handler_<'a when 'a :> CollisionHandler> : Lens<TerminationConditionCollision, 'a> = 
    TerminationConditionCollision.Handler_ >-> cast

let minDistance_ =  TerminationConditionCollision.MinDistance_

let detectorSize_ = TerminationConditionCollision.DetectorSize_

let secondaryDetector_ = TerminationConditionCollision.SecondaryDetector_

let primaryDetector_ = TerminationConditionCollision.PrimaryDetector_

let layers_ = TerminationConditionCollision.Layers_

let selfIntersection_ = TerminationConditionCollision.SelfIntersection_

let ignoreParent_ = TerminationConditionCollision.IgnoreParent_

// TerminationConditionCurvature
let maxCurvature_ = TerminationConditionCurvature.MaxCurvature_

// TerminationConditions
// distance_ - generic
let collision_ = TerminationConditions.Collision_

// BrushStrategySolid
let cap_ = BrushStrategySolid.Cap_
let alignWithParent_ = BrushStrategySolid.AlignWithParent_ 

// BrushStrategySolidJoint
let jointType_ = BrushStrategySolidJoint.JointType_

// ColliderStrategyCircles
// offset - generic
let density_ = ColliderStrategyCircles.Density_

// ColliderStrategySegments
let minLength_ = ColliderStrategySegments.MinLength_

// ColliderOptionsGeneral
let strategy_<'a when 'a :> ColliderStrategy> : Lens<ColliderOptionsGeneral, 'a> = 
    ColliderOptionsGeneral.Strategy_ >-> cast

let layer_ = ColliderOptionsGeneral.Layer_

// AgentSpeedConstant
let magnitude_ = AgentSpeedConstant.Magnitude_

// BranchingCriterionBased
// spawnConfig_ is generic
let criterion_<'a when 'a :> BranchingCriterion> : Lens<BranchingCriterionBased, 'a> = 
    BranchingCriterionBased.Criterion_ >-> cast


// generic lenses
let inline a_< ^U , ^V when ^U : (static member A_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member A_ : Lens< ^U, ^V>) ()))

let inline b_< ^U , ^V when ^U : (static member B_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member B_ : Lens< ^U, ^V>) ()))

let inline c_< ^U , ^V when ^U : (static member C_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member C_ : Lens< ^U, ^V>) ()))
    
let inline d_< ^U , ^V when ^U : (static member D_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member D_ : Lens< ^U, ^V>) ()))
        
let inline n_< ^U , ^V when ^U : (static member N_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member N_ : Lens< ^U, ^V>) ()))

let inline nx_< ^U , ^V when ^U : (static member Nx_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Nx_ : Lens< ^U, ^V>) ()))
    
let inline ny_< ^U , ^V when ^U : (static member Ny_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Ny_ : Lens< ^U, ^V>) ()))

let inline distance_< ^U , ^V when ^U : (static member Distance_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Distance_ : Lens< ^U, ^V>) ()))

let inline angle_< ^U , ^V when ^U : (static member Angle_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Angle_ : Lens< ^U, ^V>) ()))

let inline alpha_< ^U , ^V when ^U : (static member Alpha_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Alpha_ : Lens< ^U, ^V>) ()))

let inline offset_< ^U , ^V when ^U : (static member Offset_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Offset_ : Lens< ^U, ^V>) ()))

let inline factor_< ^U , ^V when ^U : (static member Factor_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Factor_ : Lens< ^U, ^V>) ()))

let inline config_< ^U , ^V when ^U : (static member Config_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Config_ : Lens< ^U, ^V>) ()))

let inline spawnConfig_< ^U , ^V when ^U : (static member SpawnConfig_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member SpawnConfig_ : Lens< ^U, ^V>) ()))

let inline groupConfig_< ^U , ^V when ^U : (static member GroupConfig_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member GroupConfig_ : Lens< ^U, ^V>) ()))

let inline terminalConfig_< ^U , ^V when ^U : (static member TerminalConfig_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member TerminalConfig_ : Lens< ^U, ^V>) ()))

let inline gap_< ^U , ^V when ^U : (static member Gap_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Gap_ : Lens< ^U, ^V>) ()))

let inline angles_< ^U , ^V when ^U : (static member Angles_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Angles_ : Lens< ^U, ^V>) ()))

let inline branchProb_< ^U , ^V when ^U : (static member BranchProb_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member BranchProb_ : Lens< ^U, ^V>) ()))

let inline colliderSize_< ^U , ^V when ^U : (static member ColliderSize_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member ColliderSize_ : Lens< ^U, ^V>) ()))

let inline hollowPart_< ^U , ^V when ^U : (static member HollowPart_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member HollowPart_ : Lens< ^U, ^V>) ()))

let inline octagonal_< ^U , ^V when ^U : (static member Octagonal_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Octagonal_ : Lens< ^U, ^V>) ()))

let inline appearance_< ^U , ^V when ^U : (static member Appearance_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Appearance_ : Lens< ^U, ^V>) ()))
    
let inline width_< ^U , ^V when ^U : (static member Width_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Width_ : Lens< ^U, ^V>) ()))
    
let inline flip_< ^U , ^V when ^U : (static member Flip_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Flip_ : Lens< ^U, ^V>) ()))
    
let inline startWithVias_< ^U , ^V when ^U : (static member StartWithVias_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member StartWithVias_ : Lens< ^U, ^V>) ()))
    
let inline lengthLimit_< ^U , ^V when ^U : (static member LengthLimit_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member LengthLimit_ : Lens< ^U, ^V>) ()))
    
let inline spawnCollider_< ^U , ^V when ^U : (static member SpawnCollider_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member SpawnCollider_ : Lens< ^U, ^V>) ()))
    
let inline recursionLimit_< ^U , ^V when ^U : (static member RecursionLimit_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member RecursionLimit_ : Lens< ^U, ^V>) ()))
    
let inline radius_< ^U , ^V when ^U : (static member Radius_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Radius_ : Lens< ^U, ^V>) ()))

let inline p_< ^U , ^V when ^U : (static member P_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member P_ : Lens< ^U, ^V>) ()))

let inline minInterval_< ^U , ^V when ^U : (static member MinInterval_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member MinInterval_ : Lens< ^U, ^V>) ()))

let inline genLimit_< ^U , ^V when ^U : (static member GenLimit_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member GenLimit_ : Lens< ^U, ^V>) ()))

let inline lengthRange_< ^U , ^V when ^U : (static member LengthRange_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member LengthRange_ : Lens< ^U, ^V>) ()))

let inline lengthSteps_< ^U , ^V when ^U : (static member LengthSteps_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member LengthSteps_ : Lens< ^U, ^V>) ()))

let inline base_< ^U , ^V when ^U : (static member Base_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Base_ : Lens< ^U, ^V>) ()))

let inline minT_< ^U , ^V when ^U : (static member MinT_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member MinT_ : Lens< ^U, ^V>) ()))

let inline intervalRange_< ^U , ^V when ^U : (static member IntervalRange_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member IntervalRange_ : Lens< ^U, ^V>) ()))

let inline midpoint_< ^U , ^V when ^U : (static member Midpoint_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Midpoint_ : Lens< ^U, ^V>) ()))

let inline scaleRange_< ^U , ^V when ^U : (static member ScaleRange_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member ScaleRange_ : Lens< ^U, ^V>) ()))

let inline maxLength_< ^U , ^V when ^U : (static member MaxLength_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member MaxLength_ : Lens< ^U, ^V>) ()))

let inline baseSpeed_< ^U , ^V when ^U : (static member BaseSpeed_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member BaseSpeed_ : Lens< ^U, ^V>) ()))

let inline delay_< ^U , ^V when ^U : (static member Delay_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Delay_ : Lens< ^U, ^V>) ()))

let inline scale_< ^U , ^V when ^U : (static member Scale_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Scale_ : Lens< ^U, ^V>) ()))

let inline start_< ^U , ^V when ^U : (static member Start_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Start_ : Lens< ^U, ^V>) ()))

let inline agentConfig_< ^U , ^V when ^U : (static member AgentConfig_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member AgentConfig_ : Lens< ^U, ^V>) ()))

let inline alphaRange_< ^U , ^V when ^U : (static member AlphaRange_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
     makeGenericLens (fun (_ : ^U) -> (^U : (static member AlphaRange_ : Lens< ^U, ^V>) ()))
 

// compound lenses
let termination_spawnConfig_ = termination_ >-> cast<TerminationStrategy, TerminationSpawn> >-> spawnConfig_