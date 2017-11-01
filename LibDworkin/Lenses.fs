module LibDworkin.Lenses

open Aether
open Aether.Operators

open Agents
open Configs

// utility functions: 

/// Isomorphism between two types. Unfortunately it's not possible to add subtype parameter constraint, so use cautiously.
let cast<'a, 'b> : Isomorphism<'a, 'b> =
    ((fun object -> box object :?> 'b), (fun object -> box object :?> 'a))

/// utility function for defining generic lenses using static type constraints
let makeGenericLens lensGetter =
    (fun object -> (lensGetter object) |> fst <| object), (fun value object -> (lensGetter object) |> snd <| value <| object)

// that's how you make generic (duck-typed) lens:
//let inline c_< ^U , ^V when ^U : (static member C_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
//    makeGenericLens (fun (_ : ^U) -> (^U : (static member C_ : Lens< ^U, ^V>) ()))


// lenses for configs:

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

// 


// generic lenses
let inline distance_< ^U , ^V when ^U : (static member Distance_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Distance_ : Lens< ^U, ^V>) ()))

let inline angle_< ^U , ^V when ^U : (static member Angle_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> =
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Angle_ : Lens< ^U, ^V>) ()))
