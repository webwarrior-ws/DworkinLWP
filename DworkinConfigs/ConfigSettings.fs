module ConfigSettings

open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Lenses

open Settings

type EnvironmentConfig = LibDworkin.Configs.DworkinEnvironmentConfig


type PCBAngles =
    | A45 = 1
    | A60 = 2
    | A45_90 = 3
    | A30_45_90 = 4
    | A30_60_90 = 5
    | A45_60_75_90 = 6

module PCBAngles =
    let GetAngles =
        function
        | PCBAngles.A45 -> [| -45.0f |]
        | PCBAngles.A60 -> [| -60.0f |]
        | PCBAngles.A45_90 -> [| -45.0f; -90.0f |]
        | PCBAngles.A30_45_90 -> [| -30.0f; -45.0f; -90.0f |]
        | PCBAngles.A30_60_90 -> [| -30.0f; -60.0f; -90.0f |]
        | PCBAngles.A45_60_75_90 -> [| -45.0f; -60.0f; -75.0f; -90.0f |]


type PCBChipShape =
    | Rectangular = 1
    | Octagonal = 2


type PCB_RandomPosChips() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()
    static do Settings.RegisterClass<PCB_RandomPosChips>()

    [<FloatRange(0.0f, 90.0f, Step=45.0f)>]
    member val Angle = Defined(0.0f) with get, set

    [<Enum([| PCBAngles.A45; PCBAngles.A60; PCBAngles.A45_90; PCBAngles.A30_45_90 |], ItemType=typeof<PCBAngles>)>]
    member val Angles = Defined(PCBAngles.A45) with get, set

    [<Enum([| PCBChipShape.Octagonal; PCBChipShape.Rectangular |], ItemType=typeof<PCBChipShape>)>]
    member val ChipsShape = Defined(PCBChipShape.Octagonal) with get, set
    
    [<Bool>]
    member val Vias = Defined(false) with get, set

    [<Enum([| PCB.Endpoints.Hollow; PCB.Endpoints.Filled |], ItemType=typeof<PCB.Endpoints>)>]
    member val Endpoints = Defined(PCB.Endpoints.Hollow) with get, set
    
    [<FloatRange(0.75f, 1.5f)>]
    member val Size = Defined(1.0f) with get, set

    [<FloatRange(0.2f, 0.8f, Step=0.05f)>]
    member val Density = Defined(0.5f) with get, set

    [<FloatRange(0.0f, 1.0f, Step=0.1f)>]
    member val ThicknessVariance = Defined(0.0f) with get, set

    override this.Build(seed) =
        let random = System.Random(seed)
        let getValue expr = Randomizeable.getValue random expr
        let isOctagonal = match getValue<@ this.ChipsShape @> with | PCBChipShape.Octagonal -> true | _ -> false
        PCB.PCB_RandomPosChips(
            RandomSeed=seed,
            Angle=getValue <@ this.Angle @>,
            Angles=PCBAngles.GetAngles(getValue<@ this.Angles @>),
            Octagonal=isOctagonal,
            Size=getValue <@ this.Size @>,
            Vias=getValue <@ this.Vias @>,
            Endpoints=getValue <@ this.Endpoints @>,
            Density=getValue <@ this.Density @>,
            ThicknessVariance=getValue<@ this.ThicknessVariance @>) 
        :> EnvironmentConfig



// Maze
type MazeAngles =
    | Square = 1
    | FortyFive = 2
    | FortyFiveSharp = 3
    | Hexagonal = 4
    | Qbert = 5
    | Arbitrary = 6
    | Arbitrary2 = 7


type Maze() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()
    static do Settings.RegisterClass<Maze>()

    [<Enum([| MazeAngles.Square; MazeAngles.FortyFive; MazeAngles.FortyFiveSharp; MazeAngles.Hexagonal; MazeAngles.Qbert; MazeAngles.Arbitrary; MazeAngles.Arbitrary2 |], 
           ItemType=typeof<MazeAngles>)>]
    member val Angles = Defined(MazeAngles.Square) with get, set

    [<FloatRange(0.5f, 2.0f)>]
    member val WallThickness = Defined(1.0f) with get, set

    [<FloatRange(0.0f, 0.5f)>]
    member val ThicknessVariance = Defined(0.0f) with get, set

    [<Bool>]
    member val Curvy = Defined(false) with get, set

    [<Enum([| 0; 45 |])>]
    member val Orientation = Defined(0) with get, set

    [<FloatRange(0.5f, 1.5f)>]
    member val Scale = Defined(1.0f) with get, set

    override this.Build(seed) =
        let random = System.Random(seed)
        let getValue expr = Randomizeable.getValue random expr

        let agentConfig, gridSize, angleIncr =
            match getValue <@ this.Angles @> with
            | MazeAngles.Square -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v2, 60, 90
            | MazeAngles.FortyFive -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v3, 42, 45
            | MazeAngles.FortyFiveSharp -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v1, 42, 45
            | MazeAngles.Hexagonal -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v4, 52, 60
            | MazeAngles.Qbert -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v5, 52, 60
            | MazeAngles.Arbitrary -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v6, 15, 60
            | MazeAngles.Arbitrary2 -> Maze.DworkinAgentConfig.straightMazeRandomAngle_v7, 30, 60

        let mutable agentConfig = 
            agentConfig 
            |> Optic.set (speed_ >-> magnitude_) 3.0f
            |> Optic.map (thickness_ >-> cast<_, ThicknessFormula.Constant> >-> c_) (fun c -> c * (getValue <@ this.WallThickness @>))
        if getValue <@ this.Curvy @> then
            agentConfig <- agentConfig |> Optic.set (curvature_) (CurvatureFormula.SNoise(A=0.075f, B=5.0f, D=0.61f))

        let config =
            Maze.LWPMaze3(
                Scale=getValue <@ this.Scale @>,
                InitialAngle=getValue <@ this.Orientation @>,
                ThicknessVariance= getValue <@ this.ThicknessVariance @>,
                AgentConfig=agentConfig,
                GridSize=float32(gridSize),
                AngleIncrement=angleIncr,
                RandomSeed=seed) 

        config :> EnvironmentConfig


type FlowingLeaves() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()
    static do Settings.RegisterClass<FlowingLeaves>()

    [<FloatRange(1.5f, 3.0f, Step=0.05f)>]
    member val Scale = Defined(2.0f) with get, set

    [<FloatRange(0.5f, 1.5f, Step=0.05f)>]
    member val Density = Defined(1.0f) with get, set

    [<FloatRange(0.0f, 0.75f, Step=0.05f)>]
    member val ScaleVariance = Defined(0.0f) with get, set
    
    [<FloatRange(0.0f, 2.0f, Step=0.1f)>]
    member val Curviness = Defined(0.0f) with get, set
    
    [<FloatRange(2.5f, 10.0f, Step=0.5f)>]
    member val LeafThickness = Defined(5.0f) with get, set
    
    [<FloatRange(40.0f, 90.0f, Step=5.0f)>]
    member val LeafLength = Defined(50.0f) with get, set
    
    [<FloatRange(0.5f, 0.8f, Step=0.05f)>]
    member val LeafShape = Defined(0.5f) with get, set

    override this.Build(seed) =
        let random = System.Random(seed)
        let getValue expr = Randomizeable.getValue random expr
        FlowingLeaves.FlowingLeaves_WIP(
            RandomSeed=seed,
            Scale=getValue <@ this.Scale @>,
            Density=(getValue <@ this.Density @>),
            ScaleVariance=getValue <@ this.ScaleVariance @>,
            Curviness=getValue <@ this.Curviness @>,
            LeafThickness=getValue <@ this.LeafThickness @>,
            LeafLength=getValue <@ this.LeafLength @>,
            LeafShape=getValue <@ this.LeafShape @>,
            Speed=1.0f) 
        :> EnvironmentConfig


// Vine
type VineStartPos =
    | Left = 1
    | Right = 2
    | Bottom = 3
    | Top = 4
    | BottomLeft = 5
    | TopRight = 6
    | LeftAndRight = 7
    | BottomAndTop = 8

type VineType =
    | Single = 1
    | Curly1 = 2
    | Curly2 = 3
    | Curly3 = 4
    | Splitting = 5

type VineLeafType =
    | None = 1
    | IndAlt = 2
    | IndByScale = 3
    | WithStem = 4
    | SlightlyCurved = 6
    | Rounded = 8
    | Straight1 = 9
    | Curved1 = 10

type Vine() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()
    static do Settings.RegisterClass<Vine>()

    [<Enum([| VineStartPos.Bottom; VineStartPos.Top; VineStartPos.Left; VineStartPos.Right; 
              VineStartPos.BottomLeft; VineStartPos.TopRight; VineStartPos.LeftAndRight; VineStartPos.BottomAndTop |],
           ItemType=typeof<VineStartPos>)>]
    member val StartingPositions = Defined(VineStartPos.Bottom) with get, set

    [<Enum([| VineType.Single; VineType.Curly1; VineType.Curly2; VineType.Curly3; VineType.Splitting |], 
           ItemType=typeof<VineType>)>]
    member val VineType = Defined(VineType.Single) with get, set

    [<Enum([| VineLeafType.None; VineLeafType.IndAlt; VineLeafType.IndByScale; VineLeafType.WithStem; 
              VineLeafType.SlightlyCurved; VineLeafType.Rounded; VineLeafType.Straight1; VineLeafType.Curved1 |],
           ItemType=typeof<VineLeafType>)>]
    member val Leaves = Defined(VineLeafType.None) with get, set
    
    [<FloatRange(1.5f, 2.5f, Step=0.1f)>]
    member val Scale = Defined(1.5f) with get, set

    override this.Build(seed) = 
        let random = System.Random(seed)
        let getValue expr = Randomizeable.getValue random expr

        let startPos = getValue <@ this.StartingPositions @>
        let vineType = getValue <@ this.VineType @>

        let sides =
            let empty = Vine.InitialPositions.None
            match startPos with
            | VineStartPos.Left -> { empty with Left=true }
            | VineStartPos.Right -> { empty with Right=true }
            | VineStartPos.Bottom -> { empty with Bottom=true }
            | VineStartPos.Top -> { empty with Top=true }
            | VineStartPos.BottomLeft -> { empty with Bottom=true; Left=true }
            | VineStartPos.TopRight -> { empty with Top=true; Right=true }
            | VineStartPos.LeftAndRight -> { empty with Left=true; Right=true }
            | VineStartPos.BottomAndTop -> { empty with Bottom=true; Top=true }

        let corner = 
            match startPos with
            | VineStartPos.BottomLeft | VineStartPos.TopRight -> true
            | _ -> false

        let angle : int =
            match vineType with
            | VineType.Single ->
                match startPos with
                | VineStartPos.Left | VineStartPos.Right | VineStartPos.LeftAndRight -> 90
                | VineStartPos.BottomLeft | VineStartPos.TopRight -> 40
                | _ -> 0
            | VineType.Curly1 ->
                match startPos with
                | VineStartPos.Left | VineStartPos.Right -> 90
                | VineStartPos.Top | VineStartPos.Bottom -> 0
                | VineStartPos.LeftAndRight -> 75
                | VineStartPos.BottomAndTop -> 5
                | _ -> 35
            | VineType.Curly2 ->
                match startPos with
                | VineStartPos.Left | VineStartPos.Right -> -75
                | VineStartPos.Top | VineStartPos.Bottom -> 45
                | VineStartPos.LeftAndRight -> -75
                | VineStartPos.BottomAndTop -> 45
                | _ -> 90
            | VineType.Curly3 ->
                match startPos with
                | VineStartPos.Left | VineStartPos.Right -> -45
                | VineStartPos.Top | VineStartPos.Bottom -> 45
                | VineStartPos.LeftAndRight -> -45
                | VineStartPos.BottomAndTop -> 45
                | _ -> 90
            | VineType.Splitting ->
                match startPos with
                | VineStartPos.Left | VineStartPos.Right -> -80
                | VineStartPos.Top | VineStartPos.Bottom | VineStartPos.BottomAndTop -> 15
                | VineStartPos.LeftAndRight -> 70
                | _ -> 55
        
        let vineConfig = 
            match vineType with
            | VineType.Single -> Vine.DworkinAgentConfig.singleVineVarying
            | VineType.Curly1 -> Vine.DworkinAgentConfig.vine1
            | VineType.Curly2 -> Vine.DworkinAgentConfig.vine2
            | VineType.Curly3 -> Vine.DworkinAgentConfig.vine3
            | VineType.Splitting -> Vine.DworkinAgentConfig.vineSplitting1

        let alpha =
            match startPos with
            | VineStartPos.BottomLeft | VineStartPos.BottomLeft -> 3.0f
            | _ -> 3.8f

        let leafStrategies : LibDworkin.Agents.BranchingStrategy[] =
            match getValue <@ this.Leaves @> with
            | VineLeafType.None -> [|  |]
            | VineLeafType.IndAlt -> [| Vine.BranchingCustom._2BranchIndLeafAlt_vines() |]
            | VineLeafType.IndByScale -> [| Vine.BranchingCustom._2BranchIndLeafByScale_vines() |]
            | VineLeafType.WithStem -> [| Vine.BranchingCustom._2BranchIndLeafWithStem_vines |]
            | VineLeafType.SlightlyCurved -> [| Vine.BranchingCriterionBased.leaves2SidesProbes1_2_vines |]
            | VineLeafType.Rounded -> [| Vine.BranchingCriterionBased.leaves2SidesProbes2_vines |]
            | VineLeafType.Straight1  -> [| Vine.BranchingCriterionBased.leaves2SidesProbesStraigh1_vines |]
            | VineLeafType.Curved1  -> [| Vine.BranchingCriterionBased.leavesProbesCurved1_vines |]
        
        Vine.Vines(AgentConfig=vineConfig, Alpha=alpha, Angle=angle, Sides=sides, Corners=corner, 
                   Scale=getValue <@ this.Scale @>, Speed=1.0f) 
            |> Optic.set (Vine.Vines.LeafStrategy_ >-> Vine.DworkinEnvironmentConfigCustom.Vines.LeafStrategy.Strategies_) leafStrategies
            :> LibDworkin.Configs.DworkinEnvironmentConfig


// Floral Swirls
type FloralSwirlsPrimary =
    | WavySingle = 1
    | WavyDouble = 2
    | SShapedSingle = 3
    | SShapedSpacefill = 4
    | MagneticSingle = 5
    | MagneticBunch = 6

type FloralSwirlsSecondary =
    | ShiftedSwirls = 1
    | RecursiveSpirals = 2
    | MagneticSpiralsDense = 3
    | PlantLike = 4

type FloralSwirlsLeaves =
    | NoLeaves = 1
    | Straight = 2 //leaves2SidesProbesStraight1_fs
    | SlightlyCurved = 3 //leaves2SidesProbes1_2_fs
    | Curved = 4 //leavesProbesCurved1_fs
    | Teardrop = 5 //leavesProbesFollowCurv_fs
    | WithStem = 6 //_2BranchIndependentLeafWithStem_fs
    | ScaledToBranchThickness = 7 //_2BranchIndLeafByScale_fs

let (|FloralSwirlsWavy|FloralSwirlsSShaped|FloralSwirlsMagnetic|) floralSwirlsType =
    match floralSwirlsType with
    | FloralSwirlsPrimary.WavySingle | FloralSwirlsPrimary.WavyDouble -> FloralSwirlsWavy
    | FloralSwirlsPrimary.SShapedSingle | FloralSwirlsPrimary.SShapedSpacefill -> FloralSwirlsSShaped
    | FloralSwirlsPrimary.MagneticSingle | FloralSwirlsPrimary.MagneticBunch -> FloralSwirlsMagnetic

let inline thickness_< ^U , ^V when ^U : (static member Thickness_ : Lens< ^U, ^V>)> : Lens< ^U, ^V> = 
    makeGenericLens (fun (_ : ^U) -> (^U : (static member Thickness_ : Lens< ^U, ^V>) ())) 

type FloralSwirls() =
    inherit LibDworkin.DworkinSettings.EnvironmentSettings()
    static do Settings.RegisterClass<FloralSwirls>()

    [<Enum([| FloralSwirlsPrimary.WavySingle; FloralSwirlsPrimary.WavyDouble; FloralSwirlsPrimary.SShapedSingle; 
              FloralSwirlsPrimary.SShapedSpacefill; FloralSwirlsPrimary.MagneticSingle; FloralSwirlsPrimary.MagneticBunch |], 
           ItemType=typeof<FloralSwirlsPrimary>)>]
    member val PrimarySwirls = Defined(FloralSwirlsPrimary.WavySingle) with get, set

    [<Enum([| FloralSwirlsSecondary.ShiftedSwirls; FloralSwirlsSecondary.RecursiveSpirals; 
              FloralSwirlsSecondary.MagneticSpiralsDense; FloralSwirlsSecondary.PlantLike |],
           ItemType=typeof<FloralSwirlsSecondary>)>]
    member val SecondarySwirls = Defined(FloralSwirlsSecondary.ShiftedSwirls) with get, set

    [<Enum([| FloralSwirlsLeaves.NoLeaves; FloralSwirlsLeaves.Straight; FloralSwirlsLeaves.SlightlyCurved; FloralSwirlsLeaves.Curved;
              FloralSwirlsLeaves.Teardrop; FloralSwirlsLeaves.WithStem; FloralSwirlsLeaves.ScaledToBranchThickness |],
           ItemType=typeof<FloralSwirlsLeaves>)>]
    member val Leaves = Defined(FloralSwirlsLeaves.NoLeaves) with get, set

    [<Bool>]
    member val LeavesOnStem = Defined(false) with get, set

    [<FloatRange(1.0f, 2.0f)>]
    member val Thickness = Defined(1.0f) with get, set

    override this.Build(seed) = 
        let random = System.Random(seed)
        let getValue expr = Randomizeable.getValue random expr

        let primarySwirlsType = getValue <@ this.PrimarySwirls @>
        let leavesOnStem = getValue <@ this.LeavesOnStem @>
        let thickness = getValue <@ this.Thickness @>

        let largeSwirlsStrats =
            match getValue <@ this.SecondarySwirls @> with
            | FloralSwirlsSecondary.ShiftedSwirls -> 
                match primarySwirlsType with
                | FloralSwirlsWavy -> FloralSwirls.LargeSwirlsStrategies.swirlsShifted4Cosine
                | FloralSwirlsSShaped -> FloralSwirls.LargeSwirlsStrategies.swirlsShifted4S
                | FloralSwirlsMagnetic -> FloralSwirls.LargeSwirlsStrategies.swirlsShifted4Magnetic
            | FloralSwirlsSecondary.RecursiveSpirals ->
                match primarySwirlsType with
                | FloralSwirlsWavy -> FloralSwirls.LargeSwirlsStrategies.swirlsRecursive4Cosine
                | FloralSwirlsSShaped -> FloralSwirls.LargeSwirlsStrategies.swirlsRecursive4S
                | FloralSwirlsMagnetic -> FloralSwirls.LargeSwirlsStrategies.swirlsRecursive4Magnetic
            | FloralSwirlsSecondary.MagneticSpiralsDense -> FloralSwirls.LargeSwirlsStrategies.swirlsMagneticMany
            | FloralSwirlsSecondary.PlantLike ->
                match primarySwirlsType with
                | FloralSwirlsWavy -> FloralSwirls.LargeSwirlsStrategies.plantLike4Cosine
                | FloralSwirlsSShaped -> FloralSwirls.LargeSwirlsStrategies.plantLike4S
                | FloralSwirlsMagnetic -> FloralSwirls.LargeSwirlsStrategies.plantLike4Magnetic

        let leavesStrat =
            let levels = LibDworkin.SpecializedCollections.NumberRange((if leavesOnStem then 1.0f else 2.0f), 10.0f)
            let inline prepareCBStrat strat = 
                strat 
                |> Optic.set 
                    (spawnConfig_ >-> cast<_, FloralSwirls.BranchingCriterionBased.SpawnConfigWithLevels> >-> levels_) 
                    levels
                :> LibDworkin.Agents.BranchingStrategy
            let inline prepareCustomStrat strat =
                strat 
                |> Optic.set levels_ levels
                :> LibDworkin.Agents.BranchingStrategy
            match getValue <@ this.Leaves @> with
            | FloralSwirlsLeaves.NoLeaves -> LibDworkin.Configs.BranchingNone() :> LibDworkin.Agents.BranchingStrategy
            | FloralSwirlsLeaves.Straight -> FloralSwirls.LeafStrategies.leaves2SidesProbesStraight1_fs |> prepareCBStrat
            | FloralSwirlsLeaves.SlightlyCurved -> 
                FloralSwirls.LeafStrategies.leaves2SidesProbes1_2_fs 
                |> Optic.map 
                    (spawnConfig_ >-> cast<_, FloralSwirls.BranchingCriterionBased.Leaves2SidesProbesStraight1_fs.SpawnConfig> >-> scaleRange_) 
                    (fun x -> x * sqrt(thickness))
                |> Optic.map 
                    (criterion_<BranchingCriteria.BranchingCriterionInterval.ConstantBoundedT> >-> d_) 
                    (fun x -> x * sqrt(thickness))
                |> prepareCBStrat
            | FloralSwirlsLeaves.Curved -> FloralSwirls.LeafStrategies.leavesProbesCurved1_fs |> prepareCBStrat
            | FloralSwirlsLeaves.Teardrop -> 
                FloralSwirls.LeafStrategies.leavesProbesFollowCurv_fs 
                |> Optic.map 
                    (spawnConfig_ >-> cast<_, FloralSwirls.BranchingCriterionBased.LeavesProbesFollowCurv_fs.SpawnConfig> 
                     >-> config_ 
                     >-> thickness_ >-> cast<_, ThicknessFormula.LeafPower_Base> 
                     >-> c_) 
                    (fun x -> x / sqrt(thickness))
                |> prepareCBStrat
            | FloralSwirlsLeaves.WithStem -> 
                FloralSwirls.LeafStrategies._2BranchIndependentLeafWithStem_fs() 
                |> Optic.map 
                    (config_ >-> config_ >-> thickness_ >-> cast<_, ThicknessFormula.LeafPower_Base> >-> c_) 
                    (fun x -> x / (thickness ** 0.67f))
                |> prepareCustomStrat
            | FloralSwirlsLeaves.ScaledToBranchThickness -> FloralSwirls.LeafStrategies._2BranchIndLeafByScale_fs() |> prepareCustomStrat

        let inline prepare envConfig =
            envConfig
            |> Optic.set largeSwirls_ largeSwirlsStrats
            |> Optic.set leafStrategy_ leavesStrat
            |> Optic.set thickness_ thickness
            :> LibDworkin.Configs.DworkinEnvironmentConfig

        match primarySwirlsType with
        | FloralSwirlsPrimary.WavySingle -> FloralSwirls.CosineSwirl(IsDouble=false) |> prepare
        | FloralSwirlsPrimary.WavyDouble -> FloralSwirls.CosineSwirl(IsDouble=true) |> prepare
        | FloralSwirlsPrimary.SShapedSingle -> FloralSwirls.S_ShapedSwirlSingle() |> prepare
        | FloralSwirlsPrimary.SShapedSpacefill -> FloralSwirls.S_ShapedSwirlSpacefill() |> prepare
        | FloralSwirlsPrimary.MagneticSingle -> FloralSwirls.magneticSpiralSingle |> prepare
        | FloralSwirlsPrimary.MagneticBunch -> FloralSwirls.magneticSpiralsBunch |> prepare



let allEnvironmentSettingsTypes = 
    [| 
        typeof<PCB_RandomPosChips>; 
        typeof<Maze>; 
        typeof<FlowingLeaves>; 
        typeof<Vine>;
        typeof<FloralSwirls>;
    |]
