namespace DworkinUI


open Settings
open LibDworkin.DworkinSettings
open LibDworkin.RendererComponents
open DworkinConfigs



module CustomEnumEditors =
    let RegisterCustomEditors = lazy(
        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.BevelType, 
            Views.RandomizedItemChooser<BevelMode>.GetFactoryForMapping(
                (function 
                 | BevelMode.LevelDown -> { ImageID=Resource.Drawable.bevelmode_level_down; Label="" }
                 | BevelMode.LevelUp -> { ImageID=Resource.Drawable.bevelmode_level_up; Label="" }
                 | BevelMode.DownLevel -> { ImageID=Resource.Drawable.bevelmode_down_level; Label="" }
                 | BevelMode.DownUp -> { ImageID=Resource.Drawable.bevelmode_down_up; Label="" }
                 | BevelMode.UpLevel -> { ImageID=Resource.Drawable.bevelmode_up_level; Label="" }
                 | BevelMode.UpDown -> { ImageID=Resource.Drawable.bevelmode_up_down; Label="" }
                 | x -> failwithf "bevel mode %A not supported" x ),
                numColumns=3) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.BevelProfile, 
            Views.RandomizedItemChooser<BevelProfile>.GetFactoryForMapping(
                (function 
                 | BevelProfile.Linear -> { ImageID=Resource.Drawable.bevelprofile_linear; Label="Linear" }
                 | BevelProfile.Circular -> { ImageID=Resource.Drawable.bevelprofile_circular; Label="Circular" }
                 | BevelProfile.InverseCircular -> { ImageID=Resource.Drawable.bevelprofile_inverse_circular; Label="Inverse circular" }
                 | BevelProfile.Sigmoid -> { ImageID=Resource.Drawable.bevelprofile_sigmoid; Label="S-shaped" }
                 | x -> failwithf "bevel profile %A not supported" x ),
                numColumns=2) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.GlowInsideType, 
            Views.RandomizedSpinnerWithImages<GlowInsideType>.GetFactoryForMapping(
                function 
                | GlowInsideType.Empty -> { ImageID=Resource.Drawable.glow_empty; Label="Empty" }
                | GlowInsideType.Glow -> { ImageID=Resource.Drawable.glow_glow; Label="Glow" }
                | GlowInsideType.Foreground -> { ImageID=Resource.Drawable.glow_foreground; Label="Foreground" }
                | x -> failwithf "bevel profile %A not supported" x ) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.Angle, 
            Views.RandomizedSpinner<int>.GetFactoryForLabelingFunction(sprintf "%d°")
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.PCBTraceAngles, 
            Views.RandomizedSpinner<ConfigSettings.PCBAngles>.GetFactoryForLabelingFunction(
                fun item ->
                    let anglesString = ConfigSettings.PCBAngles.GetAngles item |> Array.map (int >> abs >> sprintf "%d°")
                    System.String.Join(", ", anglesString))
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.PCBTracesKind, 
            Views.RandomizedSpinner<ConfigSettings.PCBTracesKind>.GetFactoryForLabelingFunction(
                function
                | ConfigSettings.PCBTracesKind.GridWithBranches -> "Grid (with branches)"
                | x -> x.ToString() )
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.MazeAngles, 
            Views.RandomizedItemChooser<ConfigSettings.MazeAngles>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.MazeAngles.Square -> { ImageID=Resource.Drawable.maze_square; Label="" }
                 | ConfigSettings.MazeAngles.FortyFive -> { ImageID=Resource.Drawable.maze_fortyfive; Label="" }
                 | ConfigSettings.MazeAngles.FortyFiveSharp -> { ImageID=Resource.Drawable.maze_fortyfive_sharp; Label="" }
                 | ConfigSettings.MazeAngles.Hexagonal -> { ImageID=Resource.Drawable.maze_hexagonal; Label="" }
                 | ConfigSettings.MazeAngles.Qbert -> { ImageID=Resource.Drawable.maze_qbert; Label="" }
                 | ConfigSettings.MazeAngles.Arbitrary -> { ImageID=Resource.Drawable.maze_arbitrary1; Label="" }
                 | ConfigSettings.MazeAngles.Arbitrary2 -> { ImageID=Resource.Drawable.maze_arbitrary2; Label="" }
                 | x -> failwithf "maze angles %A not supported" x),
                numColumns=2) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.VineStartPos, 
            Views.RandomizedItemChooser<ConfigSettings.VineStartPos>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.VineStartPos.Left -> { ImageID=Resource.Drawable.vinestartpos_left; Label="Left" }
                 | ConfigSettings.VineStartPos.Right -> { ImageID=Resource.Drawable.vinestartpos_right; Label="Right" }
                 | ConfigSettings.VineStartPos.Bottom -> { ImageID=Resource.Drawable.vinestartpos_bottom; Label="Bottom" }
                 | ConfigSettings.VineStartPos.Top -> { ImageID=Resource.Drawable.vinestartpos_top; Label="Top" }
                 | ConfigSettings.VineStartPos.BottomAndTop -> { ImageID=Resource.Drawable.vinestartpos_bottom_top; Label="Bottom & top" }
                 | ConfigSettings.VineStartPos.LeftAndRight -> { ImageID=Resource.Drawable.vinestartpos_left_right; Label="Left & right" }
                 | ConfigSettings.VineStartPos.BottomLeft -> { ImageID=Resource.Drawable.vinestartpos_bottom_left; Label="Bottom & left" }
                 | ConfigSettings.VineStartPos.TopRight -> { ImageID=Resource.Drawable.vinestartpos_top_right; Label="Top & right" }
                 | x -> failwithf "vine start position %A not supported" x),
                numColumns=4) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.VineType, 
            Views.RandomizedItemChooser<ConfigSettings.VineType>.GetFactoryForMapping(
                function 
                | ConfigSettings.VineType.Single -> { ImageID=Resource.Drawable.vinetype_single; Label="" }
                | ConfigSettings.VineType.Curly1 -> { ImageID=Resource.Drawable.vinetype_curly1; Label="" }
                | ConfigSettings.VineType.Curly2 -> { ImageID=Resource.Drawable.vinetype_curly2; Label="" }
                | ConfigSettings.VineType.Curly3 -> { ImageID=Resource.Drawable.vinetype_curly3; Label="" }
                | ConfigSettings.VineType.Splitting -> { ImageID=Resource.Drawable.vinetype_splitting; Label="" }
                | x -> failwithf "vine type %A not supported" x) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.VineLeafType, 
            Views.RandomizedItemChooser<ConfigSettings.VineLeafType>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.VineLeafType.None -> { ImageID=Resource.Drawable.leaf_noleaf; Label="" }
                 | ConfigSettings.VineLeafType.Curved1 -> { ImageID=Resource.Drawable.leaf_curved; Label="" }
                 | ConfigSettings.VineLeafType.IndAlt -> { ImageID=Resource.Drawable.leaf_indalt; Label="" }
                 | ConfigSettings.VineLeafType.IndByScale -> { ImageID=Resource.Drawable.leaf_indbyscale; Label="" }
                 | ConfigSettings.VineLeafType.Rounded -> { ImageID=Resource.Drawable.leaf_rounded; Label="" }
                 | ConfigSettings.VineLeafType.SlightlyCurved -> { ImageID=Resource.Drawable.leaf_slightlycurved; Label="" }
                 | ConfigSettings.VineLeafType.WithStem -> { ImageID=Resource.Drawable.leaf_withstem; Label="" }
                 | ConfigSettings.VineLeafType.Straight1 -> { ImageID=Resource.Drawable.leaf_straight; Label="" }
                 | x -> failwithf "vine leaf type %A not supported" x),
                numColumns=4) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.FloralSwirlsLeaves, 
            Views.RandomizedItemChooser<ConfigSettings.FloralSwirlsLeaves>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.FloralSwirlsLeaves.NoLeaves -> { ImageID=Resource.Drawable.leaf_noleaf; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.Curved -> { ImageID=Resource.Drawable.leaf_curved; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.ScaledToBranchThickness -> { ImageID=Resource.Drawable.leaf_indbyscale; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.SlightlyCurved -> { ImageID=Resource.Drawable.leaf_slightlycurved; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.WithStem -> { ImageID=Resource.Drawable.leaf_withstem; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.Straight -> { ImageID=Resource.Drawable.leaf_straight; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.Teardrop -> { ImageID=Resource.Drawable.leaf_teardrop; Label="" }
                 | x -> failwithf "leaf type %A not supported" x),
                numColumns=4) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.FloralSwirlsPrimary, 
            Views.RandomizedItemChooser<ConfigSettings.FloralSwirlsPrimary>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.FloralSwirlsPrimary.WavySingle -> { ImageID=Resource.Drawable.fsprimary_wavysingle; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.WavyDouble -> { ImageID=Resource.Drawable.fsprimary_wavydouble; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.MagneticSingle -> { ImageID=Resource.Drawable.fsprimary_magnetic_single; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.MagneticBunch -> { ImageID=Resource.Drawable.fsprimary_magnetic_bunch; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.SShapedSingle -> { ImageID=Resource.Drawable.fsprimary_s_single; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.SShapedSpacefill -> { ImageID=Resource.Drawable.fsprimary_s_spacefill; Label="" }
                 | x -> failwithf "floral swirls primary %A not supported" x),
                 numColumns=2) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.FloralSwirlsSecondary, 
            Views.RandomizedItemChooser<ConfigSettings.FloralSwirlsSecondary>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.FloralSwirlsSecondary.ShiftedSwirls -> { ImageID=Resource.Drawable.fssecondary_shiftedswirls; Label="" }
                 | ConfigSettings.FloralSwirlsSecondary.MagneticSpiralsDense -> { ImageID=Resource.Drawable.fssecondary_magneticspirals; Label="" }
                 | ConfigSettings.FloralSwirlsSecondary.PlantLike -> { ImageID=Resource.Drawable.fssecondary_plantlike; Label="" }
                 | ConfigSettings.FloralSwirlsSecondary.RecursiveSpirals -> { ImageID=Resource.Drawable.fssecondary_recursivespirals; Label="" }
                 | x -> failwithf "floral swirls secondary %A not supported" x),
                 numColumns=2) 
        )
    )