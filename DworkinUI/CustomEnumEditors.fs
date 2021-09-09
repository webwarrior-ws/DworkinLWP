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
                 | BevelMode.LevelDown -> { ImageID=Resources.Drawable.bevelmode_level_down; Label="" }
                 | BevelMode.LevelUp -> { ImageID=Resources.Drawable.bevelmode_level_up; Label="" }
                 | BevelMode.DownLevel -> { ImageID=Resources.Drawable.bevelmode_down_level; Label="" }
                 | BevelMode.DownUp -> { ImageID=Resources.Drawable.bevelmode_down_up; Label="" }
                 | BevelMode.UpLevel -> { ImageID=Resources.Drawable.bevelmode_up_level; Label="" }
                 | BevelMode.UpDown -> { ImageID=Resources.Drawable.bevelmode_up_down; Label="" }
                 | x -> failwithf "bevel mode %A not supported" x ),
                numColumns=3) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.BevelProfile, 
            Views.RandomizedItemChooser<BevelProfile>.GetFactoryForMapping(
                (function 
                 | BevelProfile.Linear -> { ImageID=Resources.Drawable.bevelprofile_linear; Label="Linear" }
                 | BevelProfile.Circular -> { ImageID=Resources.Drawable.bevelprofile_circular; Label="Circular" }
                 | BevelProfile.InverseCircular -> { ImageID=Resources.Drawable.bevelprofile_inverse_circular; Label="Inverse circular" }
                 | BevelProfile.Sigmoid -> { ImageID=Resources.Drawable.bevelprofile_sigmoid; Label="S-shaped" }
                 | x -> failwithf "bevel profile %A not supported" x ),
                numColumns=2) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.GlowInsideType, 
            Views.RandomizedSpinnerWithImages<GlowInsideType>.GetFactoryForMapping(
                function 
                | GlowInsideType.Empty -> { ImageID=Resources.Drawable.glow_empty; Label="Empty" }
                | GlowInsideType.Glow -> { ImageID=Resources.Drawable.glow_glow; Label="Glow" }
                | GlowInsideType.Foreground -> { ImageID=Resources.Drawable.glow_foreground; Label="Foreground" }
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
                    let anglesString = ConfigSettings.PCBAngles.GetAngles item |> Array.map (int >> sprintf "%d°")
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
                 | ConfigSettings.MazeAngles.Square -> { ImageID=Resources.Drawable.maze_square; Label="" }
                 | ConfigSettings.MazeAngles.FortyFive -> { ImageID=Resources.Drawable.maze_fortyfive; Label="" }
                 | ConfigSettings.MazeAngles.FortyFiveSharp -> { ImageID=Resources.Drawable.maze_fortyfive_sharp; Label="" }
                 | ConfigSettings.MazeAngles.Hexagonal -> { ImageID=Resources.Drawable.maze_hexagonal; Label="" }
                 | ConfigSettings.MazeAngles.Qbert -> { ImageID=Resources.Drawable.maze_qbert; Label="" }
                 | ConfigSettings.MazeAngles.Arbitrary -> { ImageID=Resources.Drawable.maze_arbitrary1; Label="" }
                 | ConfigSettings.MazeAngles.Arbitrary2 -> { ImageID=Resources.Drawable.maze_arbitrary2; Label="" }
                 | x -> failwithf "maze angles %A not supported" x),
                numColumns=2) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.VineStartPos, 
            Views.RandomizedItemChooser<ConfigSettings.VineStartPos>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.VineStartPos.Left -> { ImageID=Resources.Drawable.vinestartpos_left; Label="Left" }
                 | ConfigSettings.VineStartPos.Right -> { ImageID=Resources.Drawable.vinestartpos_right; Label="Right" }
                 | ConfigSettings.VineStartPos.Bottom -> { ImageID=Resources.Drawable.vinestartpos_bottom; Label="Bottom" }
                 | ConfigSettings.VineStartPos.Top -> { ImageID=Resources.Drawable.vinestartpos_top; Label="Top" }
                 | ConfigSettings.VineStartPos.BottomAndTop -> { ImageID=Resources.Drawable.vinestartpos_bottom_top; Label="Bottom & top" }
                 | ConfigSettings.VineStartPos.LeftAndRight -> { ImageID=Resources.Drawable.vinestartpos_left_right; Label="Left & right" }
                 | ConfigSettings.VineStartPos.BottomLeft -> { ImageID=Resources.Drawable.vinestartpos_bottom_left; Label="Bottom & left" }
                 | ConfigSettings.VineStartPos.TopRight -> { ImageID=Resources.Drawable.vinestartpos_top_right; Label="Top & right" }
                 | x -> failwithf "vine start position %A not supported" x),
                numColumns=4) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.VineType, 
            Views.RandomizedItemChooser<ConfigSettings.VineType>.GetFactoryForMapping(
                function 
                | ConfigSettings.VineType.Single -> { ImageID=Resources.Drawable.vinetype_single; Label="" }
                | ConfigSettings.VineType.Curly1 -> { ImageID=Resources.Drawable.vinetype_curly1; Label="" }
                | ConfigSettings.VineType.Curly2 -> { ImageID=Resources.Drawable.vinetype_curly2; Label="" }
                | ConfigSettings.VineType.Curly3 -> { ImageID=Resources.Drawable.vinetype_curly3; Label="" }
                | ConfigSettings.VineType.Splitting -> { ImageID=Resources.Drawable.vinetype_splitting; Label="" }
                | x -> failwithf "vine type %A not supported" x) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.VineLeafType, 
            Views.RandomizedItemChooser<ConfigSettings.VineLeafType>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.VineLeafType.None -> { ImageID=Resources.Drawable.leaf_noleaf; Label="" }
                 | ConfigSettings.VineLeafType.Curved1 -> { ImageID=Resources.Drawable.leaf_curved; Label="" }
                 | ConfigSettings.VineLeafType.IndAlt -> { ImageID=Resources.Drawable.leaf_indalt; Label="" }
                 | ConfigSettings.VineLeafType.IndByScale -> { ImageID=Resources.Drawable.leaf_indbyscale; Label="" }
                 | ConfigSettings.VineLeafType.Rounded -> { ImageID=Resources.Drawable.leaf_rounded; Label="" }
                 | ConfigSettings.VineLeafType.SlightlyCurved -> { ImageID=Resources.Drawable.leaf_slightlycurved; Label="" }
                 | ConfigSettings.VineLeafType.WithStem -> { ImageID=Resources.Drawable.leaf_withstem; Label="" }
                 | ConfigSettings.VineLeafType.Straight1 -> { ImageID=Resources.Drawable.leaf_straight; Label="" }
                 | x -> failwithf "vine leaf type %A not supported" x),
                numColumns=4) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.FloralSwirlsLeaves, 
            Views.RandomizedItemChooser<ConfigSettings.FloralSwirlsLeaves>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.FloralSwirlsLeaves.NoLeaves -> { ImageID=Resources.Drawable.leaf_noleaf; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.Curved -> { ImageID=Resources.Drawable.leaf_curved; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.ScaledToBranchThickness -> { ImageID=Resources.Drawable.leaf_indbyscale; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.SlightlyCurved -> { ImageID=Resources.Drawable.leaf_slightlycurved; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.WithStem -> { ImageID=Resources.Drawable.leaf_withstem; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.Straight -> { ImageID=Resources.Drawable.leaf_straight; Label="" }
                 | ConfigSettings.FloralSwirlsLeaves.Teardrop -> { ImageID=Resources.Drawable.leaf_teardrop; Label="" }
                 | x -> failwithf "leaf type %A not supported" x),
                numColumns=4) 
        )

        AndroidSettingsUIBuilder.RegisterEnumEditorFactory(
            EnumRepresentation.FloralSwirlsPrimary, 
            Views.RandomizedItemChooser<ConfigSettings.FloralSwirlsPrimary>.GetFactoryForMapping(
                (function 
                 | ConfigSettings.FloralSwirlsPrimary.WavySingle -> { ImageID=Resources.Drawable.fsprimary_wavysingle; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.WavyDouble -> { ImageID=Resources.Drawable.fsprimary_wavydouble; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.MagneticSingle -> { ImageID=Resources.Drawable.fsprimary_magnetic_single; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.MagneticBunch -> { ImageID=Resources.Drawable.fsprimary_magnetic_bunch; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.SShapedSingle -> { ImageID=Resources.Drawable.fsprimary_s_single; Label="" }
                 | ConfigSettings.FloralSwirlsPrimary.SShapedSpacefill -> { ImageID=Resources.Drawable.fsprimary_s_spacefill; Label="" }
                 | x -> failwithf "floral swirls primary %A not supported" x),
                 numColumns=2) 
        )
    )