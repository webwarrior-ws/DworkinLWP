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
                 | x -> failwithf "bevel profile %A not supported" x),
                numColumns=2) 
        )
    )