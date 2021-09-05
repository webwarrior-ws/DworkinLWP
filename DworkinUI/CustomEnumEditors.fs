namespace DworkinUI


open Settings
open LibDworkin.DworkinSettings
open LibDworkin.RendererComponents



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
    )