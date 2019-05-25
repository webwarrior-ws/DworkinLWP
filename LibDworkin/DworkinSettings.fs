module LibDworkin.DworkinSettings

open Settings


[<AbstractClass>] 
type EnvironmentSettings() =
    inherit Settings()
    abstract member Build : unit -> Configs.DworkinEnvironmentConfig

[<AbstractClass>] 
type MaterialSettings() =
    inherit Settings()
    abstract member Build : unit -> RendererComponents.Material
