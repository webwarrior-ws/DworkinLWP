module Program

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.ES20

open LibDworkin.Renderer
open LibDworkin.Aether
open LibDworkin.Configs



type MainWindow<'TRenderer when 'TRenderer :> BaseRenderer>(renderer : 'TRenderer, ?guis : GUI.Widgets.AbstractGui list) =
    inherit GUI.Window(1280, 720, match guis with | Some(lst) -> lst | None -> [])

    override this.OnLoad(e) = 
        base.OnLoad(e)
        this.Keyboard.KeyUp.Add(fun args -> 
            if args.Key = Input.Key.S then 
                for each in this.Guis do each.ToggleVisibility())
        GL.ClearColor(0.0f, 0.0f, 0.0f, 0.0f)
        renderer.OnSurfaceCreated()
    
    override this.OnResize(e) =
        base.OnResize(e)
        renderer.OnSurfaceChanged(this.Width, this.Height)
    
    override this.OnUpdateFrame(e) =
        base.OnUpdateFrame(e)
    
    override this.OnRenderFrame(e) =
        renderer.OnDrawFrame()
        base.OnRenderFrame(e)
        this.SwapBuffers()
 

 
let config = GeneratedConfigs.PCB_RandomPosChips()
let renderer = DworkinRenderer(NETCorePlatform(), config)

let configGui  = 
    GUI.Widgets.EnviromentConfigChooserGui(renderer, 
        [
            GUI.Widgets.EnvironmentConfigGui(
                "PCB-RandomPosChips", config, 
                [ 
                    GUI.Widgets.FloatSliderPropertyGui("Angle", GeneratedConfigs.PCB_RandomPosChips.Angle_, 0.0f, 90.0f, 45.0f);
                    GUI.Widgets.CheckboxPropertyGui("Vias", GeneratedConfigs.PCB_RandomPosChips.Vias_);
                    GUI.Widgets.RadioButtonPropertyGui("Endpoints", GeneratedConfigs.PCB_RandomPosChips.Endpoints_, ["hollow"; "filled"])
                ])
        ] )


[<EntryPoint>]
let main argv =
    let window = new MainWindow<_>(renderer, [configGui])
    window.Run(30.0)

    0 // return an integer exit code
