module Program

open OpenTK
open OpenTK.Graphics.ES20

open LibDworkin.Renderer
open GUI


type MainWindow<'TRenderer when 'TRenderer :> BaseRenderer>(renderer : 'TRenderer, ?guis : GUI.Widgets.AbstractGui list) =
    inherit GUI.Window(1280, 720, defaultArg guis [])

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
    Builders.environmentChooser
        renderer
        [
            Builders.environmentEditor
                "PCB-RandomPosChips"
                config
                [ 
                    Builders.floatSliderEditor "Angle" GeneratedConfigs.PCB_RandomPosChips.Angle_ 0.0f 90.0f 45.0f;
                    Builders.checkboxEditor "Vias" GeneratedConfigs.PCB_RandomPosChips.Vias_;
                    Builders.radioButtonEditor "Endpoints" GeneratedConfigs.PCB_RandomPosChips.Endpoints_ ["hollow"; "filled"];
                    Builders.checkboxEditor "Branch" GeneratedConfigs.PCB_RandomPosChips.Branch_;
                    Builders.checkboxEditor "Octagonal" GeneratedConfigs.PCB_RandomPosChips.Octagonal_;
                ]
        ]


[<EntryPoint>]
let main _argv =
    let window = new MainWindow<_>(renderer, [configGui])
    window.Run(30.0)

    0 // return an integer exit code
