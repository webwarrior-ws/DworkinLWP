module Program

open OpenTK
open OpenTK.Graphics.ES20

open LibDworkin.Aether.Operators
open LibDworkin.Renderer
open GUI
open LibDworkin



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
 

 
let pcbConfig = GeneratedConfigs.PCB_RandomPosChips()
let renderer = DworkinRenderer(NETCorePlatform(), pcbConfig)

let configGui  = 
    let possibleAnglesValues = [ [| -45.0f |]; [| -60.0f |]; [| -45.0f; -90.0f |]; [| -30.0f; -45.0f; -90.0f |] ]
    Builders.environmentChooser
        renderer
        [
            Builders.environmentEditor
                "PCB-RandPosChips"
                pcbConfig
                [ 
                    Builders.floatSliderEditor "Angle" GeneratedConfigs.PCB_RandomPosChips.Angle_ 0.0f 90.0f 45.0f;
                    Builders.checkboxEditor "Vias" GeneratedConfigs.PCB_RandomPosChips.Vias_;
                    Builders.radioButtonEditor "Endpoints" GeneratedConfigs.PCB_RandomPosChips.Endpoints_ ["hollow"; "filled"];
                    Builders.checkboxEditor "Branch" GeneratedConfigs.PCB_RandomPosChips.Branch_;
                    Builders.checkboxEditor "Octagonal" GeneratedConfigs.PCB_RandomPosChips.Octagonal_;
                    Builders.dropdownListEditor "Angles" GeneratedConfigs.PCB_RandomPosChips.Angles_ possibleAnglesValues;
                    Builders.floatSliderEditor "Thickness variance" GeneratedConfigs.PCB_RandomPosChips.ThicknessVariance_ 0.0f 1.0f 0.1f;
                    Builders.floatSliderEditor "Density" GeneratedConfigs.PCB_RandomPosChips.Density_ 0.2f 0.8f 0.05f;
                    Builders.floatSliderEditor "Size" GeneratedConfigs.PCB_RandomPosChips.Size_ 0.75f 1.5f 0.05f;
                    Builders.floatSliderEditor "Speed" GeneratedConfigs.PCB_RandomPosChips.Speed_ 0.1f 1.0f 0.1f;
                    Builders.numberboxEditor "Random seed" LibDworkin.Lenses.randomSeed_<GeneratedConfigs.PCB_RandomPosChips>;
                ];
            Builders.environmentEditor
                "SpiralsProbesBranching1"
                (GeneratedConfigs.SpiralsProbesBranching1())
                [ 
                    Builders.intSliderEditor "N" GeneratedConfigs.SpiralsProbesBranching1.N_ 1 8 1;
                    Builders.numberboxEditor "Random seed" LibDworkin.Lenses.randomSeed_<GeneratedConfigs.SpiralsProbesBranching1>;
                ];
            Builders.environmentEditor
                "MagneticSpiralsBunch"
                (GeneratedConfigs.MagneticSpiralsBunchBranching2_3())
                [ 
                    Builders.floatSliderEditor "Scale" GeneratedConfigs.MagneticSpiralsBunchBranching2_3.Scale_ 1.0f 2.0f 0.1f;
                    Builders.intSliderEditor "MaxInitialAgents" 
                        (LibDworkin.Lenses.config_ >-> GeneratedConfigs.SpawnConfig.ProbeBunchMagneticSpirals2Branch3.MaxInitialAgents_) 3 15 1;
                    Builders.numberboxEditor "Random seed" LibDworkin.Lenses.randomSeed_<GeneratedConfigs.MagneticSpiralsBunchBranching2_3>;
                ];
        ]


type ShapesTestRenderer(context) =
    inherit BaseRenderer(context)                
    
    override this.OnSurfaceChanged(w, h) =
        base.OnSurfaceChanged( w, h)
        this.SetScale(2.0f / float32 w, 2.0f / float32 h)
        this.ClearSDF()
        //GL.ClearColor(1.0f, 1.0f, 0.0f, 1.0f)
        //GL.Clear(ClearBufferMask.ColorBufferBit)

    override this.OnDrawFrame() =
        let shape1 = 
            AgentAbstractions.Shape.Polygon(
                [|Vector2(10.0f, 10.0f); Vector2(10.0f, 20.0f); Vector2(20.0f, 20.0f); Vector2(20.0f, 10.0f)|] 
                |> Array.map ((+) (Vector2(50.0f, 50.0f))) )
        let shape2 = AgentAbstractions.Shape.Polygon(
            [|Vector2(0.0f, 0.0f); Vector2(0.0f, 10.0f); Vector2(10.0f, 15.0f); Vector2(10.0f, 0.0f)|]
            |> Array.map ((+) (Vector2(100.0f, 100.0f)))
            |> Array.rev)
        //this.BuildCircle(Vector2(1.0f, 1.0f), 0.5f)
        
        this.RenderShapes([| 
            AgentAbstractions.ShapeAndPaint(shape1, AgentAbstractions.ShapePaint.Default); 
            AgentAbstractions.ShapeAndPaint(shape2, AgentAbstractions.ShapePaint.Default) |])
        this.DrawImage()

[<EntryPoint>]
let main _argv =
    let window = new MainWindow<_>(renderer, [configGui])
    //let window = new MainWindow<_>(DworkinRenderer(NETCorePlatform(), GeneratedConfigs.MagneticSpiralsBunchBranching2_3()))
    //let window = new MainWindow<_>(ShapesTestRenderer(NETCorePlatform()))
    window.Run(30.0)

    0 // return an integer exit code
