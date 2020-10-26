module Program

open OpenTK
open OpenTK.Graphics.ES20

open LibDworkin.Aether.Operators
open LibDworkin.Renderer
open LibDworkin.RendererComponents
open GUI
open LibDworkin
open LibDworkin.DworkinSettings
open Settings

// DworkinConfigs
open PCB
open FlowingLeaves
open Maze
open DworkinEnvironmentConfigCustom


type MainWindow<'TRenderer when 'TRenderer :> BaseRenderer>(renderer : 'TRenderer, ?guis : GUI.Widgets.AbstractGui list) =
    inherit GUI.Window(1280, 720, defaultArg guis [])

    member this.HandleKeyUp(args : Input.KeyboardKeyEventArgs) =
        match args.Key with
        | Input.Key.S ->
            for each in this.Guis do each.ToggleVisibility()
        | Input.Key.R -> // reload all shaders and textures in currently used effects and materials
            renderer.Effect.ResetShader()
            renderer.Effect <- renderer.Effect
            renderer.Background.ResetShader()
            renderer.Background <- renderer.Background
            renderer.Foreground.ResetShader()
            renderer.Foreground <- renderer.Foreground
        | _ -> ()

    override this.OnLoad(e) = 
        base.OnLoad(e)
        this.Keyboard.KeyUp.Add(this.HandleKeyUp)
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
 

 
let pcbConfig = PCB_RandomPosChips()
let settingsHolder = 
    ref { 
        Environment=ConfigSettings.PCB_RandomPosChips(); 
        Effect=Effects.Plain();
        ForegroundColor=Defined(OpenTK.Graphics.Color4.Black.AsRGBTriple);
        BackgroundColor=Defined(OpenTK.Graphics.Color4.White.AsRGBTriple);
        Foreground=Materials.SolidColor();
        Background=Materials.SolidColor();
        Seed=42;
    }
let platform = DotNETPlatform()
let renderer = DworkinRenderer(platform, settingsHolder)

module Builders =
    let internal makeShaderBasedComponentEditor<'T when 'T :> ShaderBasedComponent> (setter : DworkinRenderer -> 'T -> unit) name (children : (string * obj * (string -> ConfigPropertyEditor<RendererComponentFactory<'T>>)) list) : IRendererPropertyEditor = 
        let defaultValues = children |> List.map (fun (a, b, _) -> a, box b) |> dict
        let guis = children |> List.map (fun (propName, _, builder) -> builder propName)
        (Builders.shaderBasedComponentEditor platform setter name defaultValues guis)

    let effectEditor<'T when 'T :> Effect> =
        makeShaderBasedComponentEditor<'T> (fun renderer value -> renderer.Effect <- value) 
    
    let foregroundEditor<'T when 'T :> MaterialSettings> name settings = 
        ForegroundSettingsEditor<'T>(name, settings, settingsHolder) :> IRendererPropertyEditor
    
    let backgroundEditor<'T when 'T :> MaterialSettings> name settings = 
        BackgroundSettingsEditor<'T>(name, settings, settingsHolder) :> IRendererPropertyEditor

    let effectSettingsEditor<'T when 'T :> EffectSettings> name settings =
        EffectSettingsEditor<'T>(name, settings, settingsHolder) :> IRendererPropertyEditor


let configGui  = 
    let possibleAnglesValues = [ [| -45.0f |]; [| -60.0f |]; [| -45.0f; -90.0f |]; [| -30.0f; -45.0f; -90.0f |] ]
    let configChooser =
        Builders.chooser
            "Environment"
            renderer
            [
                Builders.environmentEditor
                    "PCB-RandPosChips"
                    pcbConfig
                    [ 
                        Builders.floatSliderEditor "Angle" PCB_RandomPosChips.Angle_ 0.0f 90.0f 45.0f;
                        Builders.checkboxEditor "Vias" PCB_RandomPosChips.Vias_;
                        Builders.radioButtonEditor "Endpoints" PCB_RandomPosChips.Endpoints_ ["hollow"; "filled"];
                        Builders.checkboxEditor "Branch" PCB_RandomPosChips.Branch_;
                        Builders.checkboxEditor "Octagonal" PCB_RandomPosChips.Octagonal_;
                        Builders.dropdownListEditor "Angles" PCB_RandomPosChips.Angles_ possibleAnglesValues;
                        Builders.floatSliderEditor "Thickness variance" PCB_RandomPosChips.ThicknessVariance_ 0.0f 1.0f 0.1f;
                        Builders.floatSliderEditor "Density" PCB_RandomPosChips.Density_ 0.2f 0.8f 0.05f;
                        Builders.floatSliderEditor "Size" PCB_RandomPosChips.Size_ 0.75f 1.5f 0.05f;
                        Builders.floatSliderEditor "Speed" PCB_RandomPosChips.Speed_ 0.1f 1.0f 0.1f;
                        Builders.numberboxEditor "Random seed" LibDworkin.Lenses.randomSeed_<PCB_RandomPosChips>;
                    ];
                Builders.environmentEditor
                    "SpiralsProbesBranching1"
                    (SpiralsProbesBranching1())
                    [ 
                        Builders.intSliderEditor "N" SpiralsProbesBranching1.N_ 1 8 1;
                        Builders.numberboxEditor "Random seed" LibDworkin.Lenses.randomSeed_<SpiralsProbesBranching1>;
                    ];
                Builders.environmentEditor
                    "MagneticSpiralsBunch"
                    (MagneticSpiralsBunchBranching2_3())
                    [ 
                        Builders.floatSliderEditor "Scale" MagneticSpiralsBunchBranching2_3.Scale_ 1.0f 2.0f 0.1f;
                        Builders.intSliderEditor "MaxInitialAgents" 
                            (LibDworkin.Lenses.config_ >-> MiscConfigs.SpawnConfig.ProbeBunchMagneticSpirals2Branch3.MaxInitialAgents_) 3 15 1;
                        Builders.numberboxEditor "Random seed" LibDworkin.Lenses.randomSeed_<MagneticSpiralsBunchBranching2_3>;
                    ];
                EnvironmentSettingsEditor("Maze", ConfigSettings.Maze());
                Builders.environmentEditor
                    "FlowingLeavesExperiment"
                    (FlowingLeaves_WIP())
                    [ 
                        Builders.floatSliderEditor "Scale" FlowingLeaves_WIP.Scale_ 0.5f 2.0f 0.1f;
                        Builders.floatSliderEditor "Density" FlowingLeaves_WIP.Density_ 0.75f 2.0f 0.05f;
                        Builders.floatSliderEditor "ScaleVariance" FlowingLeaves_WIP.ScaleVariance_ 0.0f 1.0f 0.1f;
                        Builders.floatSliderEditor "Curviness" FlowingLeaves_WIP.Curviness_ 0.0f 2.0f 0.1f;
                        Builders.floatSliderEditor "LeafThickness" FlowingLeaves_WIP.LeafThickness_ 2.5f 10.0f 0.5f;
                        Builders.floatSliderEditor "LeafLength" FlowingLeaves_WIP.LeafLength_ 40.0f 90.0f 5.0f;
                        Builders.floatSliderEditor "LeafShape" FlowingLeaves_WIP.LeafShape_ 0.5f 0.8f 0.05f;
                        Builders.floatSliderEditor "Speed" FlowingLeaves_WIP.Speed_ 0.25f 1.0f 0.05f;
                        Builders.floatSliderEditor "Brush Smoothness" (FlowingLeaves_WIP.AgentConfig_ >-> LibDworkin.Agents.DworkinAgentConfig.Brush_  >-> Lenses.cast<LibDworkin.Agents.BrushStrategy, LibDworkin.Configs.BrushStrategySolidSmoother> >-> LibDworkin.Configs.BrushStrategySolidSmoother.Smoothness_) 1.0f 5.0f 0.5f;
                        Builders.numberboxEditor "Random seed" LibDworkin.Lenses.randomSeed_<FlowingLeaves_WIP>;
                    ];
            ]
    let speedControl = 
        Builders.radioButtonEditor 
            "Steps/frame" 
            ((fun (r : DworkinRenderer) -> r.StepsPerFrame), (fun (value : int) (r : DworkinRenderer) -> r.StepsPerFrame <- value; r))
            [ 1; 2; 5; 20 ]
    speedControl.ConfigRef <- ref renderer
    Builders.window (Gwen.Pos.Top ||| Gwen.Pos.Left) 0.65 guiWidth [ configChooser; speedControl ]

let effectGui = 
    Builders.chooser
        "Effect"
        renderer
        [
            Builders.effectEditor<SDFVisualizerEffect> "sdfVisualizer"
                [ 
                    "frequency", box 32.0f, Builders.ShaderComponents.floatSliderEditor 8.0f 64.0f 4.0f;
                ];
            Builders.effectEditor<PlainEffect> "plain" [];
            Builders.effectSettingsEditor<Effects.Shadow> "shadow" <| Effects.Shadow();
            Builders.effectSettingsEditor<Effects.FuzzyTexture> "fuzzy texture" <| Effects.FuzzyTexture();
            Builders.effectEditor<OutlineEffect> "outline"
                [ 
                    "outlineWidth", box 0.5f, Builders.ShaderComponents.floatSliderEditor 0.01f 0.5f 0.01f;
                ];
            Builders.effectSettingsEditor<Effects.Bevel> "bevel" <| Effects.Bevel();
            Builders.effectEditor<HighlightEffect> "highlight"
                [ 
                    "lightPos", box (Geometry.Point(0.0f, 1.0f)), Builders.ShaderComponents.vec2Editor -1.0f 1.0f 0.05f;
                    "lightIntensity", box 0.5f, Builders.ShaderComponents.floatSliderEditor 0.0f 1.0f 0.05f;
                ];
            Builders.effectSettingsEditor<Effects.Glow> "glow" <| Effects.Glow();
        ]

type private MaterialGuiKind =
    | Background
    | Foreground of Chooser

let private makeMaterialGui (setter : DworkinRenderer -> Material -> unit) (kind : MaterialGuiKind) (defaultColor : Graphics.Color4) =
    let makeEditor name settings =
        match kind with 
        | Foreground(_) -> Builders.foregroundEditor name settings
        | Background -> Builders.backgroundEditor name settings
    let mutable guis = 
        [
            makeEditor "Solid color" <| Materials.SolidColor();
            Builders.makeShaderBasedComponentEditor<MetalMaterial> setter "metal"
                [ 
                    "color", box defaultColor, Builders.ShaderComponents.colorEditor;
                    "highlight", box 0.2f, Builders.ShaderComponents.floatSliderEditor 0.0f 0.4f 0.05f;
                    "scale", box 1.0f, Builders.ShaderComponents.floatSliderEditor 0.2f 3.0f 0.1f;
                    "offset", box (Geometry.Point(1.0f, 0.0f)), Builders.ShaderComponents.vec2Editor -1.0f 1.0f 0.1f;
                    "angle", box 0.0f, Builders.ShaderComponents.floatSliderEditor 0.0f 180.0f 5.0f;
                    "stretch", box 0.0f, Builders.ShaderComponents.floatSliderEditor 0.0f 20.0f 1.0f;
                    "strokeFreq", box 500.0f, Builders.ShaderComponents.floatSliderEditor 200.0f 1000.0f 50.0f;
                    "strokeLength", box 0.5f, Builders.ShaderComponents.floatSliderEditor 0.1f 1.0f 0.05f;
                    "strokeIntensity", box 0.35f, Builders.ShaderComponents.floatSliderEditor 0.0f 1.0f 0.05f;
                    "strokeAngle", box 0.0f, Builders.ShaderComponents.floatSliderEditor 0.0f 180.0f 5.0f;
                ];
            makeEditor "Wood" <| Materials.Wood();
            makeEditor "Paper" <| Materials.Paper();
            makeEditor "Metal" <| Materials.Metal();
        ]
    match kind with 
        | Foreground(bgChooser) -> 
            //guis <- guis @ [ ColorChangeMaterialEditor(platform, bgChooser, setter) ]
            // Color change editor doesn't work anymore
            ()
        | Background -> ()
    Builders.chooser
        (match kind with | Foreground(_) -> "Foreground" | Background -> "Background")
        renderer
        guis

let backgroundGui = makeMaterialGui (fun renderer value -> renderer.Background <- value) Background Graphics.Color4.White
let foregroundGui = makeMaterialGui (fun renderer value -> renderer.Foreground <- value) (Foreground backgroundGui) Graphics.Color4.Black
let colorsGui = MaterialsColorEditor(renderer, platform, settingsHolder)
let materialsGui = Builders.tabPane [ ("foreground", foregroundGui :> AbstractGui); ("background", backgroundGui :> AbstractGui) ]
let appearanceGui = Builders.window (Gwen.Pos.Top ||| Gwen.Pos.Right) 0.8 guiWidth [ effectGui; colorsGui; materialsGui ]


[<EntryPoint>]
let main _argv =
    let window = new MainWindow<_>(renderer, [configGui; appearanceGui])
    window.Run(30.0)

    0 // return an integer exit code
