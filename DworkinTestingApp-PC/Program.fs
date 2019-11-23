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
let defaultSettings = 
    { 
        Environment=ConfigSettings.PCB_RandomPosChips(); 
        Effect=Effects.Plain();
        Foreground=Materials.SolidColor(Color=Defined(OpenTK.Graphics.Color4.Black.AsRGBTriple));
        Background=Materials.SolidColor(Color=Defined(OpenTK.Graphics.Color4.White.AsRGBTriple));
        Seed=42;
    }
let platform = DotNETPlatform()
let renderer = DworkinRenderer(platform, ref defaultSettings)

module Builders =
    let internal makeShaderBasedComponentEditor<'T when 'T :> ShaderBasedComponent> (setter : DworkinRenderer -> 'T -> unit) name (children : (string * obj * (string -> ConfigPropertyEditor<RendererComponentFactory<'T>>)) list) : IRendererPropertyEditor = 
        let defaultValues = children |> List.map (fun (a, b, _) -> a, box b) |> dict
        let guis = children |> List.map (fun (propName, _, builder) -> builder propName)
        (Builders.shaderBasedComponentEditor platform setter name defaultValues guis)

    let effectEditor<'T when 'T :> Effect> =
        makeShaderBasedComponentEditor<'T> (fun renderer value -> renderer.Effect <- value) 
    
    let foregroundEditor<'T when 'T :> Material> = 
        makeShaderBasedComponentEditor<'T> (fun renderer value -> renderer.Foreground <- value)
    
    let backgroundEditor<'T when 'T :> Material> =
        makeShaderBasedComponentEditor<'T> (fun renderer value -> renderer.Background <- value)


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
            Builders.effectEditor<ShadowEffect> "shadow"
                [ 
                    "shadowDir", box (Geometry.Point(0.5f, 0.5f)), Builders.ShaderComponents.directionEditor 5.0f;
                    "shadowOffset", box 5.0f, Builders.ShaderComponents.floatSliderEditor 0.0f 20.0f 1.0f;
                    "shadowSize", box 0.5f, Builders.ShaderComponents.floatSliderEditor 0.0f 1.0f 0.05f;
                    "shadowSoftness", box 1.0f, Builders.ShaderComponents.floatSliderEditor 0.05f 1.0f 0.05f;
                    "shadowStrength", box 1.0f, Builders.ShaderComponents.floatSliderEditor 0.1f 1.0f 0.05f;
                    "inner", box false, Builders.ShaderComponents.checkboxEditor;
                ];
            Builders.effectEditor<OutlineEffect> "outline"
                [ 
                    "outlineWidth", box 0.5f, Builders.ShaderComponents.floatSliderEditor 0.01f 0.5f 0.01f;
                ];
            Builders.effectEditor<BevelEffect> "bevel"
                [ 
                    "contrast", box 5.0f, Builders.ShaderComponents.floatSliderEditor 1.0f 10.0f 1.0f;
                    "maxDistance", box 1.0f, Builders.ShaderComponents.floatSliderEditor 0.0f 1.0f 0.05f;
                    "lightPos", box (Geometry.Point(0.5f, 0.75f)), Builders.ShaderComponents.directionEditor 5.0f;
                    "bevelProfile", box BevelProfile.Linear, 
                        Builders.ShaderComponents.dropdownListEditor [ 
                            BevelProfile.Linear; BevelProfile.Hyperbolic; BevelProfile.Parabola1; BevelProfile.Parabola2; BevelProfile.Exp ];
                    "mode", box BevelMode.Inner, Builders.ShaderComponents.radioButtonEditor [ BevelMode.Inner; BevelMode.Outer; BevelMode.Ridge; BevelMode.Trench ];
                ];
            Builders.effectEditor<HighlightEffect> "highlight"
                [ 
                    "lightPos", box (Geometry.Point(0.0f, 1.0f)), Builders.ShaderComponents.vec2Editor -1.0f 1.0f 0.05f;
                    "lightIntensity", box 0.5f, Builders.ShaderComponents.floatSliderEditor 0.0f 1.0f 0.05f;
                ];
            Builders.effectEditor<GlowEffect> "glow"
                [ 
                    "glowColor", box (Graphics.Color4.Aquamarine), Builders.ShaderComponents.colorEditor;
                    "glowStrength", box 1.0f, Builders.ShaderComponents.floatSliderEditor 0.05f 1.0f 0.05f;
                    "glowType", box GlowType.Outer, Builders.ShaderComponents.radioButtonEditor [ GlowType.Outer; GlowType.Inner ];
                ];
        ]

type private MaterialGuiKind =
    | Background
    | Foreground of Chooser

let private makeMaterialGui (setter : DworkinRenderer -> Material -> unit) (kind : MaterialGuiKind) (defaultColor : Graphics.Color4) =
    let mutable guis = 
        [
            Builders.makeShaderBasedComponentEditor<SolidColorMaterial> setter "solidColor"
                [ 
                    "color", box defaultColor, Builders.ShaderComponents.colorEditor;
                ];
            Builders.makeShaderBasedComponentEditor<NormalmapAndAOMaterial> setter "normalmap+ao"
                [ 
                    "color", box defaultColor, Builders.ShaderComponents.colorEditor;
                    "lightPos", box (Geometry.Point(0.0f, 1.0f)), Builders.ShaderComponents.directionEditor 5.0f;
                    "lightHeight", box 2.0f, Builders.ShaderComponents.floatSliderEditor 1.0f 4.0f 0.1f;
                    "scaleFactor", box 2.0f, Builders.ShaderComponents.floatSliderEditor 1.0f 3.0f 0.1f;
                    "normalMap", box "TexturesCom_Leather_Plain_512_normal.png", Builders.ShaderComponents.dropdownListEditor [ "TexturesCom_Leather_Plain_512_normal.png" ];
                    "ao", box None, Builders.ShaderComponents.radioButtonEditor [ None ];
                ];
            Builders.makeShaderBasedComponentEditor<ColoredTextureMaterial> setter "colored texture"
                [ 
                    "color", box defaultColor, Builders.ShaderComponents.colorEditor;
                    "flatShade", box 0.75f, Builders.ShaderComponents.floatSliderEditor 0.0f 1.0f 0.05f;
                    "contrast", box 1.0f, Builders.ShaderComponents.floatSliderEditor 1.0f 4.0f 0.1f;
                    "scaleFactor", box 2.0f, Builders.ShaderComponents.floatSliderEditor 1.0f 3.0f 0.1f;
                    "shadeMap", box "knitted-netting_@2X.png", Builders.ShaderComponents.dropdownListEditor [ "knitted-netting_@2X.png" ];
                ];
            Builders.makeShaderBasedComponentEditor<RoughSurfaceMaterial> setter "rough surface"
                [ 
                    "color", box defaultColor, Builders.ShaderComponents.colorEditor;
                    "bumpHeight", box 0.1f, Builders.ShaderComponents.floatSliderEditor 0.0f 0.25f 0.05f;
                    "dim", box 0.3f, Builders.ShaderComponents.floatSliderEditor 0.1f 1.0f 0.05f;
                    "featureScale", box 50.0f, Builders.ShaderComponents.floatSliderEditor 1.0f 100.0f 1.0f;
                    "flatArea", box 0.5f, Builders.ShaderComponents.floatSliderEditor 0.5f 2.5f 0.1f;
                ];
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
            Builders.makeShaderBasedComponentEditor<WoodMaterial> setter "wood"
                [ 
                    "color", box defaultColor, Builders.ShaderComponents.colorEditor;
                    "angle", box 0.0f, Builders.ShaderComponents.floatSliderEditor 0.0f 180.0f 5.0f;
                    "ringsScale", box 2.7f, Builders.ShaderComponents.floatSliderEditor 2.0f 5.0f 0.1f;
                    "ringsStretch", box 15.0f, Builders.ShaderComponents.floatSliderEditor 0.0f 50.0f 5.0f;
                    "contrast", box 0.25f, Builders.ShaderComponents.floatSliderEditor 0.0f 0.5f 0.05f;
                ];
        ]
    match kind with 
        | Foreground(bgChooser) -> 
            guis <- guis @ [ ColorChangeMaterialEditor(platform, bgChooser, setter) ]
        | Background -> ()
    Builders.chooser
        (match kind with | Foreground(_) -> "Foreground" | Background -> "Background")
        renderer
        guis

let backgroundGui = makeMaterialGui (fun renderer value -> renderer.Background <- value) Background Graphics.Color4.White
let foregroundGui = makeMaterialGui (fun renderer value -> renderer.Foreground <- value) (Foreground backgroundGui) Graphics.Color4.Black
let materialsGui = Builders.tabPane [ ("foreground", foregroundGui :> AbstractGui); ("background", backgroundGui :> AbstractGui) ]
let appearanceGui = Builders.window (Gwen.Pos.Top ||| Gwen.Pos.Right) 0.8 guiWidth [ effectGui; materialsGui ]


[<EntryPoint>]
let main _argv =
    let window = new MainWindow<_>(renderer, [configGui; appearanceGui])
    //let window = new MainWindow<_>(DworkinRenderer(NETCorePlatform(), MagneticSpiralsBunchBranching2_3()))
    window.Run(30.0)

    0 // return an integer exit code
