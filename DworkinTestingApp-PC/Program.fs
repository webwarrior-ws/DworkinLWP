module Program

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.ES20

open Gwen.Control

open LibDworkin.Renderer
open LibDworkin.Aether
open LibDworkin.Configs


let config = GeneratedConfigs.PCB_RandomPosChips()
let renderer = DworkinRenderer(NETCorePlatform(), config)

let configGui  = 
    GUI.EnviromentConfigChooserGui(renderer, 
        [
            GUI.EnvironmentConfigGui(
                "PCB-RandomPosChips", config, 
                [ GUI.FloatSliderPropertyGui("Angle", GeneratedConfigs.PCB_RandomPosChips.Angle_, 0.0f, 90.0f, 45.0f) ])
        ] )


type MainWindow<'TRenderer when 'TRenderer :> BaseRenderer>(renderer : 'TRenderer) =
    inherit GUI.Window(1280, 720, [configGui])

    override this.OnLoad(e) = 
        base.OnLoad(e)
        GL.ClearColor(0.0f, 0.0f, 0.0f, 0.0f)
        renderer.OnSurfaceCreated()
        // setup gui
        //let control = new Gwen.Control.MessageBox(this.Canvas, "hello world")
        //control.SetPosition(20, 20)
        //control.Dismissed <- Gwen.Control.Base.GwenEventHandler(fun _base args -> System.Diagnostics.Debug.Print("messagebox"))
        (*
        let tabPanel = new TabControl(this.Canvas)
        tabPanel.Position(Gwen.Pos.Top ||| Gwen.Pos.Left, 20, 20)
        tabPanel.SetSize(240, 200) |> ignore
        //tabPanel.CurrentButton.UserData

        let panel = new GroupBox(tabPanel)
        tabPanel.AddPage("PCB", panel) |> ignore
        panel.Text <- "PCB-RandomPosChips"
        panel.AutoSizeToContents <- true
        panel.ShouldDrawBackground <- true
        panel.Position(Gwen.Pos.Top ||| Gwen.Pos.Left, 20, 20)
        
        let label = new Label(panel, Text="Angle", Dock=Gwen.Pos.Top)
        let angleSlider = new HorizontalSlider(panel, Min=0.0f, Max=180.0f, Value=0.0f, SnapToNotches=true, NotchCount=4, Width=180, Height=20, Dock=Gwen.Pos.Top)
        //angleSlider.Y <- label.Height
        angleSlider.add_ValueChanged(fun _base args -> 
            //config := !config |> Optic.set GeneratedConfigs.PCB_RandomPosChips.Angle_ angleSlider.Value
            (!config).Angle <- angleSlider.Value
            renderer.OnSurfaceChanged(this.Width, this.Height) )
        
        panel.SizeToChildren() |> ignore
        //tabPanel.SizeToChildren() |> ignore
        //panel.DelayedDelete()
        *)

    
    override this.OnResize(e) =
        base.OnResize(e)
        renderer.OnSurfaceChanged(this.Width, this.Height)
    
    override this.OnUpdateFrame(e) =
        base.OnUpdateFrame(e)
    
    override this.OnRenderFrame(e) =
        renderer.OnDrawFrame()
        base.OnRenderFrame(e)
        this.SwapBuffers()
        


[<EntryPoint>]
let main argv =
    let window = new MainWindow<_>(renderer)
    window.Run(30.0)

    0 // return an integer exit code
