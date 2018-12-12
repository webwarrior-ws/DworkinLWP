module rec GUI

open OpenTK
open Gwen
open Gwen.Control
open LibDworkin.Renderer
open LibDworkin.Configs
open LibDworkin.Aether



[<AbstractClass>]
type AbstractGui() =
    abstract member Update : unit -> unit
    abstract member Initialize : Base -> unit
    abstract member ToggleVisibility : unit -> unit
    default this.ToggleVisibility() = ()


[<AbstractClass>]
type EnvironmentPropertyGui<'TConfig>() =
    inherit AbstractGui()

    let mutable configRef : 'TConfig ref option = None
    member this.ConfigRef 
        with get() = configRef.Value
        and set(reference : 'TConfig ref) = configRef <- Some(reference)


type FloatSliderPropertyGui<'TConfig>(name : string, lens : Lens<'TConfig, float32>, minValue: float32, maxValue: float32, step: float32) =
    inherit EnvironmentPropertyGui<'TConfig>()
    let mutable slider : HorizontalSlider = null

    override this.Initialize(parent) =
        let panel = new GroupBox(parent)
        panel.Text <- name
        panel.AutoSizeToContents <- true
        //panel.ShouldDrawBackground <- true
        //panel.Position(Gwen.Pos.Top, 5, 5)
        let notchCount = if step = 0.0f then 0 else int((maxValue - minValue) / step)
        slider <- new HorizontalSlider(panel, Min=minValue, Max=maxValue, Value=(Optic.get lens (!this.ConfigRef)), 
                                       SnapToNotches=(notchCount>0), NotchCount=notchCount, 
                                       Width=180, Height=20, Dock=Gwen.Pos.Top)
        panel.SizeToChildren() |> ignore
    
    override this.Update() =
        if slider <> null && slider.Value <> Optic.get lens (!this.ConfigRef) then
            this.ConfigRef := Optic.set lens slider.Value (!this.ConfigRef)
 

[<AbstractClass>]
type IEnvironmentConfigGui() =
    inherit AbstractGui()
    abstract Renderer : DworkinRenderer option with get, set
    abstract Name : string with get
    abstract SetConfig : unit -> unit


type EnvironmentConfigGui<'TConfig when 'TConfig :> DworkinEnvironmentConfig>(name : string, config : 'TConfig, propertyGuis : EnvironmentPropertyGui<'TConfig> list) =
    inherit IEnvironmentConfigGui()
    let configRef = ref config
    let mutable lastConfig = config

    override val Renderer : DworkinRenderer option = None with get, set
    
    override this.Name = name
    member this.Config = !configRef

    override this.Initialize(panel : Base) =
        for gui in propertyGuis do 
            gui.ConfigRef <- configRef
            gui.Initialize(panel)
    
    override this.SetConfig() =
        match this.Renderer with
        | Some(renderer) -> renderer.EnvironmentConfig <- !configRef
        | None -> ()

    override this.Update() = 
        for gui in propertyGuis do 
            gui.Update()
        if not(Unchecked.equals lastConfig !configRef) then
            this.SetConfig()
            lastConfig <- !configRef


type EnviromentConfigChooserGui(renderer : DworkinRenderer, configGuis : IEnvironmentConfigGui list) = 
    inherit AbstractGui()

    let mutable canvas : Base = null 
    let mutable lastTabName = ""
    let mutable tabPanel : TabControl = null

    override this.Initialize(parent : Base) =
        canvas <- parent

        tabPanel <- new TabControl(canvas)
        tabPanel.Position(Gwen.Pos.Top ||| Gwen.Pos.Left, 20, 20)
        tabPanel.SetSize(240, canvas.Height * 2 / 3) |> ignore

        for each in configGuis do
            let panel = new GroupBox(tabPanel)
            let tabButton = tabPanel.AddPage(each.Name, panel)
            tabButton.Name <- each.Name
            tabButton.UserData <- each
            panel.AutoSizeToContents <- true
            panel.ShouldDrawBackground <- true
            //panel.Position(Gwen.Pos.Top ||| Gwen.Pos.Left, 10, 10)

            each.Renderer <- Some(renderer)
            each.Initialize(panel)

            panel.SizeToChildren() |> ignore
        
        lastTabName <- tabPanel.CurrentButton.Name
    
    override this.Update() = 
        if tabPanel <> null then
            let currConfigGui = tabPanel.CurrentButton.UserData :?> IEnvironmentConfigGui
            if lastTabName <> tabPanel.CurrentButton.Name then
                currConfigGui.SetConfig()
                lastTabName <- tabPanel.CurrentButton.Name
            else
                currConfigGui.Update()
    
    override this.ToggleVisibility() =
        if tabPanel <> null then
            if tabPanel.IsVisible then tabPanel.Hide() else tabPanel.Show()



type Window(width : int, height : int, guis : AbstractGui list) as this =
    inherit GameWindow(width, height)

    let input = Gwen.Input.OpenTK(this)
    let renderer = new Gwen.Renderer.OpenTK()
    let skin = new Gwen.Skin.TexturedBase(renderer, "DefaultSkin.png")
    let canvas = new Canvas(skin)

    member this.Canvas = canvas
    member this.Guis = guis
    
    override this.OnLoad(e) =
        skin.DefaultFont <- new Font(renderer, "Arial", 10)
        input.Initialize(canvas)
        canvas.SetSize(this.Width, this.Height) |> ignore

        this.Keyboard.KeyDown.Add(input.ProcessKeyDown >> ignore)
        this.Keyboard.KeyUp.Add(input.ProcessKeyUp >> ignore)

        this.Mouse.ButtonDown.Add(input.ProcessMouseMessage >> ignore)
        this.Mouse.ButtonUp.Add(input.ProcessMouseMessage >> ignore)
        this.Mouse.Move.Add(input.ProcessMouseMessage >> ignore)
        this.Mouse.WheelChanged.Add(input.ProcessMouseMessage >> ignore)

        for each in this.Guis do each.Initialize(this.Canvas)
    
    override this.OnResize(e) =
        Graphics.OpenGL.GL.Viewport(0, 0, this.Width, this.Height)
        let projectionMatrix = 
            Matrix4.CreateTranslation( Vector3(float32(-this.Width) / 2.0f, float32(-this.Height) / 2.0f, 0.0f)) * 
            Matrix4.CreateScale(Vector3(1.0f, -1.0f, 1.0f)) * 
            Matrix4.CreateOrthographic(float32(this.Width), float32(this.Height), -1.0f, 1.0f)
        renderer.Resize(ref projectionMatrix, this.Width, this.Height);
        canvas.SetSize(this.Width, this.Height) |> ignore
    
    override this.OnRenderFrame(e) =
        canvas.RenderCanvas()
    
    override this.OnUpdateFrame(e) =
        renderer.Update(e.Time)
        
        for each in this.Guis do each.Update()


module Builders =
    let foo = None