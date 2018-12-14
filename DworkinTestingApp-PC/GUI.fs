module GUI

open OpenTK
open Gwen
open Gwen.Control
open LibDworkin.Renderer
open LibDworkin.Configs
open LibDworkin.Aether


[<AutoOpen>]
module Widgets = 
    [<System.Runtime.InteropServices.DllImport("user32.dll")>]
    extern int GetDpiForSystem()
    
    let scaleFactor = 
        try
            float(GetDpiForSystem()) / 96.0;
        with | _ -> 1.0
    
    // element size constants
    let lineHeight = int(20.0 * scaleFactor)
    let sliderWidth = int(180.0 * scaleFactor)
    let guiWidth = int(220.0 * scaleFactor)
    let guiPadding = int(20.0 * scaleFactor)

    [<AbstractClass>]
    type AbstractGui() =
        abstract member Update : unit -> unit
        abstract member Initialize : Base -> unit
        abstract member ToggleVisibility : unit -> unit
        default this.ToggleVisibility() = ()

    /// Base class for editors that edit Dworkin config properties
    [<AbstractClass>]
    type ConfigPropertyEditor<'TConfig>() =
        inherit AbstractGui()

        let mutable configRef : 'TConfig ref option = None
        member this.ConfigRef 
            with get() = configRef.Value
            and set(reference : 'TConfig ref) = configRef <- Some(reference)


    type FloatSliderPropertyEditor<'TConfig>(name : string, lens : Lens<'TConfig, float32>, minValue: float32, maxValue: float32, step: float32) =
        inherit ConfigPropertyEditor<'TConfig>()
        let mutable slider : HorizontalSlider = null
        let mutable numField : TextBoxNumeric = null

        override this.Initialize(parent) =
            let panel = new GroupBox(parent, Height=lineHeight*2, Dock=Gwen.Pos.Top)
            panel.Text <- name
            let notchCount = if step = 0.0f then 0 else int((maxValue - minValue) / step)
            slider <- new HorizontalSlider(panel, Min=minValue, Max=maxValue, Value=(Optic.get lens (!this.ConfigRef)), 
                                           SnapToNotches=(notchCount>0), NotchCount=notchCount, 
                                           Width=sliderWidth*9/10, Height=lineHeight, Dock=Gwen.Pos.Left)
            numField <- new TextBoxNumeric(panel, Height=lineHeight, Width=sliderWidth/10, AutoSizeToContents=true, 
                                           Value=slider.Value, IsDisabled=true, Dock=Gwen.Pos.Left)
            slider.add_ValueChanged(fun _base _args -> numField.Value <- slider.Value)
    
        override this.Update() =
            if slider <> null && slider.Value <> Optic.get lens (!this.ConfigRef) then
                this.ConfigRef := Optic.set lens slider.Value (!this.ConfigRef)
 

     type CheckboxPropertyEditor<'TConfig>(name : string, lens : Lens<'TConfig, bool>) =
        inherit ConfigPropertyEditor<'TConfig>()
        let mutable checkbox : LabeledCheckBox = null

        override this.Initialize(parent) =
            checkbox <- new LabeledCheckBox(parent, Text=name, IsChecked=(Optic.get lens (!this.ConfigRef)), Height=lineHeight, Dock=Gwen.Pos.Top)
            if scaleFactor <> 1.0 then 
                checkbox.Children.[0].Width <- checkbox.GetChildrenSize().Y
    
        override this.Update() =
            if checkbox <> null && checkbox.IsChecked <> Optic.get lens (!this.ConfigRef) then
                this.ConfigRef := Optic.set lens checkbox.IsChecked (!this.ConfigRef)
     
     type RadioButtonPropertyEditor<'TConfig, 'TValue when 'TValue: equality>(name : string, lens : Lens<'TConfig, 'TValue>, options: 'TValue list) =
        inherit ConfigPropertyEditor<'TConfig>()
        let mutable radioGroup : RadioButtonGroup = null

        override this.Initialize(parent) =
            radioGroup <- new RadioButtonGroup(parent, Text=name, Height=lineHeight*2, Dock=Gwen.Pos.Top)
            radioGroup.AutoSizeToContents <- false
            for opt in options do
                let radioButton = radioGroup.AddOption(opt.ToString())
                radioButton.Dock <- Gwen.Pos.Left ||| Gwen.Pos.CenterH
                if scaleFactor <> 1.0 then 
                    radioButton.Margin <- Margin(0, lineHeight*3/4, 0, 0)
            let currValue = Optic.get lens (!this.ConfigRef)
            radioGroup.SetSelection(List.findIndex (fun each -> each = currValue) options)
        
        override this.Update() =
            if radioGroup <> null then
                let currIndex = radioGroup.SelectedIndex
                let currValue = Optic.get lens (!this.ConfigRef)
                if currValue <> options.[currIndex] then
                    this.ConfigRef := Optic.set lens (options.[currIndex]) (!this.ConfigRef)

    /// Superclass of EnvironmentConfigEditor needed only because it's not parametrized by config type
    [<AbstractClass>]
    type IEnvironmentConfigEditor() =
        inherit AbstractGui()
        abstract Renderer : DworkinRenderer option with get, set
        abstract Name : string with get
        abstract SetConfig : unit -> unit


    type EnvironmentConfigEditor<'TConfig when 'TConfig :> DworkinEnvironmentConfig>(name : string, 
                                                                                     config : 'TConfig, 
                                                                                     propertyGuis : ConfigPropertyEditor<'TConfig> list) =
        inherit IEnvironmentConfigEditor()
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


    type EnviromentConfigChooser(renderer : DworkinRenderer, configGuis : IEnvironmentConfigEditor list) = 
        inherit AbstractGui()

        let mutable canvas : Base = null 
        let mutable lastTabName = ""
        let mutable tabPanel : TabControl = null

        override this.Initialize(parent : Base) =
            canvas <- parent

            tabPanel <- new TabControl(canvas)
            tabPanel.Position(Gwen.Pos.Top ||| Gwen.Pos.Left, guiPadding, guiPadding)
            tabPanel.SetSize(guiWidth, canvas.Height * 2 / 3) |> ignore

            for each in configGuis do
                let panel = new GroupBox(tabPanel)
                let tabButton = tabPanel.AddPage(each.Name, panel)
                tabButton.Name <- each.Name
                tabButton.UserData <- each
                panel.AutoSizeToContents <- true
                panel.ShouldDrawBackground <- true

                each.Renderer <- Some(renderer)
                each.Initialize(panel)

                panel.SizeToChildren() |> ignore
        
            lastTabName <- tabPanel.CurrentButton.Name
    
        override this.Update() = 
            if tabPanel <> null then
                let currConfigGui = tabPanel.CurrentButton.UserData :?> IEnvironmentConfigEditor
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


/// Helper functions to make GUI Widgets
module Builders =
    let environmentChooser (renderer : DworkinRenderer) (configGuis : IEnvironmentConfigEditor list) =
        EnviromentConfigChooser(renderer, configGuis)
    
    let environmentEditor (name : string) (config : 'TConfig) (propertyGuis : ConfigPropertyEditor<'TConfig> list) =
        EnvironmentConfigEditor(name, config, propertyGuis)
    
    let floatSliderEditor (name : string) (lens : Lens<'TConfig, float32>) (minValue : float32) (maxValue : float32) (step : float32) =
        FloatSliderPropertyEditor(name, lens, minValue, maxValue, step)
    
    let checkboxEditor (name : string) (lens : Lens<'TConfig, bool>) =
        CheckboxPropertyEditor(name , lens)
    
    let radioButtonEditor (name : string) (lens : Lens<'TConfig, 'TValue>) (options: 'TValue list) =
        RadioButtonPropertyEditor(name, lens, options)