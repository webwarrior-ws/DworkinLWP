module GUI

open OpenTK

open Gwen
open Gwen.Control

open LibDworkin
open LibDworkin.Renderer
open LibDworkin.Configs
open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.RendererComponents
open LibDworkin.Geometry
open LibDworkin.DworkinSettings

open System.Collections.Generic


type RendererComponentFactory<'T when 'T :> ShaderBasedComponent>(platform : AbstractPlatform, initialParams : IDictionary<string, obj>) =
    let properties = Dictionary<string, obj>()
    do
        for pair in initialParams do properties.[pair.Key] <- pair.Value
        
    let mutable changed = true
    member this.Changed = changed

    member this.Build() : 'T = 
        changed <- false
        let typ = typeof<'T>
        let constructor = typ.GetConstructors().[0]
        let parameters = constructor.GetParameters()
        let parameterValues = Array.zeroCreate parameters.Length
        parameterValues.[0] <- box platform
        for i=1 to parameters.Length-1 do
            parameterValues.[i] <- properties.[parameters.[i].Name]
        constructor.Invoke(parameterValues) :?> 'T
        
    member this.Item
        with get(name : string) =
            properties.[name]
        and set (name : string) (value : obj) =
            changed <- true
            properties.[name] <- value
            
    member this.With(name : string, value : obj) =
        let newParams = Dictionary(properties)
        newParams.[name] <- value
        RendererComponentFactory<'T>(platform, newParams)

    static member LensFor<'TValue>(name : string) : Lens<RendererComponentFactory<'T>, 'TValue> =
        (fun factory -> unbox<'TValue> factory.[name]) , (fun v factory -> factory.[name] <- box v; factory)



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
    let guiPadding = int(10.0 * scaleFactor)

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

    
    [<AbstractClass>]
    type AbstractSliderPropertyEditor<'TConfig, 'TValue when 'TValue: equality>(name : string, lens : Lens<'TConfig, 'TValue>, minValue: 'TValue, maxValue: 'TValue, step: 'TValue) =
        inherit ConfigPropertyEditor<'TConfig>()
        let mutable slider : HorizontalSlider = null
        let mutable numField : TextBoxNumeric = null

        abstract Zero : 'TValue
        abstract ToFloat : 'TValue -> float32
        abstract FromFloat : float32 -> 'TValue

        member this.GetValue() = this.ToFloat(Optic.get lens (!this.ConfigRef))

        override this.Initialize(parent) =
            let panel = new GroupBox(parent, Text=name, Height=lineHeight*2, Width=sliderWidth+sliderWidth/10, Dock=Gwen.Pos.Top)
            let notchCount = if step = this.Zero then 0 else int((this.ToFloat maxValue - this.ToFloat minValue) / this.ToFloat step)
            slider <- new HorizontalSlider(panel, Min=this.ToFloat minValue, Max=this.ToFloat maxValue, Value=this.GetValue(), 
                                           SnapToNotches=(notchCount>0), NotchCount=notchCount, 
                                           Width=sliderWidth*17/20, Height=lineHeight, Dock=Gwen.Pos.Left)
            numField <- new TextBoxNumeric(panel, Height=lineHeight, Width=sliderWidth*3/20, AutoSizeToContents=true, 
                                           Value=slider.Value, IsDisabled=true, Dock=Gwen.Pos.Left)
            slider.add_ValueChanged(fun _base _args -> numField.Value <- slider.Value)
    
        override this.Update() =
            if slider |> isNull |> not && slider.Value <> this.GetValue() then
                this.ConfigRef := Optic.set lens (this.FromFloat slider.Value) (!this.ConfigRef)
    

    type FloatSliderPropertyEditor<'TConfig>(name : string, lens : Lens<'TConfig, float32>, minValue: float32, maxValue: float32, step: float32) =
        inherit AbstractSliderPropertyEditor<'TConfig, float32>(name, lens, minValue, maxValue, step)
        override this.Zero = 0.0f
        override this.ToFloat(x) = float32 x
        override this.FromFloat(x) = x
    

    type IntSliderPropertyEditor<'TConfig>(name : string, lens : Lens<'TConfig, int>, minValue: int, maxValue: int, step: int) =
        inherit AbstractSliderPropertyEditor<'TConfig, int>(name, lens, minValue, maxValue, step)
        override this.Zero = 0
        override this.ToFloat(x) = float32 x
        override this.FromFloat(x) = int(x)
    
    type DirectionPropertyEditor<'TConfig>(name : string, lens : Lens<'TConfig, Vector2>, step: float32) =
        inherit FloatSliderPropertyEditor<'TConfig>(name, (lens >-> DirectionPropertyEditor<'TConfig>.LensForVector), -180.0f, 180.0f, step)
        static member LensForVector : Lens<Vector2, float32> = (fun p -> p.Theta / Single.radiansInDegree), (fun a p -> Point(cos (deg2rad a), sin (deg2rad a)))
    

    type IntNumberboxPropertyEditor<'TConfig>(name : string, lens : Lens<'TConfig, int>) =
        inherit ConfigPropertyEditor<'TConfig>()
        let mutable numberbox : NumericUpDown = null

        override this.Initialize(parent) =
            let panel = new Layout.Splitter(parent, Height=lineHeight, Dock=Gwen.Pos.Top)
            let _label = new Label(panel, Text=name, Height=lineHeight, Dock=Gwen.Pos.Left)
            numberbox <- new NumericUpDown(panel, Value=float32(Optic.get lens (!this.ConfigRef)), Height=lineHeight, Width=lineHeight*3, Dock=Gwen.Pos.Right)
    
        override this.Update() =
            if numberbox |> isNull |> not && (int numberbox.Value) <> Optic.get lens (!this.ConfigRef) then
                this.ConfigRef := Optic.set lens (int numberbox.Value) (!this.ConfigRef)


    type CheckboxPropertyEditor<'TConfig>(name : string, lens : Lens<'TConfig, bool>) =
        inherit ConfigPropertyEditor<'TConfig>()
        let mutable checkbox : LabeledCheckBox = null

        override this.Initialize(parent) =
            checkbox <- new LabeledCheckBox(parent, Text=name, IsChecked=(Optic.get lens (!this.ConfigRef)), Height=lineHeight, Dock=Gwen.Pos.Top)
            if scaleFactor <> 1.0 then 
                checkbox.Children.[0].Width <- checkbox.GetChildrenSize().Y
    
        override this.Update() =
            if checkbox |> isNull |> not && checkbox.IsChecked <> Optic.get lens (!this.ConfigRef) then
                this.ConfigRef := Optic.set lens checkbox.IsChecked (!this.ConfigRef)
     
    type RadioButtonPropertyEditor<'TConfig, 'TValue when 'TValue: equality>(name : string, lens : Lens<'TConfig, 'TValue>, options: 'TValue list) =
        inherit ConfigPropertyEditor<'TConfig>()
        let mutable radioGroup : RadioButtonGroup = null

        override this.Initialize(parent) =
            radioGroup <- new RadioButtonGroup(parent, Text=name, Height=lineHeight*2, Dock=Gwen.Pos.Top)
            radioGroup.AutoSizeToContents <- false
            for opt in options do
                let radioButton = radioGroup.AddOption(sprintf "%A" opt)
                radioButton.Dock <- Gwen.Pos.Left ||| Gwen.Pos.CenterH
                if scaleFactor <> 1.0 then 
                    radioButton.Margin <- Margin(0, lineHeight*3/4, 0, 0)
            let currValue = Optic.get lens (!this.ConfigRef)
            radioGroup.SetSelection(List.findIndex (fun each -> each = currValue) options)
        
        override this.Update() =
            if radioGroup |> isNull |> not then
                let currIndex = radioGroup.SelectedIndex
                let currValue = Optic.get lens (!this.ConfigRef)
                if currValue <> options.[currIndex] then
                    this.ConfigRef := Optic.set lens (options.[currIndex]) (!this.ConfigRef)
     
    type DropdownListPropertyEditor<'TConfig, 'TValue when 'TValue: equality>(name : string, lens : Lens<'TConfig, 'TValue>, options: 'TValue list) =
        inherit ConfigPropertyEditor<'TConfig>()
        let mutable dropdownList : ComboBox = null
        let mutable items : MenuItem[] = [||]

        member val Labels : string[] = [||] with get, set

        override this.Initialize(parent) =
            let panel = new GroupBox(parent, Text=name, Height=lineHeight*2, Dock=Gwen.Pos.Top)
            dropdownList <- new ComboBox(panel, Height=lineHeight, Dock=Gwen.Pos.Top)
            items <- 
                options 
                |> List.mapi (
                    fun i opt -> 
                        let label = if this.Labels.Length = options.Length then this.Labels.[i] else sprintf "%A" opt
                        dropdownList.AddItem(label))
                |> List.toArray

        override this.Update() = 
            if dropdownList |> isNull |> not then
                let currIndex = items |> Array.findIndex (fun x -> x = dropdownList.SelectedItem)
                let currValue = Optic.get lens (!this.ConfigRef)
                if currIndex >= 0 && currValue <> options.[currIndex] then
                    this.ConfigRef := Optic.set lens (options.[currIndex]) (!this.ConfigRef)
    
    type Vector2PropertyEditor<'TConfig>(name : string, lens : Lens<'TConfig, Vector2>, minValue: float32, maxValue: float32, step: float32) =
        inherit ConfigPropertyEditor<'TConfig>()

        static let xLens : Lens<Vector2, float32> = (fun v -> v.X) , (fun x v -> Vector2(x, v.Y))
        static let yLens : Lens<Vector2, float32> = (fun v -> v.Y) , (fun y v -> Vector2(v.X, y))

        let sliderX = FloatSliderPropertyEditor("x", LibDworkin.Aether.Compose.lens lens xLens, minValue, maxValue, step)
        let sliderY = FloatSliderPropertyEditor("y", LibDworkin.Aether.Compose.lens lens yLens, minValue, maxValue, step)

        override this.Initialize(parent) =
            let panel = new GroupBox(parent, Text=name, Height=lineHeight*2, Dock=Gwen.Pos.Top)
            sliderX.ConfigRef <- this.ConfigRef
            sliderY.ConfigRef <- this.ConfigRef
            sliderX.Initialize(panel)
            sliderY.Initialize(panel)
            panel.AutoSizeToContents <- true

        override this.Update() = 
            sliderX.Update()
            sliderY.Update()

    type ColorPropertyEditor<'TConfig>(name : string, lens : Lens<'TConfig, OpenTK.Graphics.Color4>) =
        inherit ConfigPropertyEditor<'TConfig>()
        let mutable colorControl : HSVColorPicker = null

        override this.Initialize(parent) =
            let panel = new GroupBox(parent, Text=name, Dock=Gwen.Pos.Top, AutoSizeToContents=true)
            let picker = new HSVColorPicker(panel, Dock=Gwen.Pos.Top)
            // remove non-essential stuff
            for i=3 to 9 do
                picker.RemoveChild(picker.Children.[3], true)
            picker.Children.[2].Width <- picker.Children.[1].Width
            picker.Children.[2].Dock <- Gwen.Pos.Left
            picker.SizeToChildren(true, false) |> ignore
            picker.add_ColorChanged(Base.GwenEventHandler(fun _ _ -> 
                let c = picker.SelectedColor
                let newValue = OpenTK.Graphics.Color4(c.R, c.G, c.B, c.A)
                this.ConfigRef := Optic.set lens newValue (!this.ConfigRef)))
            colorControl <- picker

        override this.Update() = 
            if colorControl |> isNull |> not then
                let currValue = Optic.get lens (!this.ConfigRef)
                let currColor = System.Drawing.Color.FromArgb(currValue.ToArgb())
                if currColor <> colorControl.SelectedColor then
                    colorControl.SetColor(currColor)


    /// GUI that sets or modifies some property of Renderer (environment config, effect, material, ...)
    [<AbstractClass>]
    type IRendererPropertyEditor() =
        inherit AbstractGui()
        abstract Renderer : DworkinRenderer option with get, set
        abstract Name : string with get
        abstract SetProperty : unit -> unit


    type EnvironmentConfigEditor<'TConfig when 'TConfig :> DworkinEnvironmentConfig>(name : string, 
                                                                                     config : 'TConfig, 
                                                                                     propertyGuis : ConfigPropertyEditor<'TConfig> list) =
        inherit IRendererPropertyEditor()
        let configRef = ref config
        let mutable lastConfig = config

        override val Renderer : DworkinRenderer option = None with get, set
    
        override this.Name = name
        member this.Config = !configRef

        override this.Initialize(panel : Base) =
            for gui in propertyGuis do 
                gui.ConfigRef <- configRef
                gui.Initialize(panel)
    
        override this.SetProperty() =
            match this.Renderer with
            | Some(renderer) -> renderer.EnvironmentConfig <- !configRef
            | None -> ()

        override this.Update() = 
            for gui in propertyGuis do 
                gui.Update()
            if not(Unchecked.equals lastConfig !configRef) then
                this.SetProperty()
                lastConfig <- !configRef
    

    type SettingsEditorBuilder<'TSettings when 'TSettings :> Settings.Settings>(property : System.Reflection.PropertyInfo, 
                                                                                                  name : string,
                                                                                                  changedHolder : bool ref) =
        member this.GetLens<'T>() =
            let getDefinedValue (Settings.Randomizeable.Defined(value)) = value
            (fun s -> property.GetValue(box s) |> unbox<Settings.Randomizeable<'T>> |> getDefinedValue),
            (fun (v : 'T) s -> property.SetValue(box s, box(Settings.Randomizeable.Defined(v))); changedHolder := true; s)
        
        member this.UnboxList<'T> (value : obj list) = 
            value |> List.map unbox<'T> 

        interface Settings.ISettingsUIBuilder<ConfigPropertyEditor<'TSettings>> with
            override this.BuildEnumControl(options, optionType, _) = 
                let editorType = typeof<DropdownListPropertyEditor<_,_>>.GetGenericTypeDefinition().MakeGenericType([| typeof<'TSettings>; optionType |])
                let constructor = editorType.GetConstructors().[0]
                let thisType = this.GetType()
                let lensGetter = thisType.GetMethod("GetLens").MakeGenericMethod([| optionType |])
                let unboxer = thisType.GetMethod("UnboxList").MakeGenericMethod([| optionType |])
                let typedOptions = unboxer.Invoke(this, [| options |> List.ofArray |])
                constructor.Invoke([| box name; lensGetter.Invoke(this, [||]); typedOptions |]) |> unbox<ConfigPropertyEditor<'TSettings>>
            
            override this.BuildIntRangeControl(minVal, maxVal) = 
                IntSliderPropertyEditor<'TSettings>(name, this.GetLens(), minVal, maxVal, 1) :> ConfigPropertyEditor<'TSettings>
            
            override this.BuildFloatRangeControl(minVal, maxVal, step) = 
                FloatSliderPropertyEditor<'TSettings>(name, this.GetLens(), minVal, maxVal, step) :> ConfigPropertyEditor<'TSettings>
            
            override this.BuildBoolControl() =
                CheckboxPropertyEditor<'TSettings>(name, this.GetLens()) :> ConfigPropertyEditor<'TSettings>
            
            override this.BuildColorControl() = 
                let lens = this.GetLens() >-> (Graphics.Color4.FromRGBTriple, fun (c : Graphics.Color4) -> c.AsRGBTriple)
                ColorPropertyEditor<'TSettings>(name, lens) :> ConfigPropertyEditor<'TSettings>
    

    type SettingsEditor<'TSettings when 'TSettings :> Settings.Settings>(name : string, settings : 'TSettings) =
        inherit IRendererPropertyEditor()
        
        member val Changed : bool ref = ref false
        member val SettingsGuis : ResizeArray<ConfigPropertyEditor<'TSettings>> = ResizeArray<ConfigPropertyEditor<'TSettings>>()
        
        override val Renderer : DworkinRenderer option = None with get, set
    
        override this.Name = name
        
        override this.Initialize(panel : Base) : unit =
            for prop in settings.SettingProperties do
                let attr = prop.GetCustomAttributes(typeof<Settings.SettingAttribute>, true).[0] :?> Settings.SettingAttribute
                let builder = SettingsEditorBuilder(prop, prop.Name, this.Changed)
                let control : ConfigPropertyEditor<'TSettings> = attr.BuildUIControl(builder)
                control.ConfigRef <- ref settings
                this.SettingsGuis.Add(control)
                control.Initialize(panel)

        override this.Update() = 
            for gui in this.SettingsGuis do 
                gui.Update()
            if !this.Changed then
                this.SetProperty()
                this.Changed := false

        override this.SetProperty() = ()


    type EnvironmentSettingsEditor<'TSettings when 'TSettings :> DworkinSettings.EnvironmentSettings>(name : string, 
                                                                                                      settings : 'TSettings) as this =
        inherit SettingsEditor<'TSettings>(name, settings)
        let randomSeedControl = 
            IntNumberboxPropertyEditor<EnvironmentSettingsEditor<'TSettings>>("Seed", 
                                                                                ((fun e -> e.RandomSeed), 
                                                                                 (fun v e -> e.RandomSeed <- v; this.Changed := true; e)))
        member val RandomSeed = 42 with get, set

        override this.Initialize(panel : Base) =
            base.Initialize(panel)
            randomSeedControl.ConfigRef <- ref this
            randomSeedControl.Initialize(panel)

        override this.Update() =
            randomSeedControl.Update()
            base.Update()
        
        override this.SetProperty() =
            match this.Renderer with
            | Some(renderer) -> renderer.EnvironmentConfig <- settings.Build(this.RandomSeed)
            | None -> ()
    

    type ForegroundSettingsEditor<'TSettings when 'TSettings :> DworkinSettings.MaterialSettings>(name : string, settings : 'TSettings, 
                                                                                                  settingsHolder : SettingsHolder ref) =
        inherit SettingsEditor<'TSettings>(name, settings)
        override this.SetProperty() =
            settingsHolder := { !settingsHolder with Foreground=settings }
    

    type BackgroundSettingsEditor<'TSettings when 'TSettings :> DworkinSettings.MaterialSettings>(name : string, settings : 'TSettings, 
                                                                                                  settingsHolder : SettingsHolder ref) =
        inherit SettingsEditor<'TSettings>(name, settings)
        override this.SetProperty() =
            settingsHolder := { !settingsHolder with Background=settings }
    

    type EffectSettingsEditor<'TSettings when 'TSettings :> DworkinSettings.EffectSettings>(name : string, settings : 'TSettings, 
                                                                                            settingsHolder : SettingsHolder ref) =
            inherit SettingsEditor<'TSettings>(name, settings)
            override this.SetProperty() =
                settingsHolder := { !settingsHolder with Effect=settings }


    type RendererComponentEditor<'T when 'T :> ShaderBasedComponent>(name : string, 
                                                                     platform : AbstractPlatform, 
                                                                     initialParams, 
                                                                     propertyGuis : ConfigPropertyEditor<RendererComponentFactory<'T>> list, 
                                                                     setter : DworkinRenderer -> 'T -> unit ) =
        inherit IRendererPropertyEditor()
        member val Factory = RendererComponentFactory<'T>(platform, initialParams)

        override val Renderer : DworkinRenderer option = None with get, set
    
        override this.Name = name

        override this.Initialize(panel : Base) =
            for gui in propertyGuis do 
                gui.ConfigRef <- ref this.Factory
                gui.Initialize(panel)
    
        override this.SetProperty() =
            match this.Renderer with
            | Some(renderer) -> setter renderer (this.Factory.Build())
            | None -> ()

        override this.Update() = 
            for gui in propertyGuis do gui.Update()
            if this.Factory.Changed then
                this.SetProperty()
    
    
    /// GUI that lets user choose and edit some config
    type Chooser(renderer : DworkinRenderer, name : string, guis : IRendererPropertyEditor list) = 
        inherit AbstractGui()

        let mutable lastConfigName = ""
        let mutable groupBox : GroupBox = null
        let mutable currentPanel : GroupBox = null
        let mutable combobox : ComboBox = null

        member internal this.ComboBox = combobox

        member internal this.CurrentConfigGui =
            combobox.SelectedItem.UserData :?> IRendererPropertyEditor

        member private this.SetPanelForItem(item : MenuItem) =
            groupBox.RemoveChild(currentPanel, false)
            currentPanel <- new GroupBox(groupBox, Dock=Gwen.Pos.Top)
            currentPanel.AutoSizeToContents <- true
            currentPanel.ShouldDrawBackground <- true
            (item.UserData :?> AbstractGui).Initialize(currentPanel)
            currentPanel.SizeToChildren() |> ignore
            lastConfigName <- combobox.SelectedItem.Name
        
        abstract UpdateValue : IRendererPropertyEditor * bool -> unit
        default this.UpdateValue(gui, isNewValue) =
            if isNewValue then gui.SetProperty()
            else gui.Update()

        abstract InitializeGuis : unit -> unit
        default this.InitializeGuis() =
            for each in guis do
                let item = this.ComboBox.AddItem(each.Name, each.Name)
                item.UserData <- each
                each.Renderer <- Some(renderer)

        override this.Initialize(parent : Base) =
            groupBox <- new GroupBox(parent, Dock=Gwen.Pos.Top, Text=name)
            groupBox.ShouldDrawBackground <- true
            groupBox.AutoSizeToContents <- true
            
            combobox <- new ComboBox(groupBox, Height=lineHeight, Dock=Gwen.Pos.Top)

            this.InitializeGuis()
        
            this.SetPanelForItem(combobox.SelectedItem)
    
        override this.Update() = 
            if combobox |> isNull |> not then
                let currConfigGui = this.CurrentConfigGui
                if lastConfigName <> combobox.SelectedItem.Name then
                    this.SetPanelForItem(combobox.SelectedItem)
                    this.UpdateValue(currConfigGui, true)
                else
                    this.UpdateValue(currConfigGui, false)
    

    type ColorChangeMaterialEditor(platform : AbstractPlatform, backgroundChooser : Chooser, setter : DworkinRenderer -> Material -> unit ) =
        inherit IRendererPropertyEditor()
        static let lens : Lens<Graphics.Color4 ref, Graphics.Color4> = (fun cell -> !cell), (fun c cell -> cell := c; cell)
        let colorEditor = ColorPropertyEditor<Graphics.Color4 ref>("color change", lens)

        member val Factory : obj = null with get, set
        member val ModifiedFactory : obj = null with get, set

        member this.UpdateFactory() : bool =
            let foregroundEditor = backgroundChooser.CurrentConfigGui :> obj
            let oldFactory = this.Factory
            this.Factory <- foregroundEditor.GetType().GetProperty("Factory").GetValue(foregroundEditor)
            if oldFactory <> this.Factory then
                let factoryType = this.Factory.GetType()
                this.ModifiedFactory <- factoryType.GetMethod("With").Invoke(this.Factory, [| box "color"; box colorEditor.ConfigRef.Value.Value |])
                true
            else
                false

        override val Renderer : DworkinRenderer option = None with get, set
    
        override this.Name = "color change"

        override this.Initialize(panel : Base) =
            colorEditor.ConfigRef <- ref <| ref Graphics.Color4.Black
            this.UpdateFactory() |> ignore
            colorEditor.Initialize(panel)
    
        override this.SetProperty() =
            match this.Renderer with
            | Some(renderer) -> 
                let factoryType = this.ModifiedFactory.GetType()
                let newMaterial = factoryType.GetMethod("Build").Invoke(this.ModifiedFactory, [||]) :?> Material
                setter renderer newMaterial
            | None -> ()

        override this.Update() = 
            this.UpdateFactory() |> ignore
            colorEditor.Update()

            let factoryType = this.ModifiedFactory.GetType()
            let itemGetter = factoryType.GetProperty("Item")
            let oldColor = unbox<Graphics.Color4> <| itemGetter.GetValue(this.ModifiedFactory, [| "color" |])
            if colorEditor.ConfigRef.Value.Value <> oldColor then
                itemGetter.SetValue(this.ModifiedFactory, colorEditor.ConfigRef.Value.Value, [| "color" |])
                this.SetProperty()
    

    // used for editing foreground and background colors
    type MaterialsColorEditor(rendrer : DworkinRenderer,
                             platform : AbstractPlatform,
                             settingsHolder: SettingsHolder ref) =
        inherit IRendererPropertyEditor()
        let colorEditors = 
            let fgLens : Lens<SettingsHolder, OpenTK.Graphics.Color4> = 
                let getter h = 
                    match h.ForegroundColor with | Settings.Randomizeable.Defined(c) -> OpenTK.Graphics.Color4.FromRGBTriple(c)
                let setter (c : Graphics.Color4) h = 
                    { h with ForegroundColor=Settings.Randomizeable.Defined(c.AsRGBTriple) }
                getter, setter
            let bgLens : Lens<SettingsHolder, OpenTK.Graphics.Color4> = 
                let getter h = 
                    match h.BackgroundColor with | Settings.Randomizeable.Defined(c) -> OpenTK.Graphics.Color4.FromRGBTriple(c)
                let setter (c : Graphics.Color4) h = 
                    { h with BackgroundColor=Settings.Randomizeable.Defined(c.AsRGBTriple) }
                getter, setter
            [
                ColorPropertyEditor<LibDworkin.DworkinSettings.SettingsHolder>("Foreground", fgLens)
                ColorPropertyEditor<LibDworkin.DworkinSettings.SettingsHolder>("Background", bgLens)
            ]

        override this.Renderer
            with get() : DworkinRenderer option = Some(rendrer)
            and set(_) = ()

        override this.Name = "Colors"

        override this.Initialize(parent : Base) =
            let panel = new Control.GroupBox(parent, Dock=Gwen.Pos.Top, AutoSizeToContents=true)
            for editor in colorEditors do
                editor.ConfigRef <- settingsHolder
                editor.Initialize(panel)
            for each in panel.Children do
                each.Dock <- Gwen.Pos.Left
        
        override this.SetProperty() =
            match this.Renderer with
            | Some(renderer) -> renderer.ApplyChangesInSettings()
            | None -> ()

        override this.Update() = 
            for editor in colorEditors do
                editor.Update()
            if LibDworkin.DworkinSettings.CurrentSettings.get() <> !settingsHolder then
                this.SetProperty()


    type TabPane(children : (string * AbstractGui) list) =
        inherit AbstractGui()
        let mutable tabControl : TabControl = null

        override this.Initialize(parent : Base) =
            tabControl <- new TabControl(parent)
            tabControl.Dock <- Gwen.Pos.Fill
            for name, child in children do
                let panel = new GroupBox(tabControl, Dock=Gwen.Pos.Top)
                let page = tabControl.AddPage(name, panel)
                child.Initialize(panel)
                panel.AutoSizeToContents <- true
        
        override this.Update() =
            for _, child in children do
                child.Update()


    type ControlWindow(children : AbstractGui list, position : Pos, hSize, vSize : float) =
        inherit AbstractGui()
        let mutable window : WindowControl = null

        override this.Initialize(parent : Base) =
            let canvas = parent
            window <- new WindowControl(canvas)
            window.Position(position, guiPadding, guiPadding) 
            window.SetSize(hSize, int(float canvas.Height * vSize)) |> ignore 
            window.ShouldDrawBackground <- true

            for child in children do
                child.Initialize(window)
        
        override this.ToggleVisibility() =
            if window |> isNull |> not then
                if window.IsVisible then window.Hide() else window.Show()
        
        override this.Update() = 
            for child in children do
                child.Update()



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
    let chooser (name : string) (renderer : DworkinRenderer) (configGuis : IRendererPropertyEditor list) =
        Chooser(renderer, name, configGuis)
    
    let environmentEditor (name : string) (config : 'TConfig) (propertyGuis : ConfigPropertyEditor<'TConfig> list) =
        EnvironmentConfigEditor(name, config, propertyGuis)
    
    let floatSliderEditor (name : string) (lens : Lens<'TConfig, float32>) (minValue : float32) (maxValue : float32) (step : float32) =
        FloatSliderPropertyEditor(name, lens, minValue, maxValue, step)

    let intSliderEditor (name : string) (lens : Lens<'TConfig, int>) (minValue : int) (maxValue : int) (step : int) =
        IntSliderPropertyEditor(name, lens, minValue, maxValue, step)
    
    let checkboxEditor (name : string) (lens : Lens<'TConfig, bool>) =
        CheckboxPropertyEditor(name , lens)
    
    let radioButtonEditor (name : string) (lens : Lens<'TConfig, 'TValue>) (options: 'TValue list) =
        RadioButtonPropertyEditor(name, lens, options)
    
    let dropdownListEditor (name : string) (lens : Lens<'TConfig, 'TValue>) (options: 'TValue list) =
        DropdownListPropertyEditor(name, lens, options)

    let labeledDropdownListEditor (name : string) (lens : Lens<'TConfig, 'TValue>) (options: ('TValue * string) list) =
        let res = DropdownListPropertyEditor(name, lens, options |> List.map fst)
        res.Labels <- options |> List.map snd |> List.toArray
        res
    
    let numberboxEditor (name : string) (lens : Lens<'TConfig, int>) =
        IntNumberboxPropertyEditor(name, lens)
    
    let getLens<'T, 'TValue when 'T :> ShaderBasedComponent> (name : string) =
        RendererComponentFactory<'T>.LensFor<'TValue>(name)
    
    let shaderBasedComponentEditor<'T when 'T :> ShaderBasedComponent> 
        (platform : AbstractPlatform) 
        (setter : DworkinRenderer -> 'T -> unit) 
        (name : string) 
        (initialParams : IDictionary<string, obj>)
        (propertyGuis : ConfigPropertyEditor<RendererComponentFactory<'T>> list) =
        RendererComponentEditor<'T>(name, platform, initialParams, propertyGuis, setter) :> IRendererPropertyEditor
    
    let singleControlWindow (position : Pos) (hSize : int) (vSize : float) (control : AbstractGui) =
        ControlWindow([control], position, hSize, vSize)
    
    let window (position : Pos) (vSize : float) (hSize : int) (children : AbstractGui list) =
        ControlWindow(children, position, hSize, vSize)
    
    let tabPane (children : (string * AbstractGui) list) =
        TabPane(children)

    module ShaderComponents =
        let floatSliderEditor (minValue : float32) (maxValue : float32) (step : float32) (name : string) =
            (floatSliderEditor name (getLens<'TConfig, float32> name) minValue maxValue step) :> ConfigPropertyEditor<_>
        
        let colorEditor (name : string) =
            ColorPropertyEditor(name, getLens<_, _> name) :> ConfigPropertyEditor<_>
        
        let checkboxEditor (name : string) =
            CheckboxPropertyEditor(name, getLens<_, _> name) :> ConfigPropertyEditor<_>
        
        let vec2Editor (minValue : float32) (maxValue : float32) (step : float32) (name : string) =
            Vector2PropertyEditor(name, getLens<_, _> name, minValue, maxValue, step) :> ConfigPropertyEditor<_>
        
        let directionEditor (step : float32) (name : string) =
            DirectionPropertyEditor(name, getLens<_, _> name, step) :> ConfigPropertyEditor<_>
        
        let radioButtonEditor (options: 'TValue list) (name : string) =
            RadioButtonPropertyEditor(name, getLens<_, _> name, options) :> ConfigPropertyEditor<_>
    
        let dropdownListEditor (options: 'TValue list) (name : string) =
            DropdownListPropertyEditor(name, getLens<_, _> name, options) :> ConfigPropertyEditor<_>