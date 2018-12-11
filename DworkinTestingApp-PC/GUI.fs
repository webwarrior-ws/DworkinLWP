module GUI

open OpenTK
open Gwen


type Window(width : int, height : int) as this =
    inherit GameWindow(width, height)

    let input = Gwen.Input.OpenTK(this)
    let renderer = new Gwen.Renderer.OpenTK()
    let skin = new Gwen.Skin.TexturedBase(renderer, "DefaultSkin.png")
    let canvas = new Gwen.Control.Canvas(skin)

    member this.Canvas = canvas
    
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