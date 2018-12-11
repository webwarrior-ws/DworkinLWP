module Program

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.ES20

open LibDworkin.Renderer


type MainWindow(renderer : BaseRenderer) =
    inherit GUI.Window(1280, 720)

    override this.OnLoad(e) = 
        base.OnLoad(e)
        GL.ClearColor(0.0f, 0.0f, 0.0f, 0.0f)
        renderer.OnSurfaceCreated()
        // setup gui
        let control = new Gwen.Control.MessageBox(this.Canvas, "hello world")
        control.SetPosition(20, 20)
    
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
    let renderer = DworkinRenderer(NETCorePlatform(), GeneratedConfigs.PCB_RandomPosChips())
    let window = new MainWindow(renderer)
    window.Run(30.0)

    0 // return an integer exit code
