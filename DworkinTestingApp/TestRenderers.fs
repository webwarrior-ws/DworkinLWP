module DworkinTestingApp.TestRenderers

open OpenTK
open OpenTK.Graphics
open OpenTK.Graphics.ES20
open Javax.Microedition.Khronos

open LibDworkin.Geometry
open LibDworkin.Renderer
open LibDworkin.RendererComponents
open LibDworkin.AgentAbstractions



type BenchmarkRenderer(context, settingsRef) =
    inherit DworkinRenderer(context, settingsRef, SimpleEnvironmentStepper())

    let shapesQueue = System.Collections.Generic.Queue<ShapeAndPaint[]>()
    
    override this.ResetEnvironment(dims) =
        base.ResetEnvironment(dims)

    override this.OnSurfaceChanged(width, height) =
        base.OnSurfaceChanged(width, height)
        this.StatsPrinted <- false
        shapesQueue.Clear()
        while this.EnvironmentStepper.HasMore do
            shapesQueue.Enqueue(this.EnvironmentStepper.FetchShapesForNextStep())

    override this.OnDrawFrame() = 
        this.ApplyChangesInSettings()

        if shapesQueue.Count > 0 then
            let mutable t = 0.0
            while t < 33.3 && shapesQueue.Count > 0 do
                let shapes = shapesQueue.Dequeue()
                for _ in TimeSamplers.Shapes.Measure() do this.RenderShapes(shapes)
                for _ in TimeSamplers.Image.Measure() do this.DrawImage()
                t <- t + TimeSamplers.Shapes.Last + TimeSamplers.Image.Last
        elif not this.StatsPrinted then
            let printStats (samples : float[]) (name : string) =
                Android.Util.Log.Debug("avgTimes", 
                    sprintf "(%-6s) avg: %6.2f max: %6.2f top1%%: %6.2f stdev: %6.2f"
                        name
                        (TimeStats.average samples)
                        (TimeStats.max samples)
                        (TimeStats.top 0.01 samples)
                        (TimeStats.stdev samples) ) |> ignore
            
            printStats TimeSamplers.Environment.Samples TimeSamplers.Environment.Name
            printStats TimeSamplers.Shapes.Samples TimeSamplers.Shapes.Name
            printStats TimeSamplers.Image.Samples TimeSamplers.Image.Name
            printStats (TimeStats.add TimeSamplers.Shapes.Samples TimeSamplers.Image.Samples) "GPU"
            for windowSize in [5; 10] do
                let averages = TimeStats.movingAverage windowSize TimeSamplers.Environment.Samples
                Android.Util.Log.Debug("avgTimes", 
                    sprintf "(env. moving average %2d) avg: %6.2f max: %6.2f top1%%: %6.2f"
                        windowSize
                        (TimeStats.average averages)
                        (TimeStats.max averages)
                        (TimeStats.top 0.01 averages)) |> ignore

            this.StatsPrinted <- true