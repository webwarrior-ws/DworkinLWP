module SettingAttributeTests

open Expecto
open Accord.Statistics.Testing
open Accord.Statistics.Distributions.Univariate

open Settings


let seed = 565467
let sampleSize = 1000

let acquireSamples (attr : SettingAttribute) : obj[] =
    let rng = System.Random(seed)
    Array.init sampleSize (fun _ -> attr.RandomValue(rng))

let getDistribution (samples : 'a[]) = 
    Seq.countBy id samples
    |> Seq.toArray


[<Tests>]
let randomValueTests =
    testList "SettingAttribute.RandomValue() tests" 
        [
            testCase "EnumAttribute" <| fun _ ->
                let itemsCount = 10
                let items = Array.init itemsCount (fun _ -> obj())
                let samples = acquireSamples (EnumAttribute(items))
                Expect.containsAll samples items "must conatin all items"
                let chiSquaredTest = 
                    ChiSquareTest(getDistribution samples |> Array.map (snd >> float), 
                                  Array.create itemsCount (float (sampleSize/itemsCount)), itemsCount-1)
                Expect.isFalse chiSquaredTest.Significant "must pass chi-squared test"
            
            testCase "IntRangeAttribute" <| fun _ ->
                let low, high = -2, 8
                let range = [| low..high |]
                let itemsCount = range.Length
                let samples = acquireSamples (IntRangeAttribute(low, high)) |> Array.map unbox<int>
                Expect.all samples (fun x -> x >= low && x <= high) "must be within range"
                Expect.containsAll samples range "must conatin all values"
                let distribution = getDistribution samples
                let chiSquaredTest = 
                    ChiSquareTest(distribution |> Array.map (snd >> float), 
                                  Array.create itemsCount (float (sampleSize/itemsCount)), itemsCount-1)
                Expect.isFalse chiSquaredTest.Significant "must pass chi-squared test"
            
            testCase "FloatRangeAttribute" <| fun _ ->
                let low, high = -2.0f, 8.0f
                let samples = acquireSamples (FloatRangeAttribute(low, high)) |> Array.map unbox<float32>
                Expect.all samples (fun x -> x >= low && x <= high) "must be within range"
                let chiSquaredTest = 
                    ChiSquareTest(samples |> Array.map float, UniformContinuousDistribution(float low, float high))
                Expect.isFalse chiSquaredTest.Significant "must pass chi-squared test"
            
            testCase "FloatRangeAttribute (with step)" <| fun _ ->
                // if Step is given, random values should be in increments of Step
                let low, high = -1.0f, 1.0f
                let step = 0.5f
                let expectedValues = [| low .. step .. high |] // [|-1.0f; -0.5f; 0.0f; 0.5f; 1.0f|]
                let itemsCount = expectedValues.Length
                let samples = acquireSamples (FloatRangeAttribute(low, high, Step=step)) |> Array.map unbox<float32>
                Expect.all samples (fun x -> x >= low && x <= high) "must be within range"
                let distribution = getDistribution samples
                let chiSquaredTest = 
                    ChiSquareTest(distribution |> Array.map (snd >> float), 
                                  Array.create itemsCount (float (sampleSize/itemsCount)), itemsCount-1)
                Expect.isFalse chiSquaredTest.Significant "must pass chi-squared test"
            
            testCase "ColorAttribute" <| fun _ ->
                let samples = acquireSamples (ColorAttribute()) |> Array.map unbox<float32*float32*float32>
                Expect.all 
                    samples 
                    (fun (r, g, b) -> [|r;g;b|] |> Array.forall (fun x -> x >= 0.0f && x <= 1.0f) ) 
                    "all components must be within [0;1] range"
                let colorSamples = 
                    samples 
                    |> Array.map (fun (r,g,b) -> System.Drawing.Color.FromArgb(int(r*255.0f), int(g*255.0f), int(b*255.0f)))
                let hSamples = colorSamples |> Array.map (fun c -> c.GetHue()/360.0f |> float)
                let sSamples = colorSamples |> Array.map (fun c -> c.GetSaturation() |> float)
                let bSamples = colorSamples |> Array.map (fun c -> c.GetBrightness() |> float)
                let uniform0to1distribution = UniformContinuousDistribution(0.0, 1.0)
                Expect.isFalse (ChiSquareTest(hSamples, uniform0to1distribution)).Significant "hue must pass chi-squared test"
                // TODO: make color random generator that passes saturation and brightness chi-squared tests (?)
                //Expect.isFalse (ChiSquareTest(sSamples, uniform0to1distribution)).Significant "saturation must pass chi-squared test"
                //Expect.isFalse (ChiSquareTest(bSamples, uniform0to1distribution)).Significant "values must pass chi-squared test"
            
            testCase "BoolAttribute" <| fun _ ->
                let samples = acquireSamples (BoolAttribute()) |> Array.map unbox<bool>
                let distribution = getDistribution samples
                let chiSquaredTest = 
                    ChiSquareTest(distribution |> Array.map (snd >> float), Array.create 2 (float (sampleSize/2)), 1)
                Expect.isFalse chiSquaredTest.Significant "must pass chi-squared test"
        ]
