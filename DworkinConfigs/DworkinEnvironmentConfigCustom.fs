namespace DworkinEnvironmentConfigCustom


open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Geometry
open LibDworkin.AgentAbstractions
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.Lenses
open LibDworkin.SpecializedCollections
open LibDworkin.PharoCompatibility
open LibDworkin.Colliders
open LibDworkin.Probes
open LibDworkin.SimplexNoise

open MiscConfigs
open LibDworkin



type SpiralsProbesBranching1() = 
    inherit DworkinEnvironmentConfigCustom()
    member val Config = DworkinAgentConfig.swirls2_7_fast_ with get, set
    member val N = 2 with get, set
    member val LengthRange = NumberRange(50.0f, 200.0f) with get, set
    member val Delay = 100 with get, set

    member this.Clone() = this.MemberwiseClone() :?> SpiralsProbesBranching1

    static member Config_ : Lens<SpiralsProbesBranching1, _> = 
        (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
    static member N_ : Lens<SpiralsProbesBranching1, _> = 
        (fun x -> x.N), (fun value x -> let y = x.Clone() in y.N <- value; y)
    static member LengthRange_ : Lens<SpiralsProbesBranching1, _> = 
        (fun x -> x.LengthRange), (fun value x -> let y = x.Clone() in y.LengthRange <- value; y)
    static member Delay_ : Lens<SpiralsProbesBranching1, _> = 
        (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)

    override this.Evaluate(env) =
        let c = env.Bounds.Center
        Seq.iter
            (fun p -> 
                let d = p - c
                env.AddCollider(SegmentCollider(None, p, Point(c.X + (float32(sign d.X) * 0.001f), p.Y)))
                env.AddCollider(SegmentCollider(None, p, Point(p.X, c.Y + (float32(sign d.Y) * 0.001f)))))
            env.Bounds.Corners
        let d = env.Bounds.Width / float32(this.N + 1)
        Seq.iter
            (fun x -> 
                let finalConfig = this.Config|> Optic.set totalDistance_ (env.Random.NextInRange this.LengthRange)
                let spawnOptions = ProbeSpawnOptions(Position=Point(x, env.Bounds.Top + 1.0f), Direction=Point(0.0f, 1.0f))
                spawnOptions.DelayBy(int(env.Random.NextFloat() * float32(this.Delay)))
                let probeData = env.LaunchProbe(finalConfig, spawnOptions)
                env.Spawn(
                    finalConfig|> Optic.set totalDistance_ probeData.S, 
                    spawnOptions, 
                    fun (a : DworkinAgent) -> 
                        probeData.Id <- a.Id
                        probeData.SpawnColliders()
                        a.NoCollisions <- true)
                    |> ignore)
            (seq { (env.Bounds.Left + d) .. d .. (env.Bounds.Right - d) })

    
type MagneticSpiralsBunchBranching2_3() = 
    inherit DworkinEnvironmentConfigCustom()
    member val Config = SpawnConfig.ProbeBunchMagneticSpirals2Branch3() with get, set
    member val Scale = 1.25f with get, set

    member this.Clone() = this.MemberwiseClone() :?> MagneticSpiralsBunchBranching2_3

    static member Config_ : Lens<MagneticSpiralsBunchBranching2_3, _> = 
        (fun x -> x.Config), (fun value x -> let y = x.Clone() in y.Config <- value; y)
    
    static member Scale_ : Lens<MagneticSpiralsBunchBranching2_3, _> = 
        (fun x -> x.Scale), (fun value x -> let y = x.Clone() in y.Scale <- value; y)

    override this.Evaluate(env) =
        let bounds = env.Bounds
        let c = bounds.Center
        for p in bounds.Corners do
            let d = p - c
            env.AddCollider(SegmentCollider(None, p, Point(c.X + (float32(sign d.X) * 0.001f), p.Y)))
            env.AddCollider(SegmentCollider(None, p, Point(p.X, c.Y + (float32(sign d.Y) * 0.001f))))
            
        env.Spawn(this.Config, AgentSpawnOptions(Position=env.Bounds.BottomCenter - Point(0.0f, 1.0f)), fun a -> a.ScaleBy(this.Scale))
