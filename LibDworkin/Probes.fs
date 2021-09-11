module LibDworkin.Probes

open Agents
open Colliders
open PharoCompatibility
open AgentAbstractions
open SpecializedCollections



type internal SingleAttributeStore<'T>() =
    let mutable storedValue : 'T voption = ValueNone
    interface IAttributeAccessor<'T> with
        member this.Get() = 
            match storedValue with
            | ValueSome(value) -> Some(value)
            | ValueNone -> None

        member this.Get(value) = 
            match storedValue with 
            | ValueSome(x) -> x 
            | ValueNone -> value()
        
        member this.GetValue() = storedValue

        member this.GetOrSetIfAbsent(value) = 
            match storedValue with 
            | ValueSome(x) -> x 
            | ValueNone -> 
                let res = value()
                storedValue <- ValueSome(res)
                res
        
        member this.Set(value) =
            storedValue <- ValueSome(value)


type internal ProbeColliderAttributes() =
    let lastCollider = SingleAttributeStore<Collider>()
    interface IColliderAttributes with
        member this.LastCollider = lastCollider :> IAttributeAccessor<Collider>


type ProbeData() =
    member val Collision : Collision option = None with get, set
    member val Footprints : ResizeArray<DworkinAgentFootprint> = ResizeArray()

    member this.DistanceCovered =
        if this.Footprints.IsEmpty then
            0.0f
        else 
            (this.Footprints.Last :> IDworkinAgentState).DistanceCovered 

    member this.Id 
        with set(anInteger) =
            for each in this.Footprints do
                each.Id <- anInteger

    member this.ReachedEnd =
        if this.Footprints.IsEmpty then
            false
        else 
            (this.DistanceCovered + (1.0e-3f)) >= this.Footprints.Last.Config.TotalDistance 

    member inline this.S =
        this.DistanceCovered

    member this.SpawnColliders() =
        let attributes = ProbeColliderAttributes()
        for each in this.Footprints do
            each.Config.ColliderOptions.Evaluate(each, attributes, each.DworkinEnvironment)

    member this.UpdateFootprintsFor(aProbeAgent : DworkinProbeAgent) =
        this.Footprints.Add(aProbeAgent.GetFootprint() :?> DworkinAgentFootprint)


and DworkinProbeAgent(config, randomSeed, speedFactor : float32, stepsBetweenCollisionCheck : int) =
    inherit DworkinAgent(config, randomSeed)
    
    member this.SpeedFactor = speedFactor
    member this.StepsBetweenCollisionCheck = stepsBetweenCollisionCheck
    
    member this.HasToTerminate =
        if not <| this.LiesWithinBounds(this.DworkinEnvironment.ExtendedBounds) then
            true
        else
            (match this.Config.TerminationConditions.Distance with 
                | Some(cond) -> cond.Evaluate(this) 
                | None -> false) ||
            (match this.Config.TerminationConditions.Collision with 
                | Some(cond) when this.Age % stepsBetweenCollisionCheck = 0 -> cond.Evaluate(this) 
                | _ -> false) ||
            (match this.Config.TerminationConditions.Curvature with 
                | Some(cond) -> cond.Evaluate(this) 
                | None -> false)

    member this.Run() =
        let data = ProbeData()
        data.UpdateFootprintsFor(this)
        while not this.HasToTerminate do
            this.Step()
            data.UpdateFootprintsFor(this)
        data.Collision <- this.Collision
        data

    override this.Speed 
        with get() = base.Speed * speedFactor
        and set(aNumber) = base.Speed <- aNumber / speedFactor

    override this.Step() =
        this.MoveAndRotate()
        this.ResetCache()


type ProbeSpawnOptions() =
    inherit AgentSpawnOptions()
    member val SpeedFactor = 1.0f with get, set
    member val StepsBetweenCollisionCheck = 1 with get, set



type DworkinEnvironment with
    member this.LaunchProbe(probeConfig : DworkinAgentConfig, options : ProbeSpawnOptions, agentBlock : DworkinProbeAgent -> unit) =
        let seed = match options.Seed with | Some(value) -> value | None -> this.Random.Next()
        let probe = DworkinProbeAgent(probeConfig, seed, options.SpeedFactor, options.StepsBetweenCollisionCheck)
        probe.Environment <- Some(this :> Environment)
        options.Config <- Some(probeConfig)
        probe.Position <- options.Position
        probe.Direction <- options.Direction
        match options.Parent with 
        | Some(_) -> probe.Parent <- options.Parent 
        | None -> ()
        agentBlock(probe)
        probe.Run()
    
    member this.LaunchProbe(probeConfig, options) =
        this.LaunchProbe(probeConfig, options, ignore)


type DworkinAgent with
    member this.LaunchProbe(probeConfig : DworkinAgentConfig, options : ProbeSpawnOptions, agentBlock : DworkinProbeAgent -> unit) =
        options.Parent <- Some(this)
        this.DworkinEnvironment.LaunchProbe(
            probeConfig, 
            options,
            fun probe -> 
                probe.ThicknessFactor <- this.ThicknessFactor
                probe.LengthFactor <- this.LengthFactor
                probe.GenerationLimit <- this.GenerationLimit
                if this.IsFlipped then probe.Flip()
                agentBlock(probe))
    
    member this.LaunchProbe(probeConfig, options) =
        this.LaunchProbe(probeConfig, options, ignore)
