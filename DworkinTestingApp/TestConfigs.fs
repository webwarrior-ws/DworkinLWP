module TestConfigs


open LibDworkin.Aether
open LibDworkin.Geometry
open LibDworkin.AgentAbstractions
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.Brushes
open Android.Widget



type Termination_Spiral() =
    inherit TerminationCustom()
    member val Gap = 10.0f with get, set

    override this.Evaluate(agent, collision) =
        match collision with
        | None -> 
            agent.Spawn(
                agent.Config |> Optic.map DworkinAgentConfig.TotalDistance_ (fun x -> x - this.Gap), 
                AgentSpawnOptions(AngleDeg=90.0f))
        | Some(_) -> ()


let brushesAndSpeedsTest : DworkinEnvironmentConfig =
    let agent_Solid_Adaptive =
        DworkinAgentConfig.Default
    let agent_SolidWithJoint =
        DworkinAgentConfig.Default
        |> Optic.set DworkinAgentConfig.Termination_ (Termination_Spiral() :> TerminationStrategy)
    let agent_SolidWithJoint_Triangle =
        DworkinAgentConfig.Default 
        |> Optic.set DworkinAgentConfig.Brush_ (BrushStrategySolidJoint() :> BrushStrategy)
    let agent_SolidWithJoint_Quad =
        DworkinAgentConfig.Default 
        |> Optic.set DworkinAgentConfig.Brush_ (BrushStrategySolidJoint(JointType=BrushJoint.Quad) :> BrushStrategy)
    let agent_Circles_Thickness =
        DworkinAgentConfig.Default 
        |> Optic.set DworkinAgentConfig.Brush_ (BrushStrategyCircles() :> BrushStrategy)
        |> Optic.set DworkinAgentConfig.Speed_ (AgentSpeedThickness() :> AgentSpeed)
    
    { new DworkinEnvironmentConfig with
        member this.GetEnvironment(size) = 
            let env = DworkinEnvironment(Rectangle(Point(), size), 42)
            let topY = size.Y * 0.33f
            let bottomY = size.Y * 0.67f

            match env.Spawn(agent_Solid_Adaptive, AgentSpawnOptions(Position=Point(0.1f * size.X, topY))) with
            | Some(:? DworkinAgent as agent) -> agent.ScaleBy(5.0f)
            | _ -> ()



            env }