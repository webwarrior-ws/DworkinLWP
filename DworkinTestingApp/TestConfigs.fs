module TestConfigs


open LibDworkin.Aether
open LibDworkin.Geometry
open LibDworkin.AgentAbstractions
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.Brushes
open LibDworkin.Aether


type Termination_Spiral() =
    inherit TerminationCustom()
    member val Gap = 10.0f with get, set
    member val Angle = 90.0f with get, set

    override this.Evaluate(agent, collision) =
        match collision with
        | None -> 
            agent.Spawn(
                agent.Config |> Optic.map DworkinAgentConfig.TotalDistance_ (fun x -> x - this.Gap), 
                AgentSpawnOptions(AngleDeg=this.Angle), 
                fun a -> a.Attributes.DrawJoint.Set(true))
        | Some(_) -> ()


let brushesAndSpeedsTest : DworkinEnvironmentConfig =
    let agent_Solid_Adaptive =
        DworkinAgentConfig.Default
    let agent_SolidWithJoint =
        DworkinAgentConfig.Default
        |> Optic.set DworkinAgentConfig.Termination_ (Termination_Spiral() :> TerminationStrategy)
    let agent_SolidWithJoint_Triangle =
        agent_SolidWithJoint 
        |> Optic.set DworkinAgentConfig.Brush_ (BrushStrategySolidJoint() :> BrushStrategy)
    let agent_SolidWithJoint_Quad =
        agent_SolidWithJoint 
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

            let scale (agent : DworkinAgent) = agent.ScaleBy(Point(10.0f, 1.5f))
            let setDrawJointAndScale (agent : DworkinAgent) = 
                scale agent
                agent.Attributes.DrawJoint.Set(true)

            env.Spawn(agent_Solid_Adaptive, AgentSpawnOptions(Position=Point(0.1f * size.X, topY)), scale) |> ignore
            env.Spawn(agent_Circles_Thickness, AgentSpawnOptions(Position=Point(0.2f * size.X, topY)), fun a -> a.ScaleBy(Point(15.0f, 2.0f))) |> ignore

            // flipFactor=1, turn left
            env.Spawn(agent_SolidWithJoint_Triangle, AgentSpawnOptions(Position=Point(0.55f * size.X, topY)), setDrawJointAndScale) |> ignore
            env.Spawn(agent_SolidWithJoint_Quad, AgentSpawnOptions(Position=Point(0.9f * size.X, topY)), setDrawJointAndScale) |> ignore

            // flipFactor=-1, turn left
            let flipAndRest agent =
                setDrawJointAndScale agent
                agent.Flip()
            env.Spawn(agent_SolidWithJoint_Triangle, AgentSpawnOptions(Position=Point(0.05f * size.X, bottomY)), flipAndRest) |> ignore
            env.Spawn(agent_SolidWithJoint_Quad, AgentSpawnOptions(Position=Point(0.3f * size.X, bottomY)), flipAndRest) |> ignore

            // flipFactor=1, turn right, angle = 60 degrees
            let turnRight (config : DworkinAgentConfig) =
                let t = Termination_Spiral()
                t.Angle <- -60.0f
                config  |> Optic.set DworkinAgentConfig.Termination_ (t :> TerminationStrategy)
            env.Spawn(turnRight agent_SolidWithJoint_Triangle, AgentSpawnOptions(Position=Point(0.55f * size.X, bottomY)), setDrawJointAndScale) |> ignore
            env.Spawn(turnRight agent_SolidWithJoint_Quad, AgentSpawnOptions(Position=Point(0.75f * size.X, bottomY)), setDrawJointAndScale) |> ignore

            env }