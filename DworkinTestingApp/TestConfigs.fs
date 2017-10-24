module TestConfigs


open LibDworkin.Aether
open LibDworkin.Geometry
open LibDworkin.AgentAbstractions
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.Brushes
open LibDworkin.Aether
open LibDworkin.Aether.Operators


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
        |> Optic.set DworkinAgentConfig.Brush_ (BrushStrategySolid(Cap=BrushCap.Round) :> BrushStrategy)
        |> Optic.set DworkinAgentConfig.Speed_ (AgentSpeedScaleAdaptive() :> AgentSpeed)
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
                config  
                |> Optic.set DworkinAgentConfig.Termination_ (Termination_Spiral(Angle= -60.0f) :> TerminationStrategy)
            env.Spawn(turnRight agent_SolidWithJoint_Triangle, AgentSpawnOptions(Position=Point(0.55f * size.X, bottomY)), setDrawJointAndScale) |> ignore
            env.Spawn(turnRight agent_SolidWithJoint_Quad, AgentSpawnOptions(Position=Point(0.75f * size.X, bottomY)), setDrawJointAndScale) |> ignore

            env }


type Curvature_DirectionPreservingTurn() =
    inherit CurvatureFormula()
    member val A = 0.0247f with get, set
    override this.Evaluate(s, t) = this.A * sin(s * Single.Pi)


type Termination_Tree() =
    inherit TerminationCustom()
    override this.Evaluate(agent, collision) =
        let scale (a : DworkinAgent) = a.ScaleBy(Point(0.67f))
        agent.GenerationLimit <- 5
        agent.Spawn(agent.Config, AgentSpawnOptions(OffsetSideways=(agent.Thickness * 0.34f * 0.5f)), scale)
        agent.Spawn(agent.Config, AgentSpawnOptions(OffsetSideways=(agent.Thickness * 0.34f * -0.5f)), fun a -> scale a; a.Flip())


let colliderStrategyTest : DworkinEnvironmentConfig =
    let tree =
        DworkinAgentConfig.Default
        |> Optic.set DworkinAgentConfig.Brush_ (BrushStrategyNone() :> BrushStrategy)
        |> Optic.set DworkinAgentConfig.Curvature_ (Curvature_DirectionPreservingTurn() :> CurvatureFormula)
        |> Optic.set DworkinAgentConfig.Termination_ (Termination_Tree() :> TerminationStrategy)
        |> Optic.set DworkinAgentConfig.ColliderOptions_ (ColliderOptionsGeneral() :> ColliderOptions)
        |> Optic.set DworkinAgentConfig.Speed_ (AgentSpeedConstant(Magnitude=5.0f) :> AgentSpeed)
    
    let segmentColliders config =
        config |> Optic.set (DworkinAgentConfig.ColliderOptions_ >-> ColliderOptionsGeneral.Strategy_) (ColliderStrategySegments() :> ColliderStrategy)

    { new DworkinEnvironmentConfig with
        member this.GetEnvironment(size) = 
            let env = DworkinEnvironment(Rectangle(Point(), size), 42)

            let scale (agent : DworkinAgent) = agent.ScaleBy(Point(20.0f, 1.0f))

            env.Spawn(tree, AgentSpawnOptions(Position=Point(0.4f * size.X, 0.5f * size.Y)), scale) |> ignore
            env.Spawn(segmentColliders tree, AgentSpawnOptions(Position=Point(0.8f * size.X, 0.5f * size.Y)), scale) |> ignore

            env }