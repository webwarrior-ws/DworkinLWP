module TestConfigs


open LibDworkin.Aether
open LibDworkin.Geometry
open LibDworkin.AgentAbstractions
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.Lenses
open LibDworkin.Brushes
open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.SimplexNoise
open LibDworkin.PharoCompatibility


type Termination_Spiral() =
    inherit TerminationCustom()
    member val Gap = 10.0f with get, set
    member val Angle = 90.0f with get, set

    override this.Evaluate(agent, collision) =
        match collision with
        | None -> 
            agent.Spawn(
                agent.Config |> Optic.map totalDistance_ (fun x -> x - this.Gap), 
                AgentSpawnOptions(AngleDeg=this.Angle), 
                fun a -> a.Attributes.DrawJoint.Set(true))
        | Some(_) -> ()


let brushesAndSpeedsTest : DworkinEnvironmentConfig =
    let agent_Solid_Adaptive =
        DworkinAgentConfig.Default
        |> Optic.set brush_ (BrushStrategySolid(Cap=BrushCap.Round))
        |> Optic.set speed_ (AgentSpeedScaleAdaptive())
    let agent_SolidWithJoint =
        DworkinAgentConfig.Default
        |> Optic.set termination_ (Termination_Spiral())
    let agent_SolidWithJoint_Triangle =
        agent_SolidWithJoint 
        |> Optic.set brush_ (BrushStrategySolidJoint())
    let agent_SolidWithJoint_Quad =
        agent_SolidWithJoint 
        |> Optic.set brush_ (BrushStrategySolidJoint(JointType=BrushJoint.Quad))
    let agent_Circles_Thickness =
        DworkinAgentConfig.Default 
        |> Optic.set brush_ (BrushStrategyCircles())
        |> Optic.set speed_ (AgentSpeedThickness())
    
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
                |> Optic.set termination_ (Termination_Spiral(Angle= -60.0f))
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
        |> Optic.set brush_ (BrushStrategyNone())
        |> Optic.set curvature_ (Curvature_DirectionPreservingTurn())
        |> Optic.set termination_ (Termination_Tree())
        |> Optic.set colliderOptions_ (ColliderOptionsGeneral())
        |> Optic.set speed_ (AgentSpeedConstant(Magnitude=5.0f))
    
    let segmentColliders config =
        config |> Optic.set (colliderOptions_ >-> strategy_) (ColliderStrategySegments())

    { new DworkinEnvironmentConfig with
        member this.GetEnvironment(size) = 
            let env = DworkinEnvironment(Rectangle(Point(), size), 42)

            let scale (agent : DworkinAgent) = agent.ScaleBy(Point(20.0f, 1.0f))

            env.Spawn(tree, AgentSpawnOptions(Position=Point(0.4f * size.X, 0.5f * size.Y)), scale) |> ignore
            env.Spawn(segmentColliders tree, AgentSpawnOptions(Position=Point(0.8f * size.X, 0.5f * size.Y)), scale) |> ignore

            env }


type Density_Ex40() =
    inherit BranchingCriterionDensity()
    override this.Evaluate(s, t) =
        if t >= 0.15f then (s.SNoise() + 1.0f) * 0.05f else 0.0f

type Curvature_DiminishingSpiral() =
    inherit CurvatureFormula()
    member val Alpha = 3.0f with get, set
    static member Alpha_ : Lens<Curvature_DiminishingSpiral, float32> =
        (fun x -> x.Alpha), (fun value x -> let y = x.Clone() :?> Curvature_DiminishingSpiral in y.Alpha <- value; y)
    
    override this.Evaluate(s, t) =
        this.Alpha / 100.0f / (max (1.0f - 0.125f * s) 0.005f)

 type Spawn_Ex40() =
    inherit SpawnConfig()
    override this.Evaluate(agent, pos) =
        let config = 
            agent.Config 
            |> Optic.map 
                (curvature_ >-> Curvature_DiminishingSpiral.Alpha_) 
                (fun x -> x * agent.Random.NextInRange(0.3f, 2.5f))
        agent.Spawn(config, AgentSpawnOptions(AngleDeg= -30.0f), fun a -> a.Flip())

type Handler_Noop() =
    inherit CollisionHandler()
    override this.Evaluate(agent, collision) =
        true


let iGeoExample40  : DworkinEnvironmentConfig =
    let branching = 
        BranchingCriterionBased(Criterion=Density_Ex40(), SpawnConfig=Spawn_Ex40()) :> BranchingStrategy
    let collisionConfig =
        TerminationConditionCollision(PrimaryDetector={Distance=1.0f; Angle=60.0f}, MinDistance=2.0f, Handler=Some(Handler_Noop() :> CollisionHandler))
    let config = 
        DworkinAgentConfig.Default
        |> Optic.set curvature_ (Curvature_DiminishingSpiral())
        |> Optic.set colliderOptions_ (ColliderOptionsGeneral())
        |> Optic.set speed_ (AgentSpeedConstant(Magnitude=2.0f))
        |> Optic.set totalDistance_ 400.0f
        |> Optic.set branching_ branching
        |> Optic.set (terminationConditions_ >-> collision_) collisionConfig
        
    { new DworkinEnvironmentConfig with
        member this.GetEnvironment(size) = 
            let env = DworkinEnvironment(Rectangle(Point(), size), 42)
            env.Spawn(config, AgentSpawnOptions(Position=size*0.5f), fun a -> a.ScaleBy(Point(2.0f, 1.0f))) |> ignore
            env }

   
