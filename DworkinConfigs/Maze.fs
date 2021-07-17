namespace Maze


open LibDworkin.Aether
open LibDworkin.Aether.Operators
open LibDworkin.Geometry
open LibDworkin.Agents
open LibDworkin.Configs
open LibDworkin.Lenses
open LibDworkin.PharoCompatibility
open LibDworkin.Colliders
open LibDworkin.Probes
open LibDworkin.Brushes



module SpawnConfig =
    type StraightMazeRandomAngle() = 
        inherit SpawnConfig()
        member val Angle = 90 with get, set
        member val P = 0.75f with get, set
        member val PossibleAngles = [| 90; 0; 90 |] with get, set
        member val CollisionAccuracy = 4 with get, set

        member this.Clone() = this.MemberwiseClone() :?> StraightMazeRandomAngle
        
        static member Angle_ : Lens<StraightMazeRandomAngle, _> = 
            (fun x -> x.Angle), (fun value x -> let y = x.Clone() in y.Angle <- value; y)
        static member P_ : Lens<StraightMazeRandomAngle, _> = 
            (fun x -> x.P), (fun value x -> let y = x.Clone() in y.P <- value; y)
        static member PossibleAngles_ : Lens<StraightMazeRandomAngle, _> = 
            (fun x -> x.PossibleAngles), (fun value x -> let y = x.Clone() in y.PossibleAngles <- value; y)
        static member CollisionAccuracy_ : Lens<StraightMazeRandomAngle, _> = 
            (fun x -> x.CollisionAccuracy), (fun value x -> let y = x.Clone() in y.CollisionAccuracy <- value; y)

        override this.Evaluate(agent, pos) =
            let angles = 
                if this.Angle = 0 then
                    [| this.Angle |]
                else
                    [| this.Angle; -this.Angle |]
            if agent.Random.NextFloat() < this.P then
                angles.ShuffleBy(agent.Random) |> ignore
            
            let mutable i = 1
            for phi in angles do
                let config =
                    agent.Config 
                    |> Optic.set 
                        (termination_ >-> TerminationSpawn.SpawnConfig_ >-> cast<_, StraightMazeRandomAngle> >-> angle_) 
                        (this.PossibleAngles.AtRandom(agent.Random))
                    |> Optic.set 
                        (curvature_ >-> tryCast<_, CurvatureFormula.SNoise> >?> d_) 
                        (agent.Random.NextFloat() * 10.0f)
                let probeData = 
                    let s = 
                        (config.TerminationConditions.Collision.Value.PrimaryDetector.Distance / agent.Speed / (float32 this.CollisionAccuracy))
                        |> floor
                        |> int
                        |> max 1
                    agent.LaunchProbe(config, ProbeSpawnOptions(AngleDeg=float32 phi, StepsBetweenCollisionCheck=s))
                if Option.isNone probeData.Collision then
                    let delay = (round (agent.Config.TotalDistance / agent.Speed)) * float32(i-1) |> int
                    let spawnOptions = AgentSpawnOptions(AngleDeg=float32 phi)
                    let numSteps = int(agent.Thickness / agent.Speed) |> max 2
                    if phi <> 0 then
                        let jointType = if this.Angle <= 90 then Util.JointType.Miter else Util.JointType.Bevel
                        Util.drawJoint(agent, agent.Direction.Rotate(deg2rad(float32 phi) * agent.FlipFactor), jointType, numSteps, delay)
                    spawnOptions.DelayBy(if phi <> 0 then delay + numSteps else delay)
                    probeData.SpawnColliders()
                    agent.Spawn(
                        config, 
                        spawnOptions, 
                        fun a -> 
                            a.NoCollisions <- true)
                    i <- i + 1


module DworkinAgentConfig =
    let straightMazeRandomAngle_v2 = 
        { DworkinAgentConfig.Default with 
            Brush=BrushStrategySolid();
            ColliderOptions=ColliderOptionsGeneral(Strategy=ColliderStrategySegments());
            Speed=AgentSpeedConstant();
            Termination=TerminationSpawn(SpawnConfig=SpawnConfig.StraightMazeRandomAngle());
            TerminationConditions=
                { Collision=Some(TerminationConditionCollision(DetectorSize=DetectorSize.Absolute, IgnoreParent=true, MinDistance=5.0f, SelfIntersection=false)
                              |> Optic.set (primaryDetector_ >-> angle_) 180.0f
                              |> Optic.set (primaryDetector_ >-> distance_) 20.0f);
                Curvature=None;
                Distance=Some(TerminationConditionDistance()); };
            Thickness=ThicknessFormula.Constant(C=5.0f);
            TotalDistance=30.0f
         }

    let straightMazeRandomAngle = 
        { straightMazeRandomAngle_v2 with 
            Termination=
                TerminationSpawn(
                    SpawnConfig=SpawnConfig.StraightMazeRandomAngle(P=1.0f, PossibleAngles=[| 45; 90; -45; -90 |])
                );
            TerminationConditions=straightMazeRandomAngle_v2.TerminationConditions
                |> Optic.set (collision_ >-> TerminationConditionCollision.IgnoreParent_) true
                |> Optic.set (collision_ >-> primaryDetector_ >-> distance_) 22.0f
         }

    let straightMazeRandomAngle_v1 = 
        { straightMazeRandomAngle_v2 with 
            Brush=BrushStrategySolid();
            Termination=
                TerminationSpawn(
                    SpawnConfig=SpawnConfig.StraightMazeRandomAngle(P=0.5f, PossibleAngles=[| 135; 90; 45; 0 |], CollisionAccuracy=5)
                );
            TerminationConditions=straightMazeRandomAngle_v2.TerminationConditions
                    |> Optic.set (collision_ >-> primaryDetector_ >-> angle_) 105.0f
                    |> Optic.set (collision_ >-> primaryDetector_ >-> distance_) 19.95f
         }

    let straightMazeRandomAngle_v4 = 
        { straightMazeRandomAngle_v2 with 
            Termination=
                TerminationSpawn(
                    SpawnConfig=SpawnConfig.StraightMazeRandomAngle(Angle=0, P=0.5f, PossibleAngles=[| 60; 0; -60 |], CollisionAccuracy=3)
                )
         }

    let straightMazeRandomAngle_v5 = 
        { straightMazeRandomAngle_v2 with 
            Brush=BrushStrategySolid();
            Termination=
                TerminationSpawn(
                    SpawnConfig=SpawnConfig.StraightMazeRandomAngle(Angle=60, PossibleAngles=[| 60; 120; 0 |], CollisionAccuracy=5)
                );
            TerminationConditions=straightMazeRandomAngle_v2.TerminationConditions
                |> Optic.set (collision_ >-> primaryDetector_ >-> angle_) 120.0f
                |> Optic.set (collision_ >-> primaryDetector_ >-> distance_) 14.95f
         }

    let straightMazeRandomAngle_v6 = 
        { straightMazeRandomAngle_v2 with 
            Brush=BrushStrategySolid();
            Termination=
                TerminationSpawn(
                    SpawnConfig=
                        SpawnConfig.StraightMazeRandomAngle(
                            Angle=60,
                            P=0.5f,
                            PossibleAngles=[| 0; 30; 45; 60; 75; 90; 105; 120 |],
                            CollisionAccuracy=5
                        )
                );
            TerminationConditions=straightMazeRandomAngle_v2.TerminationConditions
                |> Optic.set (collision_ >-> primaryDetector_ >-> angle_) 90.0f
                |> Optic.set (collision_ >-> primaryDetector_ >-> distance_) 20.0f
                |> Optic.set (collision_ >-> minDistance_) 2.0f
         }

    let straightMazeRandomAngle_v7 = 
        { straightMazeRandomAngle_v2 with 
            Termination=
                TerminationSpawn(
                    SpawnConfig=
                        SpawnConfig.StraightMazeRandomAngle(
                            Angle=0,
                            P=0.5f,
                            PossibleAngles=[| 60; 0; -60; 90; -90 |],
                            CollisionAccuracy=5
                        )
                );
            TerminationConditions=straightMazeRandomAngle_v2.TerminationConditions
                |> Optic.set (collision_ >-> primaryDetector_ >-> angle_) 180.0f
                |> Optic.set (collision_ >-> primaryDetector_ >-> distance_) 25.0f
         }
     
    let straightMazeRandomAngle_v3 = 
        { straightMazeRandomAngle_v2 with 
            Termination=
                TerminationSpawn(
                    SpawnConfig=SpawnConfig.StraightMazeRandomAngle(P=0.5f, PossibleAngles=[| 45; 90; -45; -90 |], CollisionAccuracy=5)
                );
            TerminationConditions=straightMazeRandomAngle_v2.TerminationConditions
                |> Optic.set (collision_ >-> primaryDetector_ >-> angle_) 180.0f
                |> Optic.set (collision_ >-> primaryDetector_ >-> distance_) 19.95f
         }


type LWPMaze3() = 
    inherit DworkinEnvironmentConfigCustom()
    member val AgentConfig = DworkinAgentConfig.straightMazeRandomAngle_v2 with get, set
    member val Delay = 180 with get, set
    member val Scale = 1.0f with get, set
    member val NumAgents = 6 with get, set
    member val Mirror = true with get, set
    member val InitialAngle = 0 with get, set
    member val AngleIncrement = 90 with get, set
    member val GridSize = 60.0f with get, set
    member val ThicknessVariance = 0.0f with get, set

    member this.Clone() = this.MemberwiseClone() :?> LWPMaze3

    static member AgentConfig_ : Lens<LWPMaze3, _> = 
        (fun x -> x.AgentConfig), (fun value x -> let y = x.Clone() in y.AgentConfig <- value; y)
    static member Delay_ : Lens<LWPMaze3, _> = 
        (fun x -> x.Delay), (fun value x -> let y = x.Clone() in y.Delay <- value; y)
    static member Scale_ : Lens<LWPMaze3, _> = 
        (fun x -> x.Scale), (fun value x -> let y = x.Clone() in y.Scale <- value; y)
    static member NumAgents_ : Lens<LWPMaze3, _> = 
        (fun x -> x.NumAgents), (fun value x -> let y = x.Clone() in y.NumAgents <- value; y)
    static member AngleIncrement_ : Lens<LWPMaze3, _> = 
        (fun x -> x.AngleIncrement), (fun value x -> let y = x.Clone() in y.AngleIncrement <- value; y)
    static member Mirror_ : Lens<LWPMaze3, _> = 
        (fun x -> x.Mirror), (fun value x -> let y = x.Clone() in y.Mirror <- value; y)
    static member InitialAngle_ : Lens<LWPMaze3, _> = 
        (fun x -> x.InitialAngle), (fun value x -> let y = x.Clone() in y.InitialAngle <- value; y)
    static member GridSize_ : Lens<LWPMaze3, _> = 
        (fun x -> x.GridSize), (fun value x -> let y = x.Clone() in y.GridSize <- value; y)
    static member ThicknessVariance_ : Lens<LWPMaze3, _> = 
        (fun x -> x.ThicknessVariance), (fun value x -> let y = x.Clone() in y.ThicknessVariance <- value; y)

    override this.Evaluate(env) =
        let positions = ResizeArray()
        let cellSize = this.GridSize * this.Scale
        let delay = int(float32(this.Delay) / (this.AgentConfig.Speed :?> AgentSpeedConstant).Magnitude)
        
        for x in (env.Bounds.Left + cellSize) .. cellSize .. (env.Bounds.Right - cellSize) do
            for y in (env.Bounds.Top + cellSize) .. cellSize .. (env.Bounds.Bottom - cellSize) do
                positions.Add(Point(x, y))
        positions.ShuffleBy(env.Random)

        let agentBlock (a : DworkinAgent) = 
            a.NoCollisions <- true
            a.ScaleBy(this.Scale)
        let mutable config = this.AgentConfig |> Optic.map (speed_ >-> magnitude_) (fun x -> x / this.Scale)

        let spawnAgent pos angle delayed = 
            let angle = float32 angle
            let spawnOpts = ProbeSpawnOptions(Position=pos, AngleDeg=angle)
            if delayed then
                spawnOpts.DelayBy(delay)
            env.LaunchProbe(config, spawnOpts, agentBlock).SpawnColliders()
            env.Spawn(config, spawnOpts, agentBlock) |> ignore
        
        let angles = [| 0 .. this.AngleIncrement .. 360 |]
        for i in 1 .. this.NumAgents do
            let d = (i - 1) * delay
            env.Delay(
                (fun _ ->
                    let mutable triesLeft = 100
                    while triesLeft > 0 && positions.Count > 0 do
                        let p = positions.RemoveLast()
                        if not <| env.TestForCollision(CollisionLayers.All, CircleCollider(None, p, cellSize)) then
                            triesLeft <- 0
                            let c = (this.AgentConfig.Thickness :?> ThicknessFormula.Constant).C
                            config <- 
                                config
                                |> Optic.set 
                                    (thickness_ >-> cast<_,  ThicknessFormula.Constant> >-> c_) 
                                    (env.Random.NextInRange(c * (1.0f - this.ThicknessVariance), c * (1.0f + this.ThicknessVariance)))
                            let angle = this.InitialAngle + angles.AtRandom(env.Random)
                            let offset = Point(float32(sign p.X) * 0.5f, float32(sign p.Y) * 0.5f)
                            spawnAgent (p - offset) angle false
                            if this.Mirror then
                                spawnAgent (p - offset) (180 + angle) true
                        triesLeft <- triesLeft - 1
                    None),
                d)