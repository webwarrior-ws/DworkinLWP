module LibDworkin.DelayAgents

open AgentAbstractions


type ActivityDelayAgent() =
    inherit DelayAgent()

    override this.ReadyToRelease =
        not this.Environment.Value.HasActiveAgents


type FixedDelayAgent(numberOfSteps : int) =
    inherit DelayAgent()

    override this.ReadyToRelease =
        this.Age >= numberOfSteps
