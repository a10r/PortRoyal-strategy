module PortRoyalStrategy.SimulationEngine

open PortRoyalStrategy.PortRoyalModel

type Agent = GameState -> Action
// TODO maybe return type should be something else. float?
type Heuristic = int -> GameState -> int

// Agent that always plays a random move.
let randomizedAgent (state: GameState): Action = 
    validActions state |> Utils.pickRandom state.RandomNumberGenerator

let testHeuristic (playerIdx: int) (state: GameState) = 
    let currentVP = totalVictoryPoints state.Players.[playerIdx].PersonalDisplay
    let currentGold = state.Players.[playerIdx].OwnedCoins.Length
    // add a penalty if we go into tax increase territory
    let goldPenalty = if currentGold > 12 then 100 else 0
    100 * currentVP + currentGold - goldPenalty

let oneStepHeuristicAgent (heuristic: Heuristic) (state: GameState): Action =
    let actions = validActions state
    let playerIdx = (activePlayerIdx state).Value
    // looks 1 move ahead and chooses the one that gives the best result according to the heuristic.
    // problem though: the agent can kind of look into the future with this simulation design. 
    // the RNG will be different in the actual game though.
    let bestAction =
        actions
        |> Utils.shuffleList state.RandomNumberGenerator
        |> List.maxBy (fun action -> state |> step action |> heuristic playerIdx)
    bestAction

let smartTestAgent = oneStepHeuristicAgent testHeuristic

let simulateGame (state: GameState) (agents: Agent list): int * GameState = 
    let mutable state = state
    let mutable activePlayer = activePlayerIdx state
    let mutable moveCount = 0
    while activePlayer.IsSome do
        let nextAction = agents.[activePlayer.Value] state
        state <- state |> step nextAction
        activePlayer <- activePlayerIdx state
        moveCount <- moveCount + 1
    moveCount, state