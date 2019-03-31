module PortRoyalStrategy.SimulationEngine

open PortRoyalStrategy.PortRoyalModel

type Bot = GameState -> Action

// Bot that always plays a random move.
let randomizedBot (state: GameState): Action = 
    validActions state |> Utils.pickRandom state.RandomNumberGenerator

let smartTestBot (state: GameState): Action =
    let actions = validActions state
    let playerIdx = (activePlayerIdx state).Value
    let currentVP state = totalVictoryPoints state.Players.[playerIdx].PersonalDisplay
    // looks 1 move ahead and chooses the one that gives the most VP gain
    let bestAction =
        actions
        |> Utils.shuffleList state.RandomNumberGenerator
        |> List.maxBy (fun action -> step state action |> currentVP)
    bestAction

let simulateGame (state: GameState) (bots: Bot list): int * GameState = 
    let mutable state = state
    let mutable activePlayer = activePlayerIdx state
    let mutable moveCount = 0
    while activePlayer.IsSome do
        let nextAction = bots.[activePlayer.Value] state
        state <- step state nextAction
        activePlayer <- activePlayerIdx state
        moveCount <- moveCount + 1
    moveCount, state