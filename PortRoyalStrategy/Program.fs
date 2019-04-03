module PortRoyalStrategy.Program

let runningAverage currentAverage i newValue = 
    (currentAverage * (float i - 1.0) / (float i)) + (newValue / (float i))

[<EntryPoint>]
let main argv =
    let mutable p0WinCount = 0
    let mutable averageMoveCount = 0.0
    let mutable averageRuntime = 0.0
    let gameCount = 1000
    for i in [1 .. gameCount] do
        let game = PortRoyalModel.newDefaultGame 2
        let bots = [ SimulationEngine.randomizedAgent; SimulationEngine.randomizedAgent ]

        let sw = System.Diagnostics.Stopwatch.StartNew()
        let (moveCount, endState) = SimulationEngine.simulateGame game bots
        sw.Stop()

        averageMoveCount <- runningAverage averageMoveCount i (float moveCount)
        averageRuntime <- runningAverage averageRuntime i sw.Elapsed.TotalMilliseconds

        let winningPlayer = (PortRoyalModel.winningPlayer endState).Value
        if winningPlayer = 0 then
            p0WinCount <- p0WinCount + 1

        //printf "Random game %d ended in %f ms after %d actions (winning player: %d)\n" 
        //    i sw.Elapsed.TotalMilliseconds moveCount winningPlayer
    
    printf "Player 0 won %d of %d games. Avg move count: %f; avg runtime: %f ms" p0WinCount gameCount averageMoveCount averageRuntime
    0
