module PortRoyalStrategy.PortRoyalModel

open System

type Color = 
    | Black 
    | Blue 
    | Green 
    | Yellow 
    | Red

type CitizenKind =
    | Settler
    | Captain
    | Priest
    | JackOfAllTrades // Provides any citizen kind towards an expedition

type BonusCondition =
    | MaxStrength
    | MinVictoryPoints

type ShipStrength =
    | Invincible
    | Strength of int

type Card = 
    | Ship of Color: Color * Coins: int * Strength: ShipStrength
    | Trader of BonusColor: Color * Price: int * VictoryPoints: int
    | Citizen of Kind: CitizenKind * Price: int * VictoryPoints: int
    | Sailor of Strength: int * Price: int * VictoryPoints: int
    | Mademoiselle of Price: int * VictoryPoints: int
    | Jester of Price: int * VictoryPoints: int
    | Admiral of Price: int * VictoryPoints: int
    | Governor of Price: int * VictoryPoints: int
    | Expedition of CompletionRequirements: CitizenKind list * BonusCoins: int * VictoryPoints: int
    | TaxIncrease of BonusCondition: BonusCondition

let victoryPoints (c: Card): int = 
    match c with 
    | Trader(VictoryPoints = vp) | Citizen(VictoryPoints = vp) | Sailor(VictoryPoints = vp) | Mademoiselle(VictoryPoints = vp)
    | Jester(VictoryPoints = vp) | Admiral(VictoryPoints = vp) | Governor(VictoryPoints = vp) -> vp
    | _ -> 0

let fightingStrength (c: Card): int =
    match c with
    | Sailor(Strength = s) -> s
    | _ -> 0

let basePrice (c: Card): int = 
    match c with
    | Trader(Price = p) | Citizen(Price = p) | Sailor(Price = p) | Mademoiselle(Price = p)
    | Jester(Price = p) | Admiral(Price = p) | Governor(Price = p) -> p
    | _ -> 0

type Player = 
    { PersonalDisplay: Card list
      OwnedCoins: Card list }

type GamePhase =
    | Discover of CurrentPlayerIdx: int
    | TradeAndHire of CurrentPlayerIdx: int * ActivePlayerIdx: int * RemainingHireCount: int
    | GameEnded of WinningPlayerIdx: int

type GameState = 
    { RandomNumberGenerator: Utils.RandomNumberGenerator 
      Phase: GamePhase
      Players: Player list
      HarborDisplay: Card list
      Expeditions: Card list
      DrawPile: Card list
      Discard: Card list }

type Action = 
    | DiscoverCard
    | RepelShip
    | BeginHiring
    | TakeCard of CardIdx: int
    | CompleteExpedition of ExpeditionIdx: int
    | Pass

let rec drawCard (state: GameState): Card * GameState = 
    match state.DrawPile with
    | [] -> drawCard { state with DrawPile = Utils.shuffleList state.RandomNumberGenerator state.Discard; Discard = [] }
    | (drawnCard :: tail) -> (drawnCard, { state with DrawPile = tail})

let rec drawCards (state: GameState) (count: int): Card list * GameState =
    let mutable drawnCards = []
    let mutable currentState = state
    for _ in [1..count] do
        let (c, newState) = drawCard currentState
        drawnCards <- c :: drawnCards
        currentState <- newState
    (drawnCards, currentState)

// Small Utils
let totalVictoryPoints (ownedCards: Card list): int = List.sumBy victoryPoints ownedCards
let totalFightingStrength (ownedCards: Card list): int = List.sumBy fightingStrength ownedCards
let lastDrawnCard (state: GameState): Card = state.HarborDisplay.Head

let differentColoredShipCount (cards: Card list): int = 
    (cards |> List.collect (fun card -> match card with Ship(Color = c) -> [c] | _ -> []) |> List.distinct).Length

// Calculates amount of cards the current player can take from the harbor display. Special effects (governor) not taken into account.
let baseHireCount (cards: Card list): int = 
    let colorCount = cards |> differentColoredShipCount
    match colorCount with 
    | 0 | 1 | 2 | 3 -> 1
    | 4 -> 2
    | 5 -> 3
    | _ -> failwith "Invalid color count."
    
let effectivePrice (cardToHire: Card) (player: Player): int = 
    let basePrice = basePrice cardToHire
    let reduction = player.PersonalDisplay 
                    |> List.filter (fun c -> match c with Mademoiselle(_) -> true | _ -> false) 
                    |> List.length
    max 0 (basePrice - reduction)

let effectiveCoinGain (shipToTake: Card) (player: Player): int = 
    match shipToTake with 
    | Ship(color, baseCoins, _) ->  
        let isApplicableTrader (card: Card) =
            match card with 
            | Trader(bonusColor, _, _) when color = bonusColor -> true
            | _ -> false
        let bonusCoins = player.PersonalDisplay |> List.filter isApplicableTrader |> List.length
        baseCoins + bonusCoins
    | _ -> failwith "Not a ship."

let bonusHireCount (player: Player): int = 
    player.PersonalDisplay 
    |> List.filter (fun c -> match c with Governor(_) -> true | _ -> false) 
    |> List.length

let coloredShipExists (cards: Card list) (color: Color): bool = 
    cards 
    |> List.exists (fun c -> match c with Ship(color2, _, _) -> color2 = color | _ -> false)

let canRepelShip (player: Player) (ship: Card) =
    match ship with
    | Ship(_, _, strength) -> 
        match strength with 
        | Invincible -> false
        | Strength(shipStrength) -> 
            let playerStrength = totalFightingStrength player.PersonalDisplay
            shipStrength <= playerStrength
    | _ -> false

let admiralBonusCoins (cards: Card list): int =
    let admiralCount = 
        cards 
        |> List.filter (fun c -> match c with Admiral(_) -> true | _ -> false) 
        |> List.length
    admiralCount * 2

let jesterBonusCoins (cards: Card list): int = 
    cards 
    |> List.filter (fun c -> match c with Jester(_) -> true | _ -> false) 
    |> List.length

let playerReceivesCoins (playerIdx: int) (coinCount: int) (state: GameState): GameState =
    if coinCount <= 0 then 
        state
    else
    let (newCoins, state) = drawCards state coinCount
    let updatedPlayer = { state.Players.[playerIdx] with OwnedCoins = newCoins @ state.Players.[playerIdx].OwnedCoins}
    let updatedPlayerList = state.Players |> Utils.replaceAt playerIdx updatedPlayer
    { state with Players = updatedPlayerList }

// TODO incorporate these into step
let playerPaysCoins (playerIdx: int) (coinCount: int) (state: GameState): GameState = 
    let paidCoins = state.Players.[playerIdx].OwnedCoins |> List.take coinCount
    let remainingCoins = state.Players.[playerIdx].OwnedCoins |> List.skip coinCount
    let updatedPlayer = { state.Players.[playerIdx] with OwnedCoins = remainingCoins; }
    let updatedPlayerList = state.Players |> Utils.replaceAt playerIdx updatedPlayer
    { state with Discard = paidCoins @ state.Discard; Players = updatedPlayerList }

let playerTakesCardFromHarborDisplay (playerIdx: int) (cardIdx: int) (state: GameState): GameState =
    let card = state.HarborDisplay.[cardIdx]
    let updatedHarborDisplay = state.HarborDisplay |> Utils.removeAt cardIdx
    let updatedPlayer = { state.Players.[playerIdx] with PersonalDisplay = card :: state.Players.[playerIdx].PersonalDisplay }
    let updatedPlayerList = state.Players |> Utils.replaceAt playerIdx updatedPlayer
    { state with HarborDisplay = updatedHarborDisplay; Players = updatedPlayerList }

let discardFromHarborDisplay (cardIdx: int) (state: GameState): GameState = 
    let discardedCard = state.HarborDisplay.[cardIdx]
    { state with 
        HarborDisplay = state.HarborDisplay |> Utils.removeAt cardIdx
        Discard = discardedCard :: state.Discard }

let discardFromPersonalDisplay (playerIdx: int) (cardIdx: int) (state: GameState): GameState =
    let discardedCard = state.Players.[playerIdx].PersonalDisplay.[cardIdx]
    let newPersonalDisplay = state.Players.[playerIdx].PersonalDisplay |> Utils.removeAt cardIdx
    let newPlayer = { state.Players.[playerIdx] with PersonalDisplay = newPersonalDisplay }
    { state with 
        Discard = discardedCard :: state.Discard 
        Players = state.Players |> Utils.replaceAt playerIdx newPlayer }

let playerPaysCoinToOtherPlayer (payerIdx: int) (receiverIdx: int) (state: GameState): GameState =
    let coin = state.Players.[payerIdx].OwnedCoins |> List.head
    let newPayer = { state.Players.[payerIdx] with OwnedCoins = state.Players.[payerIdx].OwnedCoins |> List.tail }
    let newReceiver = { state.Players.[receiverIdx] with OwnedCoins = coin :: state.Players.[receiverIdx].OwnedCoins }
    let newPlayerList = state.Players |> Utils.replaceAt payerIdx newPayer |> Utils.replaceAt receiverIdx newReceiver
    { state with Players = newPlayerList }

let activePlayerIdx (state: GameState): int option = 
    match state.Phase with
    | Discover(CurrentPlayerIdx = idx)
    | TradeAndHire(ActivePlayerIdx = idx) -> 
        Some(idx)
    | _ ->
        None

// TODO rename current --> active; active --> something else?
let isCurrentPlayer (playerIdx: int) (state: GameState): bool =
    match state.Phase with
    | Discover(CurrentPlayerIdx = c)
    | TradeAndHire(CurrentPlayerIdx = c) -> 
        playerIdx = c
    | _ ->
        false

let playerCanAffordCard (playerIdx: int) (cardIdx: int) (state: GameState): bool =
    let card = state.HarborDisplay.[cardIdx]
    let player = state.Players.[playerIdx]
    match card with
    | Ship(_) -> true
    | _ -> 
        let price = effectivePrice card player
        let effectiveTotalPrice = if not (isCurrentPlayer playerIdx state) then price + 1 else price
        let ownedCoinCount = player.OwnedCoins.Length
        ownedCoinCount >= effectiveTotalPrice

// Check if a player has won. If yes returns the index of that player, otherwise returns None.
let winningPlayer (state: GameState): int option =
    match state.Phase with
    | GameEnded(winningPlayerIdx) -> Some(winningPlayerIdx)
    | _ ->

    let playerIdxWithVictoryPoints = 
        state.Players
        |> List.map (fun player -> player.PersonalDisplay |> List.sumBy victoryPoints)
        |> List.indexed
        
    let maxVictoryPoints = 
        playerIdxWithVictoryPoints 
        |> List.maxBy (fun (_, vp) -> vp) |> snd

    if maxVictoryPoints < 12 then
        None
    else

    let playerIdxWithMaxVictoryPoints = 
        playerIdxWithVictoryPoints
        |> List.filter (fun (_, vp) -> vp = maxVictoryPoints)

    if playerIdxWithMaxVictoryPoints.Length = 1 then
        Some (fst playerIdxWithMaxVictoryPoints.Head)
    else
    
    let playerIdxWithCoinCount = 
        playerIdxWithMaxVictoryPoints
        |> List.map (fun (idx, _) -> idx, state.Players |> List.item idx |> (fun p -> p.OwnedCoins.Length))

    let maxCoinCount = playerIdxWithCoinCount |> List.maxBy (fun (_, coinCount) -> coinCount) |> snd

    let playerIdxWithMaxCoinCount = 
        playerIdxWithCoinCount 
        |> List.filter (fun (_, coinCount) -> coinCount = maxCoinCount)

    if playerIdxWithMaxCoinCount.Length = 1 then
        Some (fst playerIdxWithMaxCoinCount.Head)
    else
        // Normally, all tied players share the win. To keep the model simple, 
        // we give the win to the first player in the list for now.
        Some (fst playerIdxWithMaxCoinCount.Head)

// TODO I'm sure there is a nicer way to model these cards...
let isACitizen card = match card with Citizen(_) -> true | _ -> false
let isCitizenWithKind card kind = match card with Citizen(k, _, _) when k = kind -> true | _ -> false
let citizenKind card = match card with Citizen(kind, _, _) -> kind | _ -> failwith "Not a citizen."

let playerCanCompleteExpedition (playerIdx: int) (expeditionIdx: int) (state: GameState): bool = 
    let citizenKinds = 
        state.Players.[playerIdx].PersonalDisplay
        |> List.filter isACitizen
        |> List.map citizenKind
    let jackOfAllTradesCount = citizenKinds |> List.filter (fun k -> k = JackOfAllTrades) |> List.length
    let expeditionCard = state.Expeditions.[expeditionIdx]
    match expeditionCard with 
    | Expedition(requirements, _, _) -> 
        // TODO maybe there is a better way to do this?
        let unfulfilledReqs = (requirements, citizenKinds) ||> List.fold (fun reqs kind -> Utils.remove kind reqs)
        let count = unfulfilledReqs.Length
        count <= jackOfAllTradesCount
    | _ -> 
        false
    
// TODO it works, but it is kind of a mess?
let playerCompletesExpedition (playerIdx: int) (expeditionIdx: int) (state: GameState): GameState =
    // Citizen cards always give exactly one victory point in the standard deck. Because of that, it does
    // not matter which citizens are chosen for the trade. Normal citizens are preferred against jack of all trades.

    if not (playerCanCompleteExpedition playerIdx expeditionIdx state) then
        failwith "Player does not meet the requirements for this expedition."

    let findCitizenKind cards kind = 
        cards |> List.tryFindIndex (fun card -> isCitizenWithKind card kind)
    let findJackOfAllTrades cards = findCitizenKind cards JackOfAllTrades

    let mutable personalDisplay = state.Players.[playerIdx].PersonalDisplay
    let mutable discardedCards = []

    let expeditionCard = state.Expeditions.[expeditionIdx]

    match expeditionCard with
    | Expedition(requirements, bonusCoins, _) ->
        for req in requirements do
            match findCitizenKind personalDisplay req with
            | Some(idx) -> 
                discardedCards <- personalDisplay.[idx] :: discardedCards
                personalDisplay <- personalDisplay |> Utils.removeAt idx
            | None ->
                match findJackOfAllTrades personalDisplay with
                | Some(idx) -> 
                    discardedCards <- personalDisplay.[idx] :: discardedCards
                    personalDisplay <- personalDisplay |> Utils.removeAt idx
                | None -> failwith "Player does not meet the requirements for this expedition."

        let newPlayer = { state.Players.[playerIdx] with PersonalDisplay = expeditionCard :: personalDisplay }

        let state = 
            { state with 
                Discard = discardedCards @ state.Discard
                Players = state.Players |> Utils.replaceAt playerIdx newPlayer
                Expeditions = state.Expeditions |> Utils.removeAt expeditionIdx }

        state |> playerReceivesCoins playerIdx bonusCoins
    | _ ->
        failwith "Cannot complete this expedition."

// Calculates the next game state given an action from the current player.
let step (action: Action) (state: GameState): GameState = 
    let todo () = raise (NotImplementedException())
    let invalidAction () = failwith "Invalid action"
    let invalidAction2 msg = failwith msg

    match state.Phase with
    | Discover(currentPlayerIdx) -> 
        match action with
        | CompleteExpedition(expeditionIdx) -> state |> playerCompletesExpedition currentPlayerIdx expeditionIdx
        | DiscoverCard -> 
            let (drawnCard, state) = drawCard state

            match drawnCard with
            | TaxIncrease(bonusCondition) -> 
                let state = { state with Discard = drawnCard :: state.Discard }

                let doTaxes (player: Player): Player * discardedCoins : Card list = 
                    let totalCoinCount = player.OwnedCoins.Length
                    let owedCoinCount = if totalCoinCount >= 12 then totalCoinCount / 2 else 0
                    let newPlayer = { player with OwnedCoins = player.OwnedCoins |> List.skip owedCoinCount }
                    let discardedCoins = List.take owedCoinCount player.OwnedCoins
                    (newPlayer, discardedCoins)

                let (newPlayers, discardedCoinLists) = state.Players |> List.map doTaxes |> List.unzip
                let allDiscardedCards = discardedCoinLists |> List.concat

                let state = { state with Players = newPlayers; Discard = allDiscardedCards @ state.Discard }

                let playersWithMaxStrength (players: Player list): int list = 
                    let indices = [0 .. players.Length - 1]
                    let playerStrengths = players |> List.map (fun p -> p.PersonalDisplay |> List.sumBy fightingStrength)
                    let maxStrength = List.max playerStrengths
                    List.zip indices playerStrengths 
                    |> List.filter (fun (player, totalStrength) -> totalStrength = maxStrength) 
                    |> List.unzip 
                    |> fst

                let playersWithMinVictoryPoints (players: Player list): int list = 
                    let indices = [0 .. players.Length - 1]
                    let playerVictoryPoints = players |> List.map (fun p -> p.PersonalDisplay |> List.sumBy victoryPoints)
                    let minVictoryPoints = List.min playerVictoryPoints
                    List.zip indices playerVictoryPoints 
                    |> List.filter (fun (player, totalVictoryPoints) -> totalVictoryPoints = minVictoryPoints) 
                    |> List.unzip 
                    |> fst

                let bonusPlayerIndices = 
                    match bonusCondition with 
                    | MaxStrength -> playersWithMaxStrength state.Players 
                    | MinVictoryPoints -> playersWithMinVictoryPoints state.Players
                
                let state = (state, bonusPlayerIndices) ||> List.fold (fun state playerIdx -> playerReceivesCoins playerIdx 1 state)
                state

            | Expedition(_) -> 
                { state with Expeditions = drawnCard :: state.Expeditions }

            | Ship(color, coins, strength) ->
                if coloredShipExists state.HarborDisplay color then
                    if not (canRepelShip state.Players.[currentPlayerIdx] drawnCard) then
                        let bonusCoins = jesterBonusCoins state.Players.[currentPlayerIdx].PersonalDisplay
                        let state = playerReceivesCoins currentPlayerIdx bonusCoins state
                        // If the player cannot repel the ship, all cards are discarded and we move to the TradeAndHire phase with an empty harbor display.
                        { state with 
                            HarborDisplay = [];
                            Discard = drawnCard :: state.HarborDisplay @ state.Discard; 
                            Phase = TradeAndHire(currentPlayerIdx, currentPlayerIdx, 1) }
                    else
                        // If the player can repel the ship, this is the only possible move so we don't make it explicit.
                        { state with Discard = drawnCard :: state.Discard}
                else
                    { state with HarborDisplay = drawnCard :: state.HarborDisplay }
            | _ -> 
                { state with HarborDisplay = drawnCard :: state.HarborDisplay }

        | RepelShip -> 
            match lastDrawnCard state with
            | Ship(_) -> 
                if canRepelShip state.Players.[currentPlayerIdx] (lastDrawnCard state) then 
                    { state with HarborDisplay = state.HarborDisplay.Tail; Discard = state.HarborDisplay.Head :: state.Discard }
                else 
                    invalidAction2 "Insufficient fighting strength to repel ship."
            | _ -> invalidAction2 "Can only repel ships."

        | BeginHiring -> 
            if state.HarborDisplay.Length = 0 then
                invalidAction2 "At least one card must be discovered."

            // Process admiral bonus effect.
            let state = 
                if state.HarborDisplay.Length >= 5 then
                    let bonusCoins = admiralBonusCoins state.Players.[currentPlayerIdx].PersonalDisplay
                    let (drawnCards, state) = drawCards state bonusCoins
                    { state with 
                        Players = state.Players |> Utils.modifyAt currentPlayerIdx (fun p -> {p with OwnedCoins = drawnCards @ p.OwnedCoins})}
                else
                    state

            let baseHireCount = baseHireCount state.HarborDisplay
            let bonusHireCount = bonusHireCount state.Players.[currentPlayerIdx]
            let nextPhase = TradeAndHire(currentPlayerIdx, currentPlayerIdx, baseHireCount + bonusHireCount)
            { state with Phase = nextPhase }

        | _ -> invalidAction ()

    | TradeAndHire(currentPlayerIdx, activePlayerIdx, remainingHireCount) ->
        // Only applies if the game moves to the next player in turn order (i.e. the active player passes).
        let nextPlayerPhase = 
            let playerCount = state.Players.Length
            let nextPlayerIdx = (activePlayerIdx + 1) % playerCount    
            if nextPlayerIdx = currentPlayerIdx then
                Discover((nextPlayerIdx + 1) % playerCount)
            else
                let bonusHireCount = bonusHireCount state.Players.[nextPlayerIdx]
                TradeAndHire(currentPlayerIdx, nextPlayerIdx, 1 + bonusHireCount)

        let nextPhase = 
            if remainingHireCount > 1 then
                TradeAndHire(currentPlayerIdx, activePlayerIdx, remainingHireCount - 1)
            else
                nextPlayerPhase

        let state = 
            match action with
            | CompleteExpedition(expeditionIdx) -> state |> playerCompletesExpedition activePlayerIdx expeditionIdx
            | TakeCard(cardIdx) -> 
                let takenCard = state.HarborDisplay.[cardIdx]
                let activePlayer = state.Players.[activePlayerIdx]
                let isNotCurrentPlayer = currentPlayerIdx <> activePlayerIdx

                let state = 
                    match takenCard with
                    | Ship(color, coins, strength) -> 
                        let totalCoinsGained = effectiveCoinGain takenCard activePlayer
                        let state = 
                            state 
                            |> playerReceivesCoins activePlayerIdx totalCoinsGained 
                            |> discardFromHarborDisplay cardIdx
                        { state with Phase = nextPhase }

                    | _ ->
                        let price = effectivePrice takenCard activePlayer
                        let effectiveTotalPrice = if isNotCurrentPlayer then price + 1 else price
                        if effectiveTotalPrice > activePlayer.OwnedCoins.Length then
                            invalidAction2 "Cannot buy this card: Insufficient coins."
                        else
                            // Card can be hired!
                            let paidCoins = activePlayer.OwnedCoins |> List.take price
                            let remainingCoins = activePlayer.OwnedCoins |> List.skip price

                            let updatedPlayer = { activePlayer with OwnedCoins = remainingCoins; PersonalDisplay = takenCard :: activePlayer.PersonalDisplay }

                            { state with 
                                Discard = paidCoins @ state.Discard
                                Players = state.Players |> Utils.modifyAt activePlayerIdx (fun p -> updatedPlayer)
                                HarborDisplay = state.HarborDisplay |> Utils.removeAt cardIdx
                                Phase = nextPhase }

                // Pay 1 coin to the current player.
                if isNotCurrentPlayer then
                    state |> playerPaysCoinToOtherPlayer activePlayerIdx currentPlayerIdx
                else
                    state
            
            | Pass -> 
                let state = 
                    match nextPlayerPhase with
                    | TradeAndHire(_, nextActivePlayerIdx, _) when state.HarborDisplay.IsEmpty ->
                        // Process Jester for non-current player.
                        let bonusCoins = jesterBonusCoins state.Players.[nextActivePlayerIdx].PersonalDisplay
                        state |> playerReceivesCoins nextActivePlayerIdx bonusCoins
                    | _ -> 
                        state

                let winningPlayer = winningPlayer state
                match nextPhase with 
                | Discover(_) when winningPlayer.IsSome ->
                    { state with Phase = GameEnded(winningPlayer.Value) }
                | _ -> 
                    { state with Phase = nextPlayerPhase }

            | _ -> invalidAction ()

        match nextPhase with
        | Discover(_) ->
            { state with Discard = state.Discard @ state.HarborDisplay; HarborDisplay = [] }
        | _ ->
            state


    | GameEnded(_) -> 
        invalidAction ()

// Returns all possible actions given the current game state
let validActions (state: GameState): Action list =
    let completableExpeditions playerIdx = 
        state.Expeditions 
        |> Utils.indices
        |> List.filter (fun idx -> playerCanCompleteExpedition playerIdx idx state)
        |> List.map CompleteExpedition

    match state.Phase with
    | Discover(currentPlayerIdx) -> 
        let completableExpeditions = completableExpeditions currentPlayerIdx
        let beginHiring = if state.HarborDisplay.IsEmpty then [] else [ BeginHiring ]
        let lastCardIsShipAndCanBeRepelled = not state.HarborDisplay.IsEmpty && canRepelShip state.Players.[currentPlayerIdx] state.HarborDisplay.Head
        let repelShip = if lastCardIsShipAndCanBeRepelled then [ RepelShip ] else []
        [ DiscoverCard ] @ beginHiring @ repelShip @ completableExpeditions

    | TradeAndHire(currentPlayerIdx, activePlayerIdx, remainingHireCount) -> 
        let completableExpeditions = completableExpeditions activePlayerIdx
        let possibleHires = 
            if remainingHireCount > 0 then
                state.HarborDisplay
                |> Utils.indices
                |> List.filter (fun idx -> playerCanAffordCard activePlayerIdx idx state)
                |> List.map TakeCard
            else
                []
        [ Pass ] @ possibleHires @ completableExpeditions
            
    | GameEnded(_) -> 
        []
    
let standardDeck: Card seq = 
    seq {
        // Yellow ships
        yield Ship(Yellow, 1, Strength 1)
        yield Ship(Yellow, 1, Strength 1)
        yield Ship(Yellow, 1, Strength 1)
        yield Ship(Yellow, 2, Strength 1)
        yield Ship(Yellow, 2, Strength 2)
        yield Ship(Yellow, 2, Strength 2)
        yield Ship(Yellow, 3, Strength 2)
        yield Ship(Yellow, 3, Strength 4)
        yield Ship(Yellow, 3, Strength 4)
        yield Ship(Yellow, 4, Strength 4)

        // Blue ships
        yield Ship(Blue, 1, Strength 1)
        yield Ship(Blue, 1, Strength 1)
        yield Ship(Blue, 1, Strength 1)
        yield Ship(Blue, 2, Strength 1)
        yield Ship(Blue, 2, Strength 2)
        yield Ship(Blue, 2, Strength 2)
        yield Ship(Blue, 3, Strength 2)
        yield Ship(Blue, 3, Strength 5) // yellow ships have 4 strength, otherwise same
        yield Ship(Blue, 3, Strength 5)
        yield Ship(Blue, 4, Strength 5)

        // Green ships
        yield Ship(Green, 1, Strength 1)
        yield Ship(Green, 1, Strength 1)
        yield Ship(Green, 1, Strength 1)
        yield Ship(Green, 2, Strength 1)
        yield Ship(Green, 2, Strength 3) // blue ships have 2 strength, otherwise same
        yield Ship(Green, 2, Strength 3)
        yield Ship(Green, 3, Strength 3)
        yield Ship(Green, 3, Strength 5)
        yield Ship(Green, 3, Strength 5)
        yield Ship(Green, 4, Strength 5)

        // Red ships
        for _ in [0 .. 2] do
            yield Ship(Red, 1, Strength 1)
            yield Ship(Red, 2, Strength 3)
        for _ in [0 .. 1] do
            yield Ship(Red, 3, Strength 6)
        yield Ship(Red, 3, Invincible)
        yield Ship(Red, 4, Invincible)

        // Black ships
        for _ in [0 .. 2] do
            yield Ship(Black, 1, Strength 2)
            yield Ship(Black, 2, Strength 4)
        for _ in [0 .. 1] do
            yield Ship(Black, 3, Strength 7)
        yield Ship(Black, 3, Invincible)
        yield Ship(Black, 4, Invincible)

        // Traders
        for color in [ Black; Green; Red ] do
            for _ in [0 .. 1] do
                yield Trader(color, 3, 1)
        for color in [ Yellow; Blue ] do
            yield Trader(color, 3, 1)
            yield Trader(color, 5, 2)

        // Citizens
        for kind in [Settler; Priest; Captain] do
            for _ in [0 .. 4] do
                yield Citizen(kind, 4, 1)
        for _ in [0 .. 3] do
            yield Citizen(JackOfAllTrades, 6, 1)

        // Sailors
        for _ in [0 .. 6] do
            yield Sailor(1, 3, 1)
        for _ in [0 .. 1] do
            yield Sailor(1, 5, 2)
        yield Sailor(1, 7, 3)

        // Pirates
        for vp in [1; 2; 3] do
            yield Sailor(2, 3 + 2*vp, vp)

        // Mademoiselles
        for _ in [0 .. 1] do
            yield Mademoiselle(7, 2)
            yield Mademoiselle(9, 3)

        // Jesters
        yield Jester(5, 1)
        for _ in [0 .. 2] do
            yield Jester(7, 2)
        yield Jester(9, 3)

        // Admirals
        yield Admiral(5, 1)
        for _ in [0 .. 2] do
            yield Admiral(7, 2)
        for _ in [0 .. 1] do
            yield Admiral(9, 3)

        // Governors
        for _ in [0 .. 3] do
            yield Governor(8, 0)

        // Tax increases
        for _ in [0 .. 1] do
            yield TaxIncrease(MaxStrength)
            yield TaxIncrease(MinVictoryPoints)

        // Expeditions
        for req in [ Settler; Priest; Captain] do
            yield Expedition([ req; req ], 2, 4)
        yield Expedition([ Priest; Priest; Settler ], 3, 6)
        yield Expedition([ Captain; Captain; Settler ], 3, 6)
    }

let newDefaultGame (playerCount: int): GameState = 
    // TODO special expedition for 5 player game
    let rng = Utils.newRng
    let deck: Card list = Seq.toList(standardDeck) |> Utils.shuffleList rng

    let initialState = 
        { RandomNumberGenerator = rng
          Phase = Discover(0)
          Players = { OwnedCoins = []; PersonalDisplay = [] } |> List.replicate playerCount
          HarborDisplay = []
          Expeditions = []
          DrawPile = deck
          Discard = [] }

    (initialState, [0 .. playerCount - 1]) 
    ||> List.fold (fun state playerIdx -> state |> playerReceivesCoins playerIdx 3)