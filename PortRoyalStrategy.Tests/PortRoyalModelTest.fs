module PortRoyalStrategy.Tests.PortRoyalModelTest

open PortRoyalStrategy
open PortRoyalStrategy.PortRoyalModel
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``[differentColoredShipCount] calculated correctly``() =
    let hand = [ Ship(Blue, 1, Strength 1); Ship(Blue, 1, Strength 1); Ship(Yellow, 1, Strength 1); Ship(Green, 1, Strength 1) ]
    let count = hand |> differentColoredShipCount
    count |> should equal 3

// used as a placeholder for when you don't care which card it is
let anyCard = Citizen(Settler, 1, 1)

let defaultPlayer = { PersonalDisplay = []; OwnedCoins = [anyCard; anyCard; anyCard]}

let simpleTwoPlayerGame = 
    { RandomNumberGenerator = Utils.seededRng 42
      Phase = Discover(0)
      Players = [ defaultPlayer; defaultPlayer ]
      HarborDisplay = []
      Expeditions = []
      DrawPile = []
      Discard = [] }

[<Fact>]
let ``[playerCanCompleteExpedition] works``() = 
    let doTest (owns: CitizenKind list) (requires: CitizenKind list) (expected: bool) =
        let player = { defaultPlayer with PersonalDisplay = owns |> List.map (fun k -> Citizen(k, 1, 1))}
        let expedition = Expedition(requires, 1, 1)
        let state = { simpleTwoPlayerGame with Expeditions = [ expedition ]; Players = [ player ] }
        playerCanCompleteExpedition 0 0 state |> should equal expected

    doTest [Settler; Priest] [Settler; Priest] true
    doTest [Settler] [Settler; Priest] false
    doTest [Settler] [Settler; Settler] false
    doTest [Settler; Settler] [Settler] true
    doTest [JackOfAllTrades] [Settler] true
    doTest [JackOfAllTrades] [Settler; Priest] false
    doTest [JackOfAllTrades; Settler] [Settler; Settler] true
    doTest [JackOfAllTrades; JackOfAllTrades] [Settler; Priest] true

[<Fact>]
let ``[drawCard] basic case works correctly``() = 
    let state = { simpleTwoPlayerGame with DrawPile = [ Ship(Blue, 1, Strength 1); Ship(Red, 1, Strength 1); ] }
    let (card, newState) = drawCard state

    card |> should equal (Ship(Blue, 1, Strength 1))
    newState.DrawPile |> should equal [ Ship(Red, 1, Strength 1) ]

[<Fact>]
let ``[drawCard] shuffle works correctly``() = 
    // Empty draw pile
    let state = { simpleTwoPlayerGame with Discard = [ Ship(Blue, 1, Strength 1); Ship(Red, 1, Strength 1); ] }
    let (card, newState) = drawCard state

    // Concrete results depend on RNG seed (due to shuffling)
    card |> should equal (Ship(Red, 1, Strength 1))
    newState.DrawPile |> should equal [ Ship(Blue, 1, Strength 1) ]
    newState.Discard |> should equal List.empty<Card>

[<Fact>]
let ``[step] discover card (basic)``() = 
    let state = { simpleTwoPlayerGame with DrawPile = [ Ship(Blue, 1, Strength 1); Ship(Red, 1, Strength 1); ] }
    let newState = step state DiscoverCard

    newState.HarborDisplay |> should equal [ Ship(Blue, 1, Strength 1) ]
    newState.Phase |> should equal (Discover(0))
    newState.DrawPile |> should equal [ Ship(Red, 1, Strength 1) ]

[<Fact>]
let ``[step] discover card (same colored ship, cannot repel)``() = 
    let state = { simpleTwoPlayerGame with 
                    HarborDisplay = [ Ship(Blue, 1, Strength 1) ]
                    DrawPile = [ Ship(Blue, 1, Strength 1); Ship(Red, 1, Strength 1); ]
                    Discard = [ Ship(Green, 1, Strength 1) ] }
    let newState = step state DiscoverCard

    // all cards should be discarded; we automatically move to the TradeAndHire phase
    newState.Discard |> should matchList ([state.DrawPile.Head] @ state.HarborDisplay @ state.Discard)
    newState.HarborDisplay |> should equal List.empty<Card>
    newState.Phase |> should equal (TradeAndHire(0, 0, 1))
    newState.DrawPile |> should equal [ Ship(Red, 1, Strength 1) ]

[<Fact>]
let ``[step] discover card (same colored ship, can repel)``() = 
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [ Sailor(1, 1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    HarborDisplay = [ Ship(Blue, 1, Strength 1) ]
                    DrawPile = [ Ship(Blue, 1, Strength 1); Ship(Red, 1, Strength 1); ]
                    Discard = [ Ship(Green, 1, Strength 1) ]
                    Players = simpleTwoPlayerGame.Players |> Utils.replaceAt 0 currentPlayer }
    let newState = step state DiscoverCard

    // current player automatically repels the ship (since it's the only possible move)
    newState.Discard |> should matchList (state.DrawPile.Head :: state.Discard)
    newState.HarborDisplay |> should equal state.HarborDisplay
    newState.Phase |> should equal (Discover(0))
    newState.DrawPile |> should equal [ Ship(Red, 1, Strength 1) ]

[<Fact>]
let ``[step] discover card (expedition)``() = 
    let expedition = Expedition([Settler; Settler], 4, 4)
    let state = { simpleTwoPlayerGame with DrawPile = [expedition] }
    let newState = step state DiscoverCard

    newState.HarborDisplay |> should equal List.empty<Card>
    newState.Phase |> should equal (Discover(0))
    newState.DrawPile |> should equal List.empty<Card>
    newState.Expeditions |> should matchList [expedition]
    

[<Fact>]
let ``[step] begin hiring (basic)``() =
    let state = { simpleTwoPlayerGame with HarborDisplay = [ Ship(Blue, 1, Strength 1) ]}
    let newState = step state BeginHiring

    newState.Phase |> should equal (TradeAndHire(0, 0, 1))

[<Fact>]
let ``[step] begin hiring, owns governor``() = 
    // player owns governor --> can take 1 extra card
    let currentPlayer = { OwnedCoins = [anyCard; anyCard; anyCard]; PersonalDisplay = [Governor(1, 1)] }
    let state = { simpleTwoPlayerGame with 
                    Phase = Discover(0)
                    HarborDisplay = [ Citizen(Settler, 2, 1) ]
                    Players = simpleTwoPlayerGame.Players |> Utils.replaceAt 0 currentPlayer}
    let newState = step state BeginHiring

    newState.Phase |> should equal (TradeAndHire(0, 0, 2))

[<Fact>]
let ``[step] pass to non-current player, owns governor``() = 
    // player owns governor --> can take 1 extra card
    let activePlayer = { OwnedCoins = [anyCard; anyCard; anyCard]; PersonalDisplay = [Governor(1, 1)] }
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 0, 1)
                    HarborDisplay = [ Citizen(Settler, 2, 1) ]
                    Players = simpleTwoPlayerGame.Players |> Utils.replaceAt 1 activePlayer}
    let newState = step state Pass

    newState.Phase |> should equal (TradeAndHire(0, 1, 2))

[<Fact>]
let ``[step] begin hiring, owns admiral, 5+ cards``() =
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [ Admiral(1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    Phase = Discover(0)
                    DrawPile = List.replicate 10 anyCard
                    HarborDisplay = List.replicate 5 anyCard
                    Players = simpleTwoPlayerGame.Players |> Utils.replaceAt 0 currentPlayer}
    let newState = step state BeginHiring

    // current player gets two extra coins
    newState.Phase |> should equal (TradeAndHire(0, 0, 1))
    newState.Players.[0].OwnedCoins |> should matchList (List.replicate 2 anyCard)
    newState.DrawPile |> should matchList (List.replicate 8 anyCard)

[<Fact>]
let ``[step] begin hiring, owns 2 admirals, 5+ cards``() =
    // tests if stacking of admirals works correctly
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [ Admiral(1, 1); Admiral(1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    Phase = Discover(0)
                    DrawPile = List.replicate 10 anyCard
                    HarborDisplay = List.replicate 5 anyCard
                    Players = simpleTwoPlayerGame.Players |> Utils.replaceAt 0 currentPlayer}
    let newState = step state BeginHiring

    // current player gets 4 extra coins
    newState.Phase |> should equal (TradeAndHire(0, 0, 1))
    newState.Players.[0].OwnedCoins |> should matchList (List.replicate 4 anyCard)
    newState.DrawPile |> should matchList (List.replicate 6 anyCard)

[<Fact>]
let ``[step] begin hiring, owns admiral, less than 5 cards``() =
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [ Admiral(1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    Phase = Discover(0)
                    DrawPile = List.replicate 10 anyCard
                    HarborDisplay = List.replicate 4 anyCard
                    Players = simpleTwoPlayerGame.Players |> Utils.replaceAt 0 currentPlayer}
    let newState = step state BeginHiring

    // current player does not get admiral bonus
    newState.Phase |> should equal (TradeAndHire(0, 0, 1))
    newState.Players.[0].OwnedCoins |> should matchList List.empty<Card>
    newState.DrawPile |> should matchList (List.replicate 10 anyCard)

[<Theory; 
  InlineData(1, 1); 
  InlineData(2, 1);
  InlineData(3, 1); 
  InlineData(4, 2); 
  InlineData(5, 3)>]
let ``[step] begin hiring, X differently colored ships gives Y base hire count``(differentlyColoredShipCount: int, baseHireCount: int) =
    let shipList = [Red; Blue; Green; Yellow; Black] |> List.map (fun c -> Ship(c, 1, Strength 1))
    let state = { simpleTwoPlayerGame with HarborDisplay = shipList |> List.take differentlyColoredShipCount }
    let newState = step state BeginHiring

    newState.Phase |> should equal (TradeAndHire(0, 0, baseHireCount))


[<Fact>]
let ``[step] cannot begin hiring when no card has been discovered``() = 
    let state = { simpleTwoPlayerGame with HarborDisplay = []}
    (fun () -> step state BeginHiring |> ignore) 
    |> should throw typeof<System.Exception>

[<Fact>]
let ``[step] take card (ship)``() = 
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 0, 1)
                    HarborDisplay = [ Ship(Blue, 2, Strength 1) ]
                    DrawPile = [ Ship(Red, 1, Strength 1); Ship(Green, 1, Strength 1) ]}
    let newState = step state (TakeCard(0))

    // two cards are taken from the draw pile; the ship from the harbor display is discarded
    newState.Players.[0].OwnedCoins |> should matchList ([ Ship(Red, 1, Strength 1); Ship(Green, 1, Strength 1) ] @ state.Players.[0].OwnedCoins)
    newState.Discard |> should equal [ Ship(Blue, 2, Strength 1) ]
    newState.HarborDisplay |> should equal List.empty<Card>
    newState.DrawPile |> should equal List.empty<Card>
    // next player's (inactive) turn, since it's not the active player they get a base hire count of 1
    newState.Phase |> should equal (TradeAndHire(0, 1, 1))

[<Fact>]
let ``[step] take card (ship), trader bonus``() = 
    // player owns 1 blue trader --> gains one more coin for the blue ship
    let currentPlayer = { OwnedCoins = [anyCard; anyCard; anyCard]; PersonalDisplay = [Trader(Blue, 1, 1)] }
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 0, 1)
                    HarborDisplay = [ Ship(Blue, 2, Strength 1) ]
                    Players = simpleTwoPlayerGame.Players |> Utils.replaceAt 0 currentPlayer
                    DrawPile = [ Ship(Red, 1, Strength 1); Ship(Green, 1, Strength 1); Ship(Yellow, 1, Strength 1) ] }
    let newState = step state (TakeCard(0))

    // player gains all three cards in the draw pile as coins
    newState.Players.[0].OwnedCoins |> should matchList (state.DrawPile @ state.Players.[0].OwnedCoins)
    newState.Discard |> should equal [ Ship(Blue, 2, Strength 1) ]
    newState.HarborDisplay |> should equal List.empty<Card>
    newState.DrawPile |> should equal List.empty<Card>
    // next player's (inactive) turn, since it's not the active player they get a base hire count of 1
    newState.Phase |> should equal (TradeAndHire(0, 1, 1))

[<Fact>]
let ``[step] take card (hirable card)``() = 
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 0, 1)
                    HarborDisplay = [ Citizen(Settler, 1, 1) ]}
    let newState = step state (TakeCard(0))

    // need to pay one coin for this card
    newState.Players.[0].OwnedCoins.Length |> should equal 2
    newState.Players.[0].PersonalDisplay |> should matchList [ Citizen(Settler, 1, 1) ]
    newState.HarborDisplay |> should equal List.empty<Card>
    newState.Discard.Length |> should equal 1
    // next player's (inactive) turn, since it's not the active player they get a base hire count of 1
    newState.Phase |> should equal (TradeAndHire(0, 1, 1))

[<Fact>]
let ``[step] take card as inactive player (hirable card)``() = 
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 1, 1)
                    HarborDisplay = [ Citizen(Settler, 1, 1) ]}
    let newState = step state (TakeCard(0))

    // need to pay one coin for this card and one coin to the active player
    newState.Players.[0].OwnedCoins.Length |> should equal 4
    newState.Players.[1].OwnedCoins.Length |> should equal 1
    newState.Players.[1].PersonalDisplay |> should matchList [ Citizen(Settler, 1, 1) ]
    newState.HarborDisplay |> should equal List.empty<Card>
    newState.Discard.Length |> should equal 1
    // this is the last inactive player to hire, so the next phase is a discover phase
    newState.Phase |> should equal (Discover(1))

[<Fact>]
let ``[step] leftover cards are discarded``() = 
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 1, 1)
                    HarborDisplay = [ Citizen(Settler, 1, 1) ]}
    let newState = step state Pass

    // need to pay one coin for this card and one coin to the active player
    newState.Players |> should equal state.Players
    newState.DrawPile |> should equal state.DrawPile
    // leftover cards get discarded!
    newState.Discard |> should matchList (state.HarborDisplay @ state.Discard)
    newState.HarborDisplay |> should equal List.empty<Card>
    newState.Phase |> should equal (Discover(1))

[<Fact>]
let ``[step] take card (hirable card), price reduction``() = 
    // player owns 1 mademoiselle --> every purchase costs 1 less
    let currentPlayer = { OwnedCoins = [anyCard; anyCard; anyCard]; PersonalDisplay = [Mademoiselle(1, 1)] }
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 0, 1)
                    HarborDisplay = [ Citizen(Settler, 2, 1) ]
                    Players = simpleTwoPlayerGame.Players |> Utils.replaceAt 0 currentPlayer}
    let newState = step state (TakeCard(0))

    // need to pay one coin for this card (normal price: 2)
    newState.Players.[0].OwnedCoins.Length |> should equal 2
    newState.Players.[0].PersonalDisplay |> should matchList ([ Citizen(Settler, 2, 1) ] @ currentPlayer.PersonalDisplay)
    newState.HarborDisplay |> should equal List.empty<Card>
    newState.Discard.Length |> should equal 1
    // next player's (inactive) turn, since it's not the active player they get a base hire count of 1
    newState.Phase |> should equal (TradeAndHire(0, 1, 1))

[<Fact>]
let ``[step] take card (hirable card), double price reduction``() = 
    // player owns 2 mademoiselles --> every purchase costs 2 less --> free purchase here
    let currentPlayer = { OwnedCoins = [anyCard; anyCard; anyCard]; PersonalDisplay = [Mademoiselle(1, 1); Mademoiselle(1, 1)] }
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 0, 1)
                    HarborDisplay = [ Citizen(Settler, 2, 1) ]
                    Players = simpleTwoPlayerGame.Players |> Utils.replaceAt 0 currentPlayer}
    let newState = step state (TakeCard(0))

    // need to pay nothing for this card (normal price: 2)
    newState.Players.[0].OwnedCoins |> should equal state.Players.[0].OwnedCoins
    newState.Players.[0].PersonalDisplay |> should matchList ([ Citizen(Settler, 2, 1) ] @ currentPlayer.PersonalDisplay)
    newState.HarborDisplay |> should equal List.empty<Card>
    newState.Discard |> should equal state.Discard
    // next player's (inactive) turn, since it's not the active player they get a base hire count of 1
    newState.Phase |> should equal (TradeAndHire(0, 1, 1))

[<Theory; InlineData(11, 0); InlineData(12, 6); InlineData(13, 6)>]
let ``[step] tax reduction, max strength``(coinCount: int, lostCoinCount: int) =
    // Current players (x2) gain 1 bonus coin each; other player loses 6 coins
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [Sailor(1, 1, 1)] }
    let otherPlayer = { OwnedCoins = List.replicate coinCount anyCard; PersonalDisplay = [] }
    let state = { simpleTwoPlayerGame with 
                    Phase = Discover(0)
                    DrawPile = [ TaxIncrease(MaxStrength); anyCard; anyCard ]
                    Players = [ currentPlayer; otherPlayer; currentPlayer ]}
    let newState = step state DiscoverCard

    newState.Players.[0].OwnedCoins |> should matchList [ anyCard ]
    newState.Players.[1].OwnedCoins |> should matchList (List.replicate (coinCount - lostCoinCount) anyCard)
    newState.Players.[2].OwnedCoins |> should matchList [ anyCard ]
    newState.Discard |> should matchList (TaxIncrease(MaxStrength) :: (List.replicate lostCoinCount anyCard))

[<Theory; InlineData(11, 0); InlineData(12, 6); InlineData(13, 6)>]
let ``[step] tax reduction, min victory points``(coinCount: int, lostCoinCount: int) =
    // Current players (x2) gain 1 bonus coin each; other player loses 6 coins
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [] }
    let otherPlayer = { OwnedCoins = List.replicate coinCount anyCard; PersonalDisplay = [Sailor(1, 1, 1)] }
    let state = { simpleTwoPlayerGame with 
                    Phase = Discover(0)
                    DrawPile = [ TaxIncrease(MinVictoryPoints); anyCard; anyCard ]
                    Players = [ currentPlayer; otherPlayer; currentPlayer ]}
    let newState = step state DiscoverCard

    newState.Players.[0].OwnedCoins |> should matchList [ anyCard ]
    newState.Players.[1].OwnedCoins |> should matchList (List.replicate (coinCount - lostCoinCount) anyCard)
    newState.Players.[2].OwnedCoins |> should matchList [ anyCard ]
    newState.Discard |> should matchList (TaxIncrease(MinVictoryPoints) :: (List.replicate lostCoinCount anyCard))

[<Fact>]
let ``[step] discover card (same colored ship, cannot repel), owns jester``() = 
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [ Jester(1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    HarborDisplay = [ Ship(Blue, 1, Strength 1) ]
                    DrawPile = [ Ship(Blue, 1, Strength 1); anyCard ]
                    Discard = [ anyCard ]
                    Players = [ currentPlayer; defaultPlayer ] }
    let newState = step state DiscoverCard

    // all cards should be discarded; we automatically move to the TradeAndHire phase
    newState.Discard |> should matchList ([ state.DrawPile.Head ] @ state.HarborDisplay @ state.Discard)
    newState.HarborDisplay |> should equal List.empty<Card>
    newState.Phase |> should equal (TradeAndHire(0, 0, 1))
    newState.DrawPile |> should equal List.empty<Card>
    // player gains 1 coin due to the jester
    newState.Players.[0].OwnedCoins |> should matchList [ anyCard ]

[<Fact>]
let ``[step] jester bonus for non-current players``() = 
    let activePlayer = { OwnedCoins = []; PersonalDisplay = [ Jester(1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 0, 0)
                    HarborDisplay = [ ] // empty!
                    DrawPile = [ anyCard ]
                    Players = [ defaultPlayer; activePlayer ] }
    let newState = step state Pass

    newState.HarborDisplay |> should equal List.empty<Card>
    newState.Phase |> should equal (TradeAndHire(0, 1, 1))
    newState.Discard |> should equal List.empty<Card>
    newState.DrawPile |> should equal List.empty<Card>
    // player gains 1 coin due to the jester
    newState.Players.[1].OwnedCoins |> should matchList [ anyCard ]

[<Fact>]
let ``[step] complete expedition, 1 settler``() =
    let expedition = Expedition([Settler], 2, 4)
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [ Citizen(Settler, 1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    DrawPile = [ anyCard; anyCard ]
                    Expeditions = [ expedition ]
                    Players = [ currentPlayer; defaultPlayer ] }
    let newState = step state (CompleteExpedition(0))

    newState.Players.[0].PersonalDisplay |> should matchList [ expedition ]
    newState.Players.[0].OwnedCoins |> should matchList [ anyCard; anyCard ] // player gets 2 bonus coins
    newState.Discard |> should matchList [ Citizen(Settler, 1, 1) ]
    newState.Expeditions |> should equal List.empty<Card>
    newState.DrawPile |> should equal List.empty<Card>

[<Fact>]
let ``[step] complete expedition, 1 settler, dummy cards``() =
    let expedition = Expedition([Settler], 2, 4)
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [ Citizen(Priest, 1, 1); Citizen(Settler, 1, 1); Citizen(Settler, 1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    DrawPile = [ anyCard; anyCard ]
                    Expeditions = [ expedition; expedition ]
                    Players = [ currentPlayer; defaultPlayer ] }
    let newState = step state (CompleteExpedition(1))

    newState.Players.[0].PersonalDisplay |> should matchList [ expedition; Citizen(Priest, 1, 1); Citizen(Settler, 1, 1) ]
    newState.Players.[0].OwnedCoins |> should matchList [ anyCard; anyCard ] // player gets 2 bonus coins
    newState.Discard |> should matchList [ Citizen(Settler, 1, 1) ]
    newState.Expeditions |> should equal [ expedition ]
    newState.DrawPile |> should equal List.empty<Card>

[<Fact>]
let ``[step] complete expedition, does not unnecessarily use JackOfAllTrades``() =
    let expedition = Expedition([Settler], 2, 4)
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [ Citizen(JackOfAllTrades, 1, 1); Citizen(Settler, 1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    DrawPile = [ anyCard; anyCard ]
                    Expeditions = [ expedition ]
                    Players = [ currentPlayer; defaultPlayer ] }
    let newState = step state (CompleteExpedition(0))

    newState.Players.[0].PersonalDisplay |> should matchList [ expedition; Citizen(JackOfAllTrades, 1, 1) ]
    newState.Players.[0].OwnedCoins |> should matchList [ anyCard; anyCard ] // player gets 2 bonus coins
    newState.Discard |> should matchList [ Citizen(Settler, 1, 1) ]
    newState.Expeditions |> should equal List.empty<Card>
    newState.DrawPile |> should equal List.empty<Card>

[<Fact>]
let ``[step] complete expedition, multiple requirements``() =
    let expedition = Expedition([Settler; Captain], 2, 4)
    let currentPlayer = { OwnedCoins = []; PersonalDisplay = [ Citizen(Priest, 1, 1); Citizen(JackOfAllTrades, 1, 1); Citizen(Settler, 1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    DrawPile = [ anyCard; anyCard ]
                    Expeditions = [ expedition ]
                    Players = [ currentPlayer; defaultPlayer ] }
    let newState = step state (CompleteExpedition(0))

    newState.Players.[0].PersonalDisplay |> should matchList [ expedition; Citizen(Priest, 1, 1);  ]
    newState.Players.[0].OwnedCoins |> should matchList [ anyCard; anyCard ] // player gets 2 bonus coins
    newState.Discard |> should matchList [ Citizen(Settler, 1, 1); Citizen(JackOfAllTrades, 1, 1) ]
    newState.Expeditions |> should equal List.empty<Card>
    newState.DrawPile |> should equal List.empty<Card>

[<Fact>]
let ``[step] game ends once a player has at least 12 victory points``() =
    let winningPlayer = { defaultPlayer with PersonalDisplay = List.replicate 6 (Citizen(Settler, 1, 2))}
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 1, 0)
                    Players = [winningPlayer; defaultPlayer] }
    let newState = step state Pass

    newState.Phase |> should equal (GameEnded(0))

[<Fact>]
let ``[step] game end, tiebreaker``() =
    // both players have 12 victory points, winning player has 1 more gold
    let winningPlayer = { OwnedCoins = [ anyCard ]; PersonalDisplay = List.replicate 6 (Citizen(Settler, 1, 2))}
    let otherPlayer = { OwnedCoins = [];  PersonalDisplay = List.replicate 6 (Citizen(Settler, 1, 2))}
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 1, 0)
                    Players = [otherPlayer; winningPlayer] }
    let newState = step state Pass

    newState.Phase |> should equal (GameEnded(1))

[<Fact>]
let ``[validActions] discover phase, beginning``() =
    validActions simpleTwoPlayerGame |> should matchList [ DiscoverCard ]

[<Fact>]
let ``[validActions] discover phase, 1 already discovered``() =
    let state = { simpleTwoPlayerGame with HarborDisplay = [ Ship(Blue, 1, Strength 1) ] }
    validActions state |> should matchList [ DiscoverCard; BeginHiring ]

[<Fact>]
let ``[validActions] discover phase, repellable ship``() =
    let player = { defaultPlayer with PersonalDisplay = [ Sailor(1, 1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    HarborDisplay = [ Ship(Blue, 1, Strength 1) ] 
                    Players = [ player ]}
    validActions state |> should matchList [ DiscoverCard; RepelShip; BeginHiring ]

[<Fact>]
let ``[validActions] trade and hire phase``() =
    let player = { defaultPlayer with OwnedCoins = [ anyCard; anyCard ] }
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 0, 1)
                    HarborDisplay = [ Ship(Blue, 1, Strength 1); Citizen(Settler, 1, 1); Citizen(Settler, 10, 1) ] 
                    Players = [ player ]}
    validActions state |> should matchList [ TakeCard(0); TakeCard(1); Pass ]

[<Fact>]
let ``[validActions] trade and hire phase, expeditions``() =
    let player = { defaultPlayer with OwnedCoins = [ anyCard; anyCard ]; PersonalDisplay = [ Citizen(Settler, 1, 1) ] }
    let state = { simpleTwoPlayerGame with 
                    Phase = TradeAndHire(0, 0, 1)
                    Expeditions = [ Expedition([Settler; Priest], 1, 1); Expedition([Settler], 1, 1)]
                    HarborDisplay = [ Ship(Blue, 1, Strength 1); Citizen(Settler, 1, 1); Citizen(Settler, 10, 1) ] 
                    Players = [ player ]}
    validActions state |> should matchList [ TakeCard(0); TakeCard(1); Pass; CompleteExpedition(1) ]