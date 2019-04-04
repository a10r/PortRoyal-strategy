module PortRoyalStrategy.Tests.UtilsTest

open PortRoyalStrategy
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Seeded RNG is deterministic``() = 
    let generateRandomSequence rng = List.init 10 (fun _ -> 100) |> List.map rng

    let rng = Utils.seededRng 42
    let rng2 = Utils.seededRng 42

    generateRandomSequence rng |> should equal (generateRandomSequence rng2)

    let rng3 = Utils.seededRng 43

    generateRandomSequence rng |> should not' (equal (generateRandomSequence rng3))

[<Fact>]
let ``RNG implementation is not immutable``() = 
    let generateRandomSequence rng = List.init 10 (fun _ -> 100) |> List.map rng
    
    let originalRng = Utils.seededRng 42

    // copying the rng does NOT create a copy of the underlying randomizer
    let rng1 = originalRng 
    let rng2 = originalRng

    let seq1 = generateRandomSequence rng1
    let seq2 = generateRandomSequence rng2

    seq1 |> should not' (equal (seq2))

[<Fact>]
let ``Shuffle list``() =
    let list = [1; 2; 3; 4; 5]
    let rng = Utils.seededRng 42
    let shuffledList = Utils.shuffleList rng list

    // Order has been randomized
    shuffledList |> should not' (equal list)
    // Elements are still the same (regardless of order)
    shuffledList |> should matchList list

[<Fact>]
let ``Pick indices``() =
    let list = [1; 2; 3; 4; 5]
    let indices = [0; 3]
    let picked = Utils.pickIndices indices list

    picked |> should equal [1; 4]

[<Fact>]
let ``Pick indices, pick none``() =
    let list = [1; 2; 3; 4; 5]
    let indices = []
    let picked = Utils.pickIndices indices list

    picked |> should equal List.empty<int>

[<Fact>]
let ``Pick indices, out of bounds indices are ignored``() =
    let list = [1; 2; 3; 4; 5]
    let indices = [1; 10]
    let picked = Utils.pickIndices indices list

    picked |> should equal [2]

[<Fact>]
let ``[tryFindFirstIndex] match exists``() =
    let list = [1; 2; 3; 2; 1]
    let idx = list |> Utils.tryFindFirstIndex (fun idx elem -> elem = 2) 
    idx |> should equal (Some 1)

[<Fact>]
let ``[tryFindFirstIndex] match does not exist``() =
    let list = [1; 2; 3; 2; 1]
    let idx = list |> Utils.tryFindFirstIndex (fun idx elem -> elem = 4) 
    idx |> should equal None