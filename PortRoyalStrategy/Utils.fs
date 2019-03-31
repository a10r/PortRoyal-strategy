module PortRoyalStrategy.Utils

open System

// Returns a random number that is greater or equal than 0 and less than the value of the parameter.
type RandomNumberGenerator = int -> int

let newRng: RandomNumberGenerator = 
    let rand = Random()
    fun max -> rand.Next(max)

let seededRng seed: RandomNumberGenerator = 
    let rand = Random(seed)
    fun max -> rand.Next(max)

let pickRandom (rng: RandomNumberGenerator) (list: 'a list): 'a = list.[rng(list.Length)]

let indices (list: 'a list): int list = [0 .. list.Length - 1]

// Applies a mapping to one index in a list, returns the modified list leaving every other index as-is.
let modifyAt (index: int) (change: 'a -> 'a) (list: 'a list): 'a list =
    list |> List.indexed |> List.map (fun (idx, elem) -> if idx = index then change elem else elem)

let replaceAt (index: int) (newElem: 'a) (list: 'a list): 'a list =
    list |> List.indexed |> List.map (fun (idx, elem) -> if idx = index then newElem else elem)

let removeAt (index: int) (list: 'a list): 'a list = 
    list |> List.indexed |> List.filter (fun (i, _) -> i <> index) |> List.map snd

let rec remove (item: 'a) (list: 'a list): 'a list = 
    match list with
    | head :: tail when head = item -> tail
    | head :: tail -> head :: remove item tail
    | [] -> []

// TODO maybe you could write this with List.indexed and some sort of list subset function.
let pickIndices (indices: int list) (list: 'a list): 'a list = 
    let rec pickIndices' (currentIndex: int) (indices: int list) (list: 'a list): 'a list =
        match (indices, list) with
        | ([], _) | (_, []) -> 
            []
        | (nextIndex :: otherIndices, head :: tail) -> 
            if currentIndex = nextIndex then
                head :: (pickIndices' (currentIndex + 1) otherIndices tail)
            else
                (pickIndices' (currentIndex + 1) indices tail)
    pickIndices' 0 indices list        

// Returns the *index* of the first element for which the predicate returns true.
let tryFindFirstIndex (predicate: int -> 'a -> bool) (list: 'a list): int option = 
    let rec tryFindFirstIndex' currentIndex predicate list =
        match list with
        | head :: tail when predicate currentIndex head -> Some currentIndex
        | head :: tail -> tryFindFirstIndex' (currentIndex + 1) predicate tail
        | _ -> None
    tryFindFirstIndex' 0 predicate list

// TODO this is probably quite inefficient (maybe try List.permute?)
// see also: https://stackoverflow.com/questions/10251744/performance-of-list-permute
let rec shuffleList (rng: RandomNumberGenerator) (list: 'a list): 'a list = 
    match list with
    | [] -> []
    | list -> 
        let randomIndex = rng(list.Length)
        list.[randomIndex] :: shuffleList rng (removeAt randomIndex list)
