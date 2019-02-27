module FSAdvent2018.Day12

open FParsec
open System.IO

type Input = 
    | State of bool list
    | Rule of int*bool

type Pots = Pots of int*bool list

let pPot = (pchar '.' >>% false) <|> (pchar '#' >>% true)

let pState = pstring "initial state:" >>. spaces >>. many pPot |>> State

let pow2 n = 
    [1 .. n] |> List.fold (fun acc _ -> 2 * acc) 1

let hashPattern (x:bool list) =
    x 
    |> List.mapi (fun i p -> if p then (pow2 i) else 0)
    |> List.sum

let pPattern = 
    many pPot .>> spaces1 |>> hashPattern

let pRule = 
    pPattern .>>. (pstring "=>" >>. spaces >>. pPot) |>> fun (r,res) -> Rule (r,res) 

let readFile file =
    let pLine = pState <|> pRule
    let matchState p = 
        match p with
        | Success(State s,_,_) -> Some s
        | _ -> None
    let matchProducers p = 
        match p with
        | Success(Rule (i, true), _, _) -> Some i
        | _ -> None
    let read = File.ReadAllLines file |> Seq.map (run pLine)
    let initial = read |> Seq.choose matchState |> Seq.find (fun _ -> true) 
    let producers = read |> Seq.choose matchProducers |> Set.ofSeq
    (Pots (0, initial), producers)

let evolve (Pots (offset, pots)) (producers:Set<int>) =
    // Pad to five empty pots each side
    let leadingEmpty = pots |> List.takeWhile not |> List.length
    let trailingEmpty = pots |> List.rev |> List.takeWhile not |> List.length
    let addStart = List.init (5 - leadingEmpty) (fun _ -> false)
    let addEnd = List.init (5 - trailingEmpty) (fun _ -> false)
    let ePots = List.concat [addStart;pots;addEnd]
    let acc (collected, rest) _ =
        if (List.length rest < 5) then (collected, List.skip 1 rest)
        else
            let pattern = List.take 5 rest |> hashPattern
            let produced = Set.contains pattern producers
            (produced::collected, List.skip 1 rest) 
    let (fPots, _) = ePots |> List.fold acc ([], ePots)
    Pots (offset - (List.length addStart) + 2, List.rev fPots)

let sumPots  (Pots (offset, pots)) = 
    let sumParts = pots |> List.mapi (fun i has -> if has then
                                                      //printfn "Sumpart %d" (i + offset)
                                                      (i + offset)
                                                      else 0)
    //printfn "%A" sumParts
    let sum = sumParts |> List.sum
    //printfn "Sum %d" sum
    sum

let show (Pots(offset, pots) as ppots) =
    let row = pots |> List.map (fun p -> if p then "#" else ".") |> String.concat ""
    let count = sumPots ppots 
    sprintf "(%d) %s [%d]" count row offset

let showGenerations pots producers n =
    let acc (Pots(offset, pots) as ppots) i = 
        let result = evolve ppots producers
        if (i <= 20L) then
            printfn "[%d] %s" i (show ppots)
        else ()
        result
    [0L..n] |> List.fold acc pots

let runDay file = 
    printfn "Running Day %d" 11
    let testState = "initial state: #..#.#..##......###...###"
    let (initial, producers) = (readFile file)
    let p = showGenerations initial producers 20L 
    //let p = showGenerations initial producers 50000000000L 
    //printfn "%s" (show p)
    ()
  

