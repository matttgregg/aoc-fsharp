module FSAdvent2018.Day9

open System.IO

open FParsec

type Game = {stones:int array; players:int; cStone:int; next:int; scores:int[] }

let currentPlayer stone players =
    ((stone - 1) % players)

let showGame (g:Game) =
    let printStone i s =
        if i = g.cStone then (sprintf "(%d)" s) else (string s) 
    let player = currentPlayer (g.next - 1) g.players
    let playerLabel = if (g.next = 1) then "[.]" else (sprintf "[%d]" (player + 1))
    let stones = Array.mapi printStone g.stones |> String.concat " "
    sprintf "%s %s" playerLabel stones

let playStone (g:Game) :Game =
    let breakPoint = ((g.cStone + 1) % (Array.length g.stones)) + 1
    let before = Array.take breakPoint g.stones
    let after = Array.skip breakPoint g.stones
    if breakPoint = 0 then
        let newStones = Array.concat [g.stones; (Array.ofList [g.next])]
        { g with stones = newStones; cStone = (Array.length newStones - 1); next = g.next + 1}
        else
        let newStones = Array.concat [before; (Array.ofList [g.next]); after]
        { g with stones = newStones; cStone = breakPoint; next = g.next + 1}

// Scoring - score next stone, score current - 7, and following stone becomes current 
let scoreStone (g:Game) :Game =
    let stoneCount = Array.length g.stones
    let scoringIndex = (stoneCount + g.cStone - 7) % stoneCount
    let before = Array.take scoringIndex g.stones
    let afterPlusScoring = Array.skip scoringIndex g.stones
    let after = Array.skip 1 afterPlusScoring
    let newStones = Array.concat [before;after]
    let newCurrent = if scoringIndex >= (Array.length g.stones - 1) then 0 else scoringIndex  
    let scorePoints = g.next + (Array.head afterPlusScoring)
    let player = currentPlayer g.next g.players
    //printfn "Player %d scores %d" (player + 1) scorePoints
    let updateScores i sc = if (i = player) then (sc + scorePoints) else sc
    let newScores = Array.mapi updateScores g.scores
    { g with stones = newStones; scores = newScores; next = g.next + 1; cStone = newCurrent }

let play (g:Game) =
    if (g.next % 23 = 0) then scoreStone g else playStone g

let playAcc g _ :Game = 
    let nextG = play g
    if (nextG.next % 100 = 0) then printfn "%d" nextG.next else ()
    nextG

let winner {scores=s} =
    s
    |> Array.zip (Array.init (Array.length s) ((+) 1)) 
    |> Array.maxBy snd 

let score players e =
    let zeroScores = Array.init players (fun _ -> 0)
    let game = {stones=(Array.ofList [0]); players=players; cStone=0; next=1; scores=zeroScores}
    [1..e] 
    |> List.fold playAcc game
    |> winner

let printScore players e =
    let (w, s) = score players e 
    printfn "For %d players, last marble %d, winner is %d with %d points." players e w s    

let runDay (players:int) (finalStone:int) :unit =
    printScore 9 25
    printScore 17 1104
    printScore 21 6111
    printScore 465 7194000
