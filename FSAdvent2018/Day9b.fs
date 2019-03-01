module FSAdvent2018.Day9b

open System.IO
open System

open FParsec

type Stone (value:int) = 
    let mutable prev = None:Stone option
    let mutable next = None:Stone option
    member this.Next = defaultArg next this
    member this.Prev = defaultArg prev this
    member this.Value = value
    member this.SetNext (nxt:Stone) = next <- Some nxt
    member this.SetNext (nxt:Stone option) = next <- nxt
    member this.SetPrev (prv:Stone) = prev <- Some prv 
    member this.SetPrev (prv:Stone option) = prev <- prv 
    member this.ShiftBack (n:int) =
         [1..n] |> List.fold (fun (s:Stone) _ -> s.Prev) this
    member this.ShiftForward (n:int) =
         [1..n] |> List.fold (fun (s:Stone) _ -> s.Next) this
    member this.Pop () = 
        this.Prev.SetNext this.Next
        this.Next.SetPrev this.Prev
        (this.Value, this.Next)
    member this.Insert (s:Stone) =
        s.SetPrev this
        s.SetNext this.Next
        this.Next.SetPrev s
        this.SetNext s
        s
    member this.ShowTo (v:int) =
        let foldStone ((stones:string list), (s:Stone)) _ = ((string s.Value) :: stones, s.Next)
        let (svals,_) = [1..v] |> List.fold foldStone ([], this) 
        sprintf "%s" (svals |> List.rev |> String.concat " ")

let makeStone v = 
    let s = Stone(v)
    s.SetPrev s
    s.SetNext s
    s

type Game = {Stones:Stone; NextStone:int; PlayerCount:int; StoneCount:int; Scores:int64[] }

let playStone ({Stones = s; NextStone = n; StoneCount = sc} as game) =
    let updatedStone = s.Next.Insert(Stone(n))
    { game with Stones = updatedStone; NextStone = n+1; StoneCount = sc + 1}

let scoreStone game =
    let (v, moved) = (game.Stones.ShiftBack 7).Pop()
    let toScore = (int64 (v + game.NextStone))
    let scoringPlayer = (game.NextStone - 1) % game.PlayerCount
    Array.set game.Scores scoringPlayer (game.Scores.[scoringPlayer] + toScore)

    {game with Stones = moved; 
                NextStone = game.NextStone + 1;
                StoneCount = game.StoneCount - 1; }

let playOne game =
    if (game.NextStone % 23 = 0) then
        scoreStone game
        else
        playStone game

let showGame {Stones = s; StoneCount = sc; Scores=scores} =
    sprintf "%s \t [%s]" (s.ShowTo sc) (String.concat ":" (Array.map string scores))

let maxScore {Scores = s} =
    Array.max s

let startGame players = {Stones = makeStone(0); 
                NextStone = 1; 
                PlayerCount = players; 
                StoneCount = 1;
                Scores = Array.init players (fun _ -> 0L)}

let runGame players final =
    let accGame g _ = playOne g
    let finalGame = [1..final] |> List.fold accGame (startGame players)
    showGame finalGame 

let quickGame players final =
    let accGame g _ = playOne g
    let finalGame = [1..final] |> List.fold accGame (startGame players)
    sprintf "For %d players, and %d stones, the winning score is %d : [%s]" 
        players 
        final 
        (maxScore finalGame)
        "" //(finalGame.Scores |> Array.map string |> String.concat " ")

let runDay _ =
    let players = 9
    printfn "%A" DateTime.Now
    printfn "%s" (runGame 9 25) 
    printfn "%s" (runGame 9 50) 
    printfn "%s" (quickGame 9 25) 
    printfn "%s" (quickGame 10 1618) 
    printfn "%s" (quickGame 13 7999) 
    printfn "%s" (quickGame 17 1104) 
    printfn "%s" (quickGame 21 6111) 
    printfn "%s" (quickGame 30 5807) 
    printfn "%A" DateTime.Now
    printfn "%s" (quickGame 465 7194000) 
    printfn "%A" DateTime.Now
