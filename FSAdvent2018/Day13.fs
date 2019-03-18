module FSAdvent2018.Day13

open FParsec
open System.IO
open System.Threading

// /->-\        
// |   |  /----\
// | /-+--+-\  |
// | | |  | v  |
// \-+-/  \-+--/
//  \------/

type Track = | Vertical | Horizontal | Cross | TurnUpRight | TurnUpLeft | Empty

type CartDirection = | Up | Down | Left | Right

type TurnDirection =  | TStraight | TRight | TLeft

type Location = {x:int; y:int}

type Cart = { Direction:CartDirection; NextTurn:TurnDirection; Loc:Location }

let nextTurn dir =
    match dir with
    | TLeft -> TStraight
    | TStraight -> TRight
    | TRight -> TLeft


let turnCart track cart =
    let turnCross cDir =
        let nDir = match cart.NextTurn, cDir with
                        | TStraight, d -> d
                        | TLeft, Up -> Left
                        | TLeft, Down -> Right
                        | TLeft, Left -> Down
                        | TLeft, Right -> Up
                        | TRight, Up -> Right
                        | TRight, Down -> Left
                        | TRight, Left -> Up
                        | TRight, Right -> Down
        { cart with Direction = nDir; NextTurn = (nextTurn cart.NextTurn) }
    match cart.Direction, track with
         | Up, TurnUpRight -> { cart with Direction = Right }
         | Left, TurnUpRight -> { cart with Direction = Down }
         | Down, TurnUpRight -> { cart with Direction = Left }
         | Right, TurnUpRight -> { cart with Direction = Up }
         | Up, TurnUpLeft -> { cart with Direction = Left }
         | Left, TurnUpLeft -> { cart with Direction = Up }
         | Down, TurnUpLeft -> { cart with Direction = Right }
         | Right, TurnUpLeft -> { cart with Direction = Down }
         | _, Cross -> turnCross cart.Direction
         | _, _ -> cart

let shiftCart cart =
    let newLoc = match cart.Direction with
                    | Up -> {cart.Loc with y = cart.Loc.y - 1} 
                    | Down -> {cart.Loc with y = cart.Loc.y + 1} 
                    | Left -> {cart.Loc with x = cart.Loc.x - 1} 
                    | Right -> {cart.Loc with x = cart.Loc.x + 1} 
    { cart with Loc = newLoc }

type TrackMap = TrackMap of Map<Location, Track>

let trackAt (l:Location) (TrackMap m) =
    if m.ContainsKey(l) then m.[l] else Horizontal

let iterCart cart m =
    cart |> (turnCart (trackAt cart.Loc m)) |> shiftCart 

// Parsing functions

let pHor = (pchar '-' <|> pchar '>' <|> pchar '<') >>% Horizontal
let pVer = (pchar '|' <|> pchar '^' <|> pchar 'v') >>% Vertical
let pTurn = (pchar '/' >>% TurnUpRight) <|> (pchar '\\' >>% TurnUpLeft)
let pCross = pchar '+' >>% Cross
let pEmpty = (pchar ' ') >>% Empty
let pTrack = many1 (pHor <|> pVer <|> pTurn <|> pCross <|> pEmpty)

let runTrack (l:string) =
    match run pTrack l with
    | Success (r, _, _) -> Some r
    | _ -> None

let tracksOf (lines:string seq) =
    let tracks = TrackMap Map.empty
    let mapped = lines |> Seq.choose runTrack
    let mapLine y (TrackMap ts) (l:Track list) =
        let foldLine (x, tracks) (t:Track) =
            (x+1, Map.add {x=x; y=y} t tracks)
        l
        |> List.fold foldLine (0, ts)
        |> snd
        |> TrackMap
    let foldLines (y, tracks) (l:Track list) =
        (y + 1, mapLine y tracks l)
    Seq.fold foldLines (0, tracks) mapped 
    |> snd
    
let pcUp = pchar '^' >>% Some {Direction = Up; NextTurn = TLeft; Loc = {x=0;y=0}}
let pcDown = pchar 'v' >>% Some {Direction = Down; NextTurn = TLeft; Loc = {x=0;y=0}}
let pcLeft = pchar '<' >>% Some {Direction = Left; NextTurn = TLeft; Loc = {x=0;y=0}}
let pcRight = pchar '>' >>% Some {Direction = Right; NextTurn = TLeft; Loc = {x=0;y=0}}
let pcNone = (pchar '-' <|> pchar '|' <|> pchar '\\' <|> pchar '/' <|> pchar ' ' <|> pchar '+') >>% None
let pCart = many1 (pcUp <|> pcDown <|> pcLeft <|> pcRight <|> pcNone)    

let runpCarts (l:string) = 
    match run pCart l with
    | Success (r,_,_) -> Some r
    | _ -> None

let cartsOf (lines:string seq) = 
    let mapped = lines |> Seq.choose runpCarts
    printfn "Mapped line: %A" mapped
    let mapLine y cs l =
        let foldLine (x, cs) (c:Cart option) =
            match c with 
            | Some ct -> (x+1,  {ct with Loc = {x=x; y=y}} :: cs)
            | _ -> (x+1, cs)
        List.fold foldLine (0, cs) l
        |> snd
    let foldLines (y, cts) (c:Cart option list) =
        (y + 1, mapLine y cts c)
    Seq.fold foldLines (0, []) mapped |> snd

let sortCarts cs =
    let xof {Loc={x=x}} = x
    let yof {Loc={y=y}} = y
    List.sortBy xof cs |> List.sortBy yof

let trackingSet cs = 
    List.map (fun c -> c.Loc) cs |> Set.ofList

type MoverResult = |  Moved of Cart | Crashed of Cart

let iterTracking (m, (mp:Set<Location>), (crashes:Set<Location>), acc) ({Loc=sLoc} as c) = 
    if (Set.contains sLoc crashes) then (m, mp, crashes, Crashed c :: acc) else
    let rem = Set.remove sLoc mp
    let nc = iterCart c m
    //printfn "New location %A : Existing locations %A" nc.Loc rem
    //printfn "Existing crashes %A" crashes
    let mres = if (Set.contains nc.Loc mp || Set.contains nc.Loc crashes)  then 
                Crashed nc  
                else Moved nc
    let nacc = mres :: acc 
    let nmp = Set.add nc.Loc rem
    let ncrashes = match mres with 
                    | Crashed nc -> Set.add nc.Loc crashes
                    | _ -> crashes
    (m, nmp, ncrashes, nacc)

let pushCarts cs m =
    let sorted = sortCarts cs
    let t = trackingSet sorted
    let crashes:Set<Location> = Set.empty
    sorted |> List.fold iterTracking (m, t, crashes, [])

let printCart {Direction = d; Loc={x=x;y=y}} =
    match d with 
    | Up -> sprintf "^ at %d %d" x y
    | Down -> sprintf "v at %d %d" x y
    | Left -> sprintf "< at %d %d" x y
    | Right -> sprintf "> at %d %d" x y

let printMovedCarts c = 
    match c with 
    | Moved c -> printCart c
    | Crashed c -> sprintf "**CRASHED** %s" (printCart c)

let removeCrashes (cts:Cart list) mv =
    let removeOne at (cs:Cart list) =
        let toRemove = List.tryFind (fun c -> c.Loc = at) cs
        match toRemove with
        | Some rem -> List.filter (fun c -> c <> rem) cs
        | _ -> cs
    match mv with 
    | Crashed {Loc=loc} -> cts |> removeOne loc |> removeOne loc
    | _ -> cts

let rec tryPushCarts cs m ct = 
    let (_,_,crsh, res) = pushCarts cs m
    let chooseCrashes mr = match mr with
                            | Crashed  l -> Some l
                            | _ -> None 
    let chooseMoves mr = match mr with 
                            | Moved c -> Some c
                            | Crashed c -> Some c
    let crashes = List.choose chooseCrashes res
    let movedCarts = List.choose chooseMoves res
    let safeCarts = List.fold removeCrashes movedCarts res
    printfn "At step %d" ct
    List.map printMovedCarts res |> String.concat "\n" |> printfn "%s\n"
    if (not (List.isEmpty crashes)) then 
        printfn "Crashes at ! %A" crsh 
        else ()
    //List.map printCart moves |> String.concat "\n" |> printfn "%s\n"
    printfn "After removals..."
    List.map printCart safeCarts |> String.concat "\n" |> printfn "%s\n"
    if (List.length safeCarts <= 1)// || ct > 5) 
        then (ct, safeCarts) 
        else tryPushCarts safeCarts m (ct + 1)
    //if (not (List.isEmpty crashes)) then (ct, crashes) else (tryPushCarts moves m (ct  + 1))


let runDay file =
    printfn "Running day %d !" 13
    let lines = File.ReadAllLines file
    let tm = tracksOf lines
    let cs = cartsOf lines
    printfn "Read Map:\n %A" tm
    let cstring = List.map printCart cs |> String.concat "\n" 
    printfn "***Read Carts***:\n %s" cstring
    let firstCrash = tryPushCarts cs tm 0
    printfn "Last cart! : %A" firstCrash