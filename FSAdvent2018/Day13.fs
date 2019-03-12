module FSAdvent2018.Day13

open FParsec

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
let pEmpty = (pchar ' ') >>% Empty
let pTrack = many1 (pHor <|> pVer <|> pTurn <|> pEmpty)

let runTrack (l:string) =
    match run pTrack l with
    | Success (r, _, _) -> Some r
    | _ -> None

let tracksOf (lines:string seq) =
    let tracks = TrackMap Map.empty
    let mapped = lines |> Seq.choose runTrack
    let mapLine y ts (l:Track list) =
        let foldLine (x, tracks) (t:Track) =
            (x+1, Map.add {x=x; y=y} t tracks)
        l
        |> List.fold foldLine (0, ts)
    let foldLines (y, tracks) (l:Track list) =
        (y + 1, Map.)
    
    
    

let runDay file =
    printfn "Running day %d !" 13