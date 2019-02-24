module FSAdvent2018.Day3a

open System.IO
open FParsec

// Parse entries like : #123 @ 3,2: 5x4
// Claim ID 123
// At 3 from left, 2 from top
// 5 wide and 4 tall

let ws = spaces
let pid = (pstring "#") >>. pint32 .>> ws
let plocation =
    let start = (pchar '@') .>>. ws
    let nmber = (pint32 .>> (pchar ',')) .>>. ((pint32 .>> (pchar ':' .>>. ws)))
    start >>. nmber
let psize = (pint32 .>> (pchar 'x')) .>>. pint32

let pclaim = pid .>>. (plocation .>>. psize)

type Point = { x:int; y:int }
type Claim = { id:int; location:Point; size:Point }

let pointsIn (claim:Claim) =
    seq { for i in claim.location.x .. (claim.location.x + claim.size.x - 1) do
              for j in claim.location.y .. (claim.location.y + claim.size.y - 1) do
                  yield { x = i; y = j}}

let ReadLine (line:string) =
    match (run pclaim line) with
    | Success((id, ((x,y), (w,h))), _, _) -> Some { id = id; location = {x = x; y = y}; size = {x=w; y = h} }
    | Failure(_,_,_)-> None

type Cloth = Cloth of Map<Point, int list> 

let cut (cloth:Cloth) (claim:Claim) =
    let updateCloth point id (Cloth cloth) =
        match Map.tryFind point cloth with
        | None -> Cloth (Map.add point [id] cloth)
        | Some v -> Cloth (Map.add point (id :: v) cloth)
        
    (pointsIn claim)
        |> Seq.fold (fun c p -> updateCloth p claim.id c) cloth
        
let show (Cloth cloth) =
    let { x = minX}, _ = Map.toSeq cloth |> Seq.minBy (fun ({x = x}, _) -> x) 
    let { x = maxX}, _ = Map.toSeq cloth |> Seq.maxBy (fun ({x = x}, _) -> x)
    let { y = minY}, _ = Map.toSeq cloth |> Seq.minBy (fun ({y = y}, _) -> y)
    let { y = maxY}, _ = Map.toSeq cloth |> Seq.maxBy (fun ({y = y}, _) -> y)
    let showAt x y =
        match (Map.tryFind ({x = x; y = y}) cloth) with
        | Some ids -> if (List.length ids) = 1 then (List.head ids |> string) else "X"
        | None -> "."
    let rowAt y = [(minX - 1) .. (maxX + 1)] |> List.map (fun x -> showAt x y) |> String.concat " "
    let mapString = [(minY - 1) .. (maxY + 1)] |> List.map rowAt |> String.concat "\n"
    let header = sprintf "%i, %i to %i, %i" minX minY maxX maxY
    sprintf "%s\n\n%s" header mapString

let Test file =
    let claims = File.ReadLines file |> Seq.map ReadLine
    let cloth = claims
                |> Seq.choose id
                |> Seq.fold (fun cloth claim -> cut cloth claim) (Cloth (Map.empty))
    let overlaps =
        let (Cloth c) = cloth
        c |> Map.toSeq |> Seq.where (fun (_, v) -> List.length v > 1) |> Seq.length
        
    let allClaims = claims |> Seq.choose id
                           |> Seq.choose (fun c -> Some c.id)
                           |> Set.ofSeq
                           
    let overlappingClaims (Cloth c) =
        let getOverlapping overlapping (_, ids) =
            if (List.length ids) > 1 then (Set.union overlapping (Set.ofList ids)) else overlapping
        c |> Map.toSeq
          |> Seq.fold getOverlapping Set.empty
    
    let nonOverlapping = Set.difference allClaims (overlappingClaims cloth)    
    printfn "%A" nonOverlapping 
