module FSAdvent2018.Day6

open System.IO
open FParsec;

type Point = { x:int; y:int; label:string }

let pPoint = pint32 .>>. ((pchar ',' .>>. spaces) >>. (pint32)) |>> (fun (x,y) -> {x = x; y= y; label=""}) 

let manhattan p q =
    abs (p.x - q.x) + abs (p.y - q.y)
    
let runPoints res =
    match res with
    | Success (pt, _, _) -> Some pt
    | _ -> None

let bounding pts =
    let {x=xmin} = pts |> Seq.minBy (fun {x=x} -> x)
    let {x=xmax} = pts |> Seq.maxBy (fun {x=x} -> x)
    let {y=ymin} = pts |> Seq.minBy (fun {y=y} -> y)
    let {y=ymax} = pts |> Seq.maxBy (fun {y=y} -> y)
    ((xmin, ymin), (xmax, ymax))

let nearest pt points =
    let pointDistance = points |> Seq.map (fun p -> (manhattan pt p), p.label)
    let minPoint = Seq.minBy fst pointDistance
    if ((pointDistance |> Seq.where (fun p -> (fst p) = fst minPoint)) |> Seq.length) > 1 then
        None
        else Some (snd minPoint)
        
let plot points =
    let ((xmin, ymin), (xmax, ymax)) = bounding points
    let charFor n =
        match n with
        | None -> "."
        | Some c -> c
    let rowFor y =
        [(xmin - 1) .. (xmax + 1)]
        |> List.map (fun x -> nearest {x=x; y = y; label=""} points)
        |> List.map charFor
        |> String.concat " " 
    [(ymin - 1) .. (ymax + 1)] |> List.map rowFor |> String.concat "\n"
    
type Range = | Inf | Finite of int

let edgePoints points = 
    let ((xmin, ymin), (xmax, ymax)) = bounding points
    let vals y =
        [(xmin - 1); (xmax + 1)]
        |> List.choose (fun x -> nearest {x=x; y = y; label=""} points)
    let full = [(ymin - 1); (ymax + 1)] |> List.map vals |> List.concat
    full |> Set.ofList

let rangesFor points =
    let edges = edgePoints points
    let ((xmin, ymin), (xmax, ymax)) = bounding points
    let vals y =
        [(xmin - 1) .. (xmax + 1)]
        |> List.map (fun x -> nearest {x=x; y = y; label=""} points)
    let full = [(ymin - 1) .. (ymax + 1)] |> List.map vals |> List.concat
    let accumulate (acc:Map<string, int>) nrst =
        match nrst with
        | Some str ->
            if (acc.ContainsKey(str)) then
                Map.add str (acc.[str] + 1) acc
                else Map.add str 1 acc
        | None -> acc
    full
    |> List.fold accumulate Map.empty
    |> Map.toList
    |> List.map (fun (p, cnt) -> if (Set.contains p edges) then (p, Inf) else (p, Finite cnt))
     
let maxRange pts =
    let allRanges = rangesFor pts
    let sortedRanges = allRanges |> List.sortByDescending (fun (p, rnge) ->
        match rnge with
        | Inf -> 0
        | Finite c -> c)
    sortedRanges |> List.head
    
let distanceToAll points (pt:Point) =
    points |> Seq.map (manhattan pt) |> Seq.sum
    
let plotRangeWithin (within:int) points =
    let ((xmin, ymin), (xmax, ymax)) = bounding points
    let charFor n = if (n < within) then "#" else ","
    let rowFor y =
        [(xmin - 1) .. (xmax + 1)]
        |> List.map (fun x -> distanceToAll points {x=x; y=y; label=""})
        |> List.map charFor
        |> String.concat " "
    [(ymin - 1) .. (ymax + 1)] |> List.map rowFor |> String.concat "\n"
    
let countWithin within points = 
    let ((xmin, ymin), (xmax, ymax)) = bounding points
    let rowFor y =
        [(xmin - 1) .. (xmax + 1)]
        |> List.map (fun x -> distanceToAll points {x=x; y=y; label=""})
        |> List.filter (fun d -> d < within)
        |> List.length
    [(ymin - 1) .. (ymax + 1)] |> List.map rowFor |> List.sum

let withinEdges within points = 
    let ((xmin, ymin), (xmax, ymax)) = bounding points
    let rowFor y =
        [(xmin - 1); (xmax + 1)]
        |> List.map (fun x -> distanceToAll points {x=x; y=y; label=""})
        |> List.filter (fun d -> d < within)
        |> List.length
    [(ymin - 1); (ymax + 1)] |> List.map rowFor |> List.sum

let Day6 file =
    let limit = 10000
    let points = File.ReadLines file |> Seq.map (run pPoint) |> Seq.choose runPoints
    let labelled = points |> Seq.mapi (fun i p -> { p with label = ((char (i + 65)) |> string)})
    printfn "%s" (plotRangeWithin limit labelled)
    printfn "Range size: %A" (countWithin limit labelled)
    printfn "Overflow: %A" (withinEdges limit labelled)