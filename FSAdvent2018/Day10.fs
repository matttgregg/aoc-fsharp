module FSAdvent2018.Day10

open System.IO
open FParsec;

// Note that y are *downwards*.

type Position = { x:int64; y:int64}

type Velocity = { vx:int64; vy:int64 }

type Light = {pos:Position; vel:Velocity}

// Parse lines like:
// position=<-3,  6> velocity=< 2, -1>

let pFromCoord f = ((pchar '<' .>> spaces) >>. pint64 .>> spaces) 
                .>>. ((pchar ',' >>. spaces) >>. pint64 .>> spaces) .>> (pchar '>' .>> spaces) 
                |>> f

let pPosition = pFromCoord (fun (px,py) -> {x=px;y=py})
let pVelocity = pFromCoord (fun (px,py) -> {vx=px;vy=py})

let pLine = (pstring "position=" >>. pPosition .>> spaces)
            .>>. (pstring "velocity=" >>. pVelocity)
            |>> fun (p,v) -> {pos = p;vel = v}

let matchParse p =
    match p with 
    | Success (p, _, _) -> Some p
    | _ -> None

let showLights (lights:Light seq) =
    // Put all lights in a dictionary
    let allPos = lights |> Seq.map (fun {pos = p} -> p)
    let minX = allPos |> Seq.map (fun {x=x} -> x) |> Seq.min 
    let maxX = allPos |> Seq.map (fun {x=x} -> x) |> Seq.max 
    let minY = allPos |> Seq.map (fun {y=y} -> y) |> Seq.min 
    let maxY = allPos |> Seq.map (fun {y=y} -> y) |> Seq.max 
    let posSet = Set.ofSeq allPos
    let rowFor y = [minX..maxX] |> List.map (fun x -> if (Set.contains {x=x; y=y}) posSet then "#" else ".") |> String.concat ""
    [minY..maxY] |> List.map rowFor |> String.concat "\n"

let showShape (lights:Light seq) =
    // Put all lights in a dictionary
    let allPos = lights |> Seq.map (fun {pos = p} -> p)
    let minX = allPos |> Seq.map (fun {x=x} -> x) |> Seq.min
    let maxX = allPos |> Seq.map (fun {x=x} -> x) |> Seq.max
    let minY = allPos |> Seq.map (fun {y=y} -> y) |> Seq.min
    let maxY = allPos |> Seq.map (fun {y=y} -> y) |> Seq.max
    let area = (maxX - minX) * (maxY - minY)
    sprintf "%d lights over %d %d to %d %d [%d]" (Seq.length lights) minX minY maxX maxY area

let areaOf (lights:Light seq) =
    // Put all lights in a dictionary
    let allPos = lights |> Seq.map (fun {pos = p} -> p)
    let minX = allPos |> Seq.map (fun {x=x} -> x) |> Seq.min
    let maxX = allPos |> Seq.map (fun {x=x} -> x) |> Seq.max
    let minY = allPos |> Seq.map (fun {y=y} -> y) |> Seq.min
    let maxY = allPos |> Seq.map (fun {y=y} -> y) |> Seq.max
    (maxX - minX) * (maxY - minY)

let moveLights t (lights:Light seq) =
    let move ({x=x; y=y} as p) ({vx=vx; vy=vy} as v) t = {x=(x + vx *  t); y = (y + vy * t)} 
    let moveLight t ({pos=p; vel=v} as l) = {l with pos=(move p v t)}
    lights |> Seq.map (moveLight t)

let runDay file = 
    printfn "Hello Day %d!" 10
    let test = "position=<-3,  6> velocity=< 2, -1>"
    let lights = File.ReadLines file |> Seq.map (run pLine) |> Seq.choose matchParse
    printfn "%s" (showShape lights)
    let doStep l i =
        printfn "After %d" i
        printfn "%s" (showLights (moveLights i l))
        System.Console.ReadLine() |> ignore

    //let converged = [(int64 10000)..(int64 20000)] |> List.map (fun t -> (areaOf (moveLights t lights), t)) |> List.minBy fst |> snd
    //printfn "Converges at %d : %s" converged (showShape (moveLights converged lights))
    //[(converged - (int64 10))..(converged + (int64 10))] |> List.iter (fun t -> printfn "%d:%s" t (showShape (moveLights t lights)))
    [(int64 10080)..(int64 10082)] |> List.iter (doStep lights)


