module FSAdvent2018.Day7

open System.IO
open FParsec;

type Requirement = Needs of char
type Rule = ForTask of (char * Requirement)

let pRule = (pstring "Step " >>. asciiUpper .>> spaces1)
            .>>. (pstring "must be finished before step " >>. asciiUpper .>> spaces1)
            .>> pstring "can begin." |>> (fun (from,thn) -> ForTask (thn, Needs from))
            
let justRule rule =
    match rule with
    | Success (result, _, _) -> Some result
    | _ -> None
    
let allTasks rules =
    let folder tasks (ForTask (x, Needs y)) =
        tasks |> Set.add x |> Set.add y
    rules |> Seq.fold folder Set.empty
    
let isAvailable alreadyDone rules task =
    let blocking = rules |> Seq.where (fun (ForTask (x, Needs y)) -> x = task)
    let required = blocking |> Seq.map (fun (ForTask (x, Needs y)) -> y) |> Set.ofSeq
    Set.isSubset required alreadyDone
    
let allAvailable alreadyDone rules =
    let tasks = allTasks rules
    tasks
    |> Seq.where (isAvailable alreadyDone rules)
    |> Seq.except (Set.toSeq alreadyDone)
    |> Seq.sortBy int
    
let rec runTasks rules (alreadyDone:char list) =
    let already = Set.ofList alreadyDone
    let available = allAvailable already rules
    if (Seq.length available) = 0 then (alreadyDone |> List.rev |> List.map string |> String.concat "")
    else
        let next = Seq.head available
        runTasks rules (next :: alreadyDone)

let Day7 file =
    let rules = File.ReadLines file |> Seq.map (run pRule) |> Seq.choose justRule
    printfn "Day7!"
    let tst = "Step C must be finished before step A can begin."
    printfn "Test : %A" (run pRule tst)
    printfn "Rules %A" rules
    printfn "Tasks %A" (allTasks rules)
    printfn "Can run %A" (allAvailable Set.empty rules)
    printfn "Run order %s" (runTasks rules [])

