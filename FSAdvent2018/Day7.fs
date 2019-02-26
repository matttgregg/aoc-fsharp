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
    if (Seq.isEmpty available) then (alreadyDone |> List.rev |> List.map string |> String.concat "")
    else
        let next = Seq.head available
        runTasks rules (next :: alreadyDone)

type WorkOrder = {Task:char; Start:int; End:int}
type Worker = {Id:int; Orders:WorkOrder list}

let completedJobsAt time workers =
    let completedBy worker =
        worker.Orders
        |> List.filter (fun {End=e} -> e <= time)
        |> List.map (fun {Task = t} -> t)
    workers |> List.collect completedBy |> Set.ofList

let assignedJobs workers = 
    let completedBy worker =
        worker.Orders
        |> List.map (fun {Task = t} -> t)
    workers |> List.collect completedBy |> Seq.ofList

let isBusy time worker =
    worker.Orders
    |> List.exists (fun {End=e} -> e > time)

let isFree time worker = not <| isBusy time worker

let freeWorkersAt time workers =
    workers |> List.filter (isFree time)

let makeWorkers n =
    [1..n] |> List.map (fun i -> {Id=i; Orders=[]})

let tryMinBy f things =
    if (List.length things = 0) then None else Some (List.minBy f things)

let nextCompletion time workers =
    let nextForWorker worker =
        worker.Orders
        |> List.filter (fun {End=e} -> e > time)
        |> tryMinBy (fun {End=e} -> e)
    let nextTask =
        workers
        |> List.choose nextForWorker
        |> tryMinBy (fun {End=e} -> e)
    nextTask

let nextCompletionTime time workers =
    let nextCompl = nextCompletion time workers
    match nextCompl with
        | Some {End = e} -> Some e
        | _ -> None

let timeFor (t:char) = 60 + (int t) - 64

type accWorker = {workers:Worker list; jobs:char list;}

let foldWorker time acc worker =
    if (isBusy time worker) || (List.isEmpty acc.jobs) then
        {acc with workers=worker::acc.workers}
    else
        let newJob = List.head acc.jobs
        let restJobs = List.skip 1 acc.jobs
        let newOrder = {Task = newJob; Start=time; End=(time + timeFor newJob)}
        let updatedWorker = {worker with Orders = newOrder::worker.Orders}
        {acc with workers = updatedWorker::acc.workers; jobs=restJobs}


let assignTasksAt rules time workers =
    let completed = completedJobsAt time workers
    let unblocked = allAvailable completed rules 
    let available =
        unblocked
        |> Seq.except (assignedJobs workers) 
        |> Seq.sort
    let assigned =
        workers
        |> List.fold (foldWorker time) {workers=[]; jobs=(Seq.toList available)}
    assigned.workers

let finishedAt workers =
    let finished worker =
        worker.Orders
        |> List.map (fun {End=e} -> e)
        |> List.max
    workers
    |> List.map finished
    |> List.max

let rec runAll time rules workers =
    let newWorkers = assignTasksAt rules time workers
    let nextTick = nextCompletionTime time newWorkers
    match nextTick with
        | None -> newWorkers
        | Some t -> runAll t rules newWorkers

let runningAt time worker = 
    let task = 
      worker.Orders 
      |> List.tryFind (fun o -> o.Start <= time && o.End > time)
    match task with 
    | None -> '.'
    | Some {Task=t} -> t

let rowFor workers t =
    let workerPart =
      workers 
      |> List.map (runningAt t)
      |> List.map string
      |> String.concat " "
    sprintf "%d\t%s" t workerPart
    

let Day7 file =
    let rules = File.ReadLines file |> Seq.map (run pRule) |> Seq.choose justRule
    printfn "Day7!"
    let tst = "Step C must be finished before step A can begin."
    printfn "Test : %A" (run pRule tst)
    printfn "Rules %A" rules
    printfn "Tasks %A" (allTasks rules)
    printfn "Can run %A" (allAvailable Set.empty rules)
    printfn "Run order %s" (runTasks rules [])
    let gang = makeWorkers 5
    let finishedGang = (runAll 0 rules gang)
    printfn "Completed Gang: %A" finishedGang
    let endTime = finishedAt finishedGang
    let byMinute = [0..endTime] |> List.map (rowFor finishedGang) |> String.concat "\n"
    printfn "%s" byMinute 
    printfn "Finished at %d" endTime



