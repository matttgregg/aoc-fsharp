module FSAdvent2018.Day4

open System
open System.IO
open FParsec

// Read Events like:
// [1518-11-01 00:00] Guard #10 begins shift
// [1518-11-01 00:05] falls asleep
// [1518-11-01 00:25] wakes up

type Guard = { Day:int; GuardId:int }
type Moment = { Day:int; Minute:int }
type GuardEvent = | Arrives of Guard | Sleep of Moment | Awake of Moment

let dayOf (e:GuardEvent) =
    match e with
    | Arrives {Day = d} -> d
    | Sleep {Day = d} -> d
    | Awake {Day = d } -> d
    
let minuteOf (e:GuardEvent) =
    match e with
    | Arrives _ -> 0
    | Sleep {Minute = m } -> m
    | Awake {Minute = m } -> m

let ws = spaces
let pMoment =
    let year = pint32 .>> pchar '-'
    let month = pint32 .>> pchar '-'
    let day = pint32 .>> spaces1
    let hour = pint32 .>> pchar ':'
    let minutes = pint32
    let toDate ((((year, month), day), hour), minutes) =
        let date = new DateTime(year, month, day)
        if hour > 0 then
            { Day = date.DayOfYear + 1; Minute = minutes - 60 }
            else
            { Day = date.DayOfYear; Minute = minutes }
    (pchar '[' >>. year .>>. month .>>. day .>>. hour .>>. minutes .>> pchar ']' .>> spaces) |>> toDate

type parseType = | GuardEv of int | AwakeEv | AsleepEv
let pEvent =
    let toEvent (m:Moment, p:parseType) =
        match p with
        | GuardEv g -> Arrives { Day = m.Day; GuardId = g }
        | AwakeEv -> Awake m
        | AsleepEv -> Sleep m
    let pGuard = (pstring "Guard #" >>. pint32 .>> pstring " begins shift") |>> GuardEv
    let pSleep = (pstring "falls asleep") >>% AsleepEv
    let pAwake = (pstring "wakes up") >>% AwakeEv
    pMoment .>>. (pGuard <|> pSleep <|> pAwake) |>> toEvent
    
let allDays (events:GuardEvent list) = events |> List.map dayOf |> List.distinct |> List.sort

let allGuards events =
    let guardOf e =
        match e with
        | Arrives {GuardId = g} -> Some g
        | _ -> None
    events |> List.choose guardOf |> List.distinct |> List.sort

let guardForDay (events:GuardEvent list) (day:int) =
    let pickGuard (e:GuardEvent) =
        match e with
        | Arrives {GuardId = g; Day = d} -> if d = day then Some g else None
        | _ -> None
    (defaultArg (List.tryPick pickGuard events) -1)
    
let eventsForDay (events:GuardEvent list) (day:int) =
    let isForDay day e =
        match e with
        | Awake {Day=d} -> if (d = day) then Some e else None
        | Sleep {Day=d} -> if (d = day) then Some e else None
        | _ -> None
    events |> List.choose (isForDay day) |> List.sortBy minuteOf
    
let awakeAt (events:GuardEvent list) (minute:int) =
    let lastEvent =
        events |> List.tryFindBack (fun e -> (minuteOf e) <= minute)
    match lastEvent with
    | Some (Sleep _) -> false
    | _ -> true
    
let asleepAt (events:GuardEvent list) (minute:int) = awakeAt events minute |> not
    
let selectEvents parsedEvents =
    let ok parsedEvent =
        match parsedEvent with
        | Success (v, _, _) -> Some v
        | _ -> None
    parsedEvents |> Seq.choose ok

let sleepingForDay events day =
    let dayEvents = eventsForDay events day
    let guard = guardForDay events day
    let sleepingMinutes = [0..59] |> List.where (asleepAt dayEvents) |>  List.length
    (guard, sleepingMinutes)
    
let sleepingGuards events =
    let guards = allGuards events
    let days = allDays events
    let recordDay (sleeping:Map<int, int>) (guard, sleep) =
        if (sleeping.ContainsKey(guard)) then
            Map.add guard (sleep + sleeping.[guard]) sleeping
        else
            Map.add guard sleep sleeping
    days |> List.map (sleepingForDay events) |> List.fold recordDay Map.empty
    
let sleepiestGuard events =
    let guards = allGuards events
    let sleeping = sleepingGuards events
    guards |> List.sortByDescending (fun x -> sleeping.[x]) |> List.head
    
let mostSleepingMinute events guard =
    let days = allDays events
    let asleepAtOn (minute:int) (day:int) =
        (guard = guardForDay events day) && (asleepAt (eventsForDay events day) minute)  
    let daysAsleepAt minute =
        days |> List.where (asleepAtOn minute) |> List.length
    [0..59] |> List.sortByDescending daysAsleepAt |> List.head
    
let sleepiestAtMinute events =
    let days = allDays events
    let guardsByMinute:Map<int,int list> = Map.empty
    let recordMinuteForDay events acc day =
        let dayEvents = eventsForDay events day
        let guard = guardForDay events day
        let update (acc:Map<int, int list>) min =
            if (awakeAt dayEvents min) then acc
            else
                if (acc.ContainsKey(min)) then Map.add min (guard :: acc.[min]) acc
                else
                    Map.add min [guard] acc
        [0..59] |> List.fold update acc
    let allMinuteDays = days |> List.fold (recordMinuteForDay events) guardsByMinute
    let maxForMinute guards = guards |> List.countBy id |> List.maxBy (fun (k,c) -> c)
    let sleepingMinutes = allMinuteDays |> Map.toSeq |> Seq.choose (fun (k, v) -> Some k)
    sleepingMinutes |> Seq.map (fun m -> (m, maxForMinute allMinuteDays.[m]))
                    |> Seq.maxBy (fun (m, (guard, count)) -> count)
    //
    
let lineForDay (events:GuardEvent list) (day:int) =
    let date = new DateTime(int64 1518) + TimeSpan.FromDays(float (day - 1))
    let guard, sleepingMinutes = sleepingForDay events day
    let dayEvents = eventsForDay events day
    let minutes = [0..59] |> List.map (awakeAt dayEvents)
                          |> List.map (fun x -> if x then "." else "#")
                          |> String.concat ""
    sprintf "%d-%d\t#%d\t%s\t%d" date.Month date.Day guard minutes sleepingMinutes 


let Day4 file =
    let events = File.ReadLines file |> Seq.map (run pEvent) |> selectEvents |> List.ofSeq
    //printfn "All events %A" events
    let allDayLines = (allDays events) |> List.map (lineForDay events)
    printfn "%s" (String.concat "\n" allDayLines)
    let sleepiestG = sleepiestGuard events
    printfn "Sleepiest %d" sleepiestG
    let sleepiestM = (mostSleepingMinute events sleepiestG)
    printfn "Sleepiest minute %d" sleepiestM
    printfn "Code = %d" (sleepiestG * sleepiestM)
    let (min, (guard, times)) = (sleepiestAtMinute events)
    printfn "Guard %d is asleep %d times at 00:%d" guard times min
    printfn "CodeB = %d" (guard * min)
