module FSAdvent2018.Day11

let powerOf x y serial = 
    let rid = x + 10
    let power = 
        y * rid
        |> (+) serial
        |> (*) rid
        |> (fun p -> if p < 100 then 0 else (p/100) % 10)
        |> (fun p -> p - 5)
    power

let powerOfSquare size x y serial = 
    let powers = seq {
        for i in [x..x+(size - 1)] do
            for j in [y..y+(size - 1)] do
                yield powerOf i j serial 
    }
    powers |> Seq.sum

let powersInGrid size serial = 
    seq {
        for i in 1..(300 - (size - 1)) do
            for j in 1..(300 - (size - 1)) do
                yield (powerOfSquare size i j serial, (i, j))
    }

let maxForGrid serial size =
    let max = powersInGrid size serial
              |> Seq.maxBy fst
    printfn "Max for %d at size %d is %A" serial size max
    max

let maxAllSizes serial = 
    [1..30]
    |> List.map (fun s -> ((maxForGrid serial s), s))
    |> List.maxBy (fst >> fst) 

let runDay () =
    printfn "Day %d!" 11
    printfn "%d" (powerOf 3 5 8)
    printfn "%d" (powerOf 122 79 57)
    printfn "%d" (powerOf 217 196 39)
    printfn "%d" (powerOf 101 153 71)
    let tryGrid serial = printfn "Max at %d is %A" serial (maxAllSizes serial)
    tryGrid 18
    tryGrid 42
    tryGrid 7347

