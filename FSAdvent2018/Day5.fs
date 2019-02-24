module FSAdvent2018.Day5

open System
open System.IO

let react (x:char) (y:char) =
    ((Char.IsLower x) <> (Char.IsLower y)) && (Char.ToUpper x = Char.ToUpper y) 

let collapse (str:string) =
    let collapseAcc (acc, count, lastChar) currentChar =
        if lastChar = '_' then (acc, count, currentChar)
        else if (react lastChar currentChar) then (acc, count + 1, '_')
        else (lastChar :: acc, count, currentChar)
    let (accumulated, count, finalChar) =
        (str + "_").ToCharArray()
        |> Array.fold collapseAcc ([], 0, '_')
    (List.rev accumulated |> List.map string |> String.concat "", count)

let rec collapseFully (str:string) =
    let (collapsed, collapses) = collapse str
    if (collapses = 0) then collapsed else collapseFully collapsed 

let purgeCollapse (purge:char) (str:string) =
    printfn "Collapsing with %c" purge
    let upperPurge = Char.ToUpper purge
    let purged = str |> String.filter (fun c -> (Char.ToUpper c) <> upperPurge)
    let collapsed = collapseFully purged
    let result = String.length collapsed
    printfn "Finished with %c (%d)" purge result
    result

let Day5 file =
    let data = File.ReadAllText file |> String.filter (Char.IsLetter)
    let purgings = [(int 'A') .. (int 'Z')]
                   |> Array.ofList
                   |> Array.Parallel.map (fun c -> (char c, purgeCollapse (char c) data))
    printfn "%A" (purgings |> Array.sortBy snd)
    
