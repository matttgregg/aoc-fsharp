// Learn more about F# at http://fsharp.org

open FSAdvent2018.Day8

[<EntryPoint>]
let main argv =
    let dataDir = "../data/"
    let file = "day8.dat"
    runDay (dataDir + file)
    0 // return an integer exit code
