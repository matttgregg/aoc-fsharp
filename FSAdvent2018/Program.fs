// Learn more about F# at http://fsharp.org

open FSAdvent2018.Day7

[<EntryPoint>]
let main argv =
    let dataDir = "../data/"
    let file = "day7.dat"
    Day7 (dataDir + file)
    0 // return an integer exit code
