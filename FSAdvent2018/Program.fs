// Learn more about F# at http://fsharp.org

open FSAdvent2018.Day4

[<EntryPoint>]
let main argv =
    let dataDir = "/Users/matthewgregg/Code/FSAdvent2018/data/"
    let file = "day4.dat"
    Day4 (dataDir + file)
    0 // return an integer exit code
