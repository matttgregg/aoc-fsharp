﻿// Learn more about F# at http://fsharp.org

open FSAdvent2018.Day12

[<EntryPoint>]
let main argv =
    let dataDir = "../data/"
    let file = "day12.dat"
    runDay (dataDir + file)
    0 // return an integer exit code
