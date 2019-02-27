module FSAdvent2018.Day8

open System.IO

open FParsec

let readInts = many (pint32 .>> spaces)

type node = {children:node list; meta:int list; cchildren:int; cmeta:int}

type readState = {nodes:node list; remaining:int list }

let readHeader data =
    match data with
    | c :: m :: rest -> ({cchildren = c; cmeta = m; children = []; meta = []}, rest)
    | _ -> ({cchildren = 0; cmeta = 0; children = []; meta = []}, data)

let needsRead state =
    match state.nodes with
    | [] -> true
    | h :: rest -> h.cchildren > (List.length h.children)



let nextState state =
    if needsRead state then
        let header,rest = readHeader state.remaining
        {state with nodes = header :: state.nodes; remaining = rest}
    else if (List.length state.nodes) = 1 then
        let topNode = List.head state.nodes
        let meta = List.take (topNode.cmeta) state.remaining
        let restData = List.skip (topNode.cmeta) state.remaining
        let newNode = { topNode with meta = meta }    
        {state with nodes = [newNode]; remaining = restData}
    else
        let topNode = List.head state.nodes
        let parentNode = List.skip 1 state.nodes |> List.head
        let restNodes = List.skip 2 state.nodes
        let meta = List.take (topNode.cmeta) state.remaining
        let restData = List.skip (topNode.cmeta) state.remaining
        let newNode = { topNode with meta = meta }
        if parentNode.cchildren > (List.length parentNode.children) then
            let newParent = { parentNode with children = newNode :: parentNode.children }
            {state with nodes = newParent :: restNodes; remaining = restData}
        else
            {state with nodes = newNode :: parentNode :: restNodes; remaining = restData}
          
let rec sumMeta node =
    let partSums =
        node.children
        |> List.map sumMeta
        |> List.sum
    partSums + (List.sum node.meta)
    
let sumAllMeta nodes =
    nodes
    |> List.map sumMeta
    |> List.sum

let rec value node =
    if (List.isEmpty node.children) then
        node.meta |> List.sum
    else
        node.meta |> List.map (valueMeta node) |> List.sum
and valueMeta node m =
    if (List.length node.children) < m then
        0
    else
        List.item (m - 1) (List.rev node.children) |> value 

let valueAll nodes =
    nodes |> List.map value |> List.sum

let rec parseAll state =
    if (List.length state.remaining) = 0 then state
    else parseAll (nextState state)

let init data =
    { nodes = []; remaining = data }

let realize parsed =
    match parsed with
    | Success (d, _, _) -> d
    | _ -> []

let runDay file =
    let data = File.ReadAllText file |> run readInts |> realize
    printfn "Day8!"
    let s = init data
    let allParsed = parseAll s
    //printfn "%A" allParsed
    printfn "Result = %d" (sumAllMeta allParsed.nodes)
    printfn "Value = %d" (valueAll allParsed.nodes)