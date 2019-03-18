module FSAdvent2018.Day14

type RecipeArr = {Reps:int array; Size:int}
type ScoreCard = {Recipes:RecipeArr; Imp1:int; Imp2:int;}


let desc (sc:ScoreCard) =
    sc.Recipes |> List.mapi (fun i v -> 
                                if i = sc.Imp1 
                                then (sprintf "(1)[%d]" v)
                                else if i = sc.Imp2 then (sprintf "(2)[%d]" v)
                                else (sprintf "%d" v))
    |> String.concat " "

let iterCard sc = 
    let toSingleInt = fun x -> (int x) - (int '0')
    let sc1 = sc.Recipes.[sc.Imp1]
    let sc2 = sc.Recipes.[sc.Imp2]
    let nrs = (sc1 + sc2) 
                |> string 
                |> Seq.map toSingleInt 
                |> Seq.toList 
                |> List.rev
                |> fun x -> List.append x sc.Recipes
    let ni1 = (sc.Imp1 + sc1 + 1) % List.length nrs
    let ni2 = (sc.Imp2 + sc2 + 1) % List.length nrs
    { Recipes = nrs; Imp1 = ni1; Imp2 = ni2}

let InitCard () = {Recipes = [3;7]; Imp1 = 0; Imp2 = 1}

let rGenerator s = 
    let nxt = iterCard s
    Some (nxt, nxt)

let growUntil r0 ln = 
    let cards = Seq.unfold rGenerator r0
    cards |> Seq.skipWhile (fun sc -> List.length sc.Recipes < ln) |> Seq.head

let nextNAfter r0 n after = 
    let term = growUntil r0 (n + after)
    let nextN = term.Recipes |> List.skip after |>  List.take n |> List.map string |> String.concat ""
    (term, nextN)

let solveFor n = 
    let r0 = InitCard ()
    let tm, res = nextNAfter r0 10 n
    //printfn "Recipe Card : %s" (desc tm)
    printfn "Next 10 after %d is %s" n res

let runDay () = 
    printfn "Run day %d" 14
    let cards = Seq.unfold rGenerator
    let r0 = InitCard ()
    printfn "%s" (desc r0)
    printfn "%s" (desc (iterCard r0))
    let cards = Seq.unfold rGenerator r0
    cards |> Seq.take 10 |> Seq.iteri (fun i x -> printfn "%d : %s" i (desc x))
    solveFor 9
    solveFor 5
    solveFor 18
    solveFor 2018
    solveFor 290431