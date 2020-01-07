#load "translated.fsx"
open Translated
// Function for randomly generating a tree with max depth l
let randomtree l = 
    let r = System.Random()
    let rec randomtree' n =
        match n with
        | _ when n >= r.Next(1,l) -> [Node(sprintf "%i" n,[])]
        | _ -> [1 .. r.Next(1, l+l)] |> List.map (fun _ -> Node(sprintf "%i" n, randomtree' (n+1)))
    Node(sprintf "%i" 0,randomtree' 1)

let rec scaletree (n:float) (Node(x', y)) =
    Node((fst x', (snd x') * n), List.map (scaletree n) y)

let willowTree num_levels =
    let rec willowTree' l level = 
        match l,level with
        | _,_ when l=num_levels -> [1..12] |> List.map (fun _ -> Node(sprintf "%i" (num_levels - level), willowTree' 1 (level-1)))
        | _,_ when level > 0 ->  [Node(sprintf "%i" (num_levels - level), willowTree' 1 (level-1))]
        | _,_ -> [Node(sprintf "%i" (num_levels - level),[])]
    Node(sprintf"%i" 0, willowTree' num_levels num_levels)

let squareTree q = 
    match q with
    | _ -> Node(sprintf "", [1..q] |> List.map (fun _ -> willowTree q))

let randomSquareTree width depth =
    let rand = System.Random()
    let randomizedMatrix =
        [1 .. depth]
        |> List.map (fun _ ->
            [1 .. width] |> List.map (fun _ -> rand.Next(0,width))
        ) 
    printf "%A" randomizedMatrix
    let nodes = 
        randomizedMatrix
        |> List.fold (fun s xs -> 
            [0 .. width-1] |> List.map (fun i -> Node("s", (List.zip s xs) |> (List.filter (fun (_, j) -> j = i )) |> (List.map (fst)) ))
        ) ([1 .. width] |> List.map (fun _ -> Node("l", [])))
    Node("r", nodes)