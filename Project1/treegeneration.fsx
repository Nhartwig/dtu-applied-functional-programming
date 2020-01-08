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
     
let createTree nodeAmount leafAmount =
    let rec subtree nodesLeft depth =
        match nodesLeft, depth with
        | nl, _ when nl < 1 -> []
        | _, _ when nodesLeft < leafAmount -> [0 .. (nodesLeft + leafAmount)] |> List.map (fun _ -> Node(sprintf "%i" depth, []))
        | _, _ -> [1 .. leafAmount] |> List.map (fun i -> Node(sprintf "%i" depth, subtree (nodesLeft - (leafAmount * i) - 1) (depth + 1)))
    Node("0", subtree (nodeAmount - 1 - leafAmount) 1)
    
let easyTree nodeAmount =
    let rec easyTree' nodesLeft depth =
        match nodesLeft with
        | 0 -> []
        | _ -> [Node(sprintf "%i" depth, easyTree' (nodesLeft - 1) (depth + 1))]
    Node("0", easyTree' (nodeAmount - 1) 1)
    
let treeBatch treeSizes = treeSizes |> List.map (fun size -> easyTree size)
