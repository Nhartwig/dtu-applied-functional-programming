#load "translated.fsx"
open Translated
// Function for randomly generating a tree with max depth l
let randomtree l = 
    let r = System.Random()
    let rec randomtree' n =
        match n with
        | _ when n >= r.Next(1,l) -> [Node(sprintf "%i" n,[])]
        | _ -> [1 .. r.Next(1, l+l)] |> List.map (fun _ -> Node(sprintf "%i" 1, randomtree' (n+1)))
    Node(sprintf "%i" 0,randomtree' 0)

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