(* 2 Representing trees *)
type Tree<'a> = Node of 'a * ('a Tree list)

let movetree (Node((label, x), subtrees), x') = 
    Node((label, x+x'), subtrees)

(* 3 representing extents *)
type Extent = (float * float) list

let moveextent (e : Extent, x) = List.map (fun (p,q) -> (p+x, q+x) ) e

let rec merge (ps : Extent) (qs : Extent) =
    match (ps, qs) with
    | (_, []) -> ps
    | ([], _) -> qs
    | ((p,_)::ps', (_,q)::qs') -> (p,q) :: merge ps' qs'

let rec mergelist (es : Extent list) = 
    match es with
    | [] -> []
    | e::es' -> merge e (mergelist es')

(* 4 Fitting extents *)
let rmax (p, q) = if p > q then p else q
let rec fit (ps :Extent) (qs : Extent) = 
    match (ps, qs) with
    | ((_,p)::ps', (q,_)::qs') -> rmax(fit ps' qs', p - q + 1.0)
    | (_,_) -> 0.0

let fitlist1 es =
    let rec fitlist1' acc es =
        match es with
        | [] -> []
        | e::es' -> 
            let x = fit acc e
            x :: fitlist1' (merge acc (moveextent(e, x) )) es'
    fitlist1' [] es

let flipextent (e : Extent) : Extent = List.map (fun (p,q) -> (-q,-p)) e
let fitlistr es = 
    List.rev es
    |> List.map flipextent
    |> fitlist1
    |> List.map (~-)
    |> List.rev

let mean (x,y) = (x+y)/2.0
let fitlist (es: Extent list) = List.map mean (List.zip (fitlist1 es) (fitlistr es))

(* 5 Designing the tree *)

let design tree =
    let rec design' (Node(label, subtrees)) =
        let trees, extents = List.unzip (List.map design' subtrees)
        let positions = fitlist extents
        let ptrees = List.map movetree (List.zip trees positions)
        let pextents = List.map moveextent (List.zip extents positions)
        let resultextent = (0.0, 0.0) :: mergelist pextents
        let resulttree = Node((label, 0.0), ptrees)
        (resulttree, resultextent)
    fst (design' tree)

//let test = design(Node(1,[Node(2,[Node(4,[]); Node(5,[]); Node(6,[]); Node(7,[])]); Node(3,[]); Node(9,[])]))
//printf "%A" test