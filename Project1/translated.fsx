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

(* Our own part *)

//let test = design(Node(1,[Node(2,[Node(4,[]); Node(5,[]); Node(6,[]); Node(7,[])]); Node(3,[]); Node(9,[])]))
//printf "%A" test

// Function for randomly generating a tree with max depth l
let randomtree l = 
    let r = System.Random()
    let rec randomtree' n =
        match n with
        | _ when n >= r.Next(1,l) -> [Node(sprintf "%i" n,[])]
        | _ -> [1 .. r.Next(1, l+l)] |> List.map (fun _ -> Node(sprintf "%i" 1, randomtree' (n+1)))
    Node(sprintf "%i" 0,randomtree' 0)
//printf "%A" (randomtree 5)

// function for generating postscript
let postscript tree =
    let start = "%!
<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice
1 1 scale
700 999 translate
newpath
/Times-Roman findfont 10 scalefont setfont\n"
    let ending = "stroke\nshowpage"

    let positions x (subtree : Tree<string * float> list) =
        List.map (fun (Node(x',_)) -> x + (int (snd x')) ) subtree

    let drawLines pos y =
        List.map (fun x -> (sprintf "%i %i moveto\n" x (y) ) + (sprintf "%i %i lineto\n" x (y-10)) ) pos
        |> List.fold (+) ""

    let rec postscript' (x : int,y : int) ( Node((label, X'), subtree) : Tree<string * float> ) =
        match subtree with
        | [] -> sprintf "%i %i moveto\n" x (y-10) + sprintf "(%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" label
        | _ ->  let X = int X'
                let pos = positions (X+x) subtree
                sprintf "%i %i moveto\n" (X+x) (y-10) +
                sprintf "(%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" label +
                sprintf "%i %i moveto\n" (X+x) (y-13) +
                sprintf "%i %i lineto\n" (X+x) (y-40) +
                sprintf "%i %i moveto\n" (List.min pos) (y-40) +
                sprintf "%i %i lineto\n" (List.max pos) (y-40) +
                (drawLines pos (y-40)) +
                //"stroke\n" +
                ( (List.map (postscript' (X+x,y-50)) subtree)
                    |> List.fold (+) "")
    
    let middle = postscript' (0, 0) tree
    start + middle + ending

// Testing postscript
let rec scaletree n (Node(x', y)) =
    Node((fst x', (snd x') * n), List.map (scaletree n) y)

let tree = (design (randomtree 5)) |> (scaletree 5.0)

let postscripttree = (postscript tree)
printf "%s" postscripttree

open System.IO;

File.WriteAllText("test.ps", postscripttree)