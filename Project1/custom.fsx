#load "translated.fsx"
open Translated
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