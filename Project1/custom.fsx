#load "translated.fsx"
open Translated
#load "postscript.fsx"
open Postscript
#load "Timing.fsx"
open Timing
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

// Defining objects
let rec scaletree n (Node(x', y)) =
    Node((fst x', (snd x') * n), List.map (scaletree n) y)

let tree = (design (randomtree 7)) |> (scaletree 5.0)

// Time difference

let timeDefault = timeOperation (fun() -> postscript tree)
let timeConcat = timeOperation (fun() -> postscriptConcat tree)

printf "\n\nTime for default: %i\nTime for Concat:  %i\n\n" (timeDefault.millisecondsTaken) (timeConcat.millisecondsTaken)

// Testing postscript

let postscripttree = (postscriptConcat tree)
//printf "%s" postscripttree

open System.IO;

File.WriteAllText("test.ps", postscripttree)
