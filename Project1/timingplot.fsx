#load "translated.fsx"
#load "treegeneration.fsx"
#load "postscript.fsx"
#load "Timing.fsx"
open Translated
open Treegeneration
open Postscript
open Timing

// Defining tree
// Call squareTree to generate square tree x units deep by x units wide
let sqrndtree l =  (randomSquareTree l l) |> (design)
let rndtree l = (randomtree l) |> (design)
let sqtree l = (squareTree l) |> (design)

// generate a tree for each list element
let genTrees tree sizes = List.map (fun size -> (tree size)) sizes

// run each tree through the 3 timing methods
let treesDefault trees = List.map (fun tree -> string((timeOperation (fun() -> postscript' (List.fold (fun s x -> s + (x) ) "" ) tree)).millisecondsTaken) ) trees
let treesConcat trees = List.map (fun tree -> string ((timeOperation (fun() -> postscript' (String.concat "\n") tree)).millisecondsTaken) ) trees
let stringBuilder =  fun xs -> xs |> Seq.fold (fun (sb:System.Text.StringBuilder) (s: string) -> sb.Append(s) ) (System.Text.StringBuilder()) |> fun sb -> sb.ToString()
let treesBuilder trees = List.map (fun tree -> string((timeOperation (fun() -> postscript' (stringBuilder) tree)).millisecondsTaken)) trees


// how many trees to generate
let sizes = [1 .. 8]
let trees = genTrees sqtree sizes

let timesDefault = ["default"]@((trees) |> treesDefault)
let timesConcat = ["Concat"]@((trees) |> treesConcat)
let timesBuilder = ["builder"]@((trees) |> treesBuilder)

let times = timesDefault@timesConcat@timesBuilder
let result = List.fold (fun i j -> i+"\n" + j) "" times
               

open System.IO;

File.WriteAllText("timedata1.txt", result)