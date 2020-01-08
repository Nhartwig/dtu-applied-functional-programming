#load "translated.fsx"
#load "treegeneration.fsx"
#load "postscript.fsx"
#load "Timing.fsx"
open Translated
open Treegeneration
open Postscript
open Timing
(* Our own part *)


// Defining tree
// Call squareTree to generate square tree x units deep by x units wide
let tree =  randomtree 1
            //|> (scaletree 5.0)

let trees = treeBatch [100;200;300;400] 

// Time difference
let timeDefault = timeOperation (fun() -> postscript' (List.fold (fun s x -> s + (x) ) "" ) tree)
let timeConcat = timeOperation (fun() -> postscript' (String.concat "\n") tree)
let stringbuilder = fun xs -> xs |> Seq.fold (fun (sb:System.Text.StringBuilder) (s: string) -> sb.Append(s) ) (System.Text.StringBuilder()) |> fun sb -> sb.ToString()
let timeBuilder = timeOperation (fun() -> postscript' (stringbuilder) tree)

printf "\n\nTime for default: %i\nTime for Concat:  %i\nTime for Builder: %i\n\n" (timeDefault.millisecondsTaken) (timeConcat.millisecondsTaken) (timeBuilder.millisecondsTaken)

let runTest tree =
    let timeDefault = timeOperation (fun() -> postscript' (List.fold (fun s x -> s + (x) ) "" ) tree)
    let timeConcat = timeOperation (fun() -> postscript' (String.concat "\n") tree)
    let stringbuilder = fun xs -> xs |> Seq.fold (fun (sb:System.Text.StringBuilder) (s: string) -> sb.Append(s) ) (System.Text.StringBuilder()) |> fun sb -> sb.ToString()
    let timeBuilder = timeOperation (fun() -> postscript' (stringbuilder) tree)
    (timeDefault.millisecondsTaken, timeConcat.millisecondsTaken, timeBuilder.millisecondsTaken)
    
let times = trees |> List.map (runTest)

printf "%A" times
// Testing postscript

let postscripttree = (postscript tree)
//printf "%s" postscripttree

open System.IO;

File.WriteAllText("test.ps", postscripttree)
