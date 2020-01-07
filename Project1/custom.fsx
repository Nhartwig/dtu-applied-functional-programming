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
let tree =  randomtree 5
            |> (design) 
            |> (scaletree 5.0)

// Time difference
let timeDefault = timeOperation (fun() -> postscript tree)
let timeConcat = timeOperation (fun() -> postscriptConcat tree)
let timeBuilder = timeOperation (fun() -> postscriptStringBuilder tree)

printf "\n\nTime for default: %i\nTime for Concat:  %i\nTime for Builder: %i\n\n" (timeDefault.millisecondsTaken) (timeConcat.millisecondsTaken) (timeBuilder.millisecondsTaken)

// Testing postscript

let postscripttree = (postscriptStringBuilder tree)
//printf "%s" postscripttree

open System.IO;

File.WriteAllText("test.ps", postscripttree)
