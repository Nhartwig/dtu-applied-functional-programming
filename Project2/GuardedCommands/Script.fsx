(* Take the three links to where you store your packages and paste into a file called "PersonalSettings.fsx"
It should look something like this
#r @"<project>\GuardedCommands\packages\FsLexYacc.Runtime.7.0.6\lib\portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10\FsLexYacc.Runtime.dll";
#r @"<project>\GuardedCommands\bin\Debug\Machine.dll";
#r @"<project>\GuardedCommands\bin\Debug\VirtualMachine.dll";
*)

#read "PersonalSettings.fsx"
#load "PersonalSettings.fsx"
 
#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "TypeCheck.fs"
#load "CodeGen.fs"
#load "CodeGenOpt.fs"
#load "Util.fs"
#load "Testing.fsx"

open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

open Machine
open VirtualMachine
open Testing

System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;

// testing the test module

let fail = Testing.test [(0, "gcs-errors/Ex0.gc"); (0, "gcs-errors/Ex1.gc")]
let pass = Testing.test [(0, "gcs/Ex1.gc"); (0, "gcs/Ex2.gc"); (0, "gcs/Ex3.gc"); (0, "gcs/Ex4.gc"); (0, "gcs/Ex5.gc"); (0, "gcs/Ex6.gc"); (0, "gcs/Skip.gc")]

// The Ex0.gc example:

let ex0Tree = parseFromFile "gcs/Ex0.gc";;

let _ = tcP ex0Tree;;

let ex0Code = CP ex0Tree;; 

let _ = go ex0Tree;;

let _ = goTrace ex0Tree;;


// Parsing of Ex1.gc

let ex1Tree = parseFromFile "gcs/Ex1.gc";; 

// -- is typechecked as follows:

let _ = tcP ex1Tree;;

// obtain symbolic code:
let ex1Code = CP ex1Tree;; 

// -- is executed with trace as follows:
let stack = goTrace ex1Tree;;

// -- is executed as follows (no trace):
let sameStack = go ex1Tree;;

// "All in one" parse from file, type check, compile and run 

let _ = exec "gcs/Ex1.gc";;

let _ = exec "gcs/Ex2.gc";;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["gcs/Ex1.gc"; "gcs/Ex2.gc"];;

// All programs relating to the basic version can be parsed:
let pts = List.map parseFromFile ["gcs/Ex1.gc"; "gcs/Ex2.gc";"gcs/Ex3.gc"; "gcs/Ex4.gc"; "gcs/Ex5.gc"; "gcs/Ex6.gc"; "gcs/Skip.gc"];;

// The parse tree for Ex3.gc
List.item 2 pts ;;

// Test of programs covered by the first task (Section 3.7):
List.iter exec ["gcs/Ex1.gc"; "gcs/Ex2.gc";"gcs/Ex3.gc"; "gcs/Ex4.gc"; "gcs/Ex5.gc"; "gcs/Ex6.gc"; "gcs/Skip.gc"];;
(*
// Test of programs covered by the second task (Section 4.3):
List.iter exec ["Ex7.gc"; "fact.gc"; "factRec.gc"; "factCBV.gc"];;

// Test of programs covered by the fourth task (Section 5.4):
List.iter exec ["A0.gc"; "A1.gc"; "A2.gc"; "A3.gc"];;

// Test of programs covered by the fifth task (Section 6.1):
List.iter exec ["A4.gc"; "Swap.gc"; "QuickSortV1.gc"];;

// Test of programs covered by the fifth task (Section 7.4):
List.iter exec ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

*)