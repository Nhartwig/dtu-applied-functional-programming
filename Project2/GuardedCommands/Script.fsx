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

// testing the test module

let fail = Testing.test [   (0, "gcs-errors/Ex0.gc"); 
                            (0, "gcs-errors/Ex1.gc");   
                            (0, "gcs-errors/Ex1.gc"); 
                            (0, "gcs-errors/do0.gc"); 
                            (0, "gcs-errors/do1.gc");
                            (0, "gcs-errors/do2.gc");   
                            (0, "gcs-errors/while0.gc"); 
                            (0, "gcs-errors/while1.gc"); 
                            (0, "gcs-errors/while2.gc") ]
fail |> Testing.hideExnMsg               

let pass = Testing.test [   (0, "gcs/Ex0.gc"); 
                            (0, "gcs/Ex1.gc");   
                            (0, "gcs/Ex1.gc");  ]

// Test of programs covered by the first task (Section 3.7):
List.iter exec ["gcs/Ex1.gc"; "gcs/Ex2.gc";"gcs/Ex3.gc"; "gcs/Ex4.gc"; "gcs/Ex5.gc"; "gcs/Ex6.gc"; "gcs/Skip.gc"];;

// Tests that should fail in typechecker
Testing.test [  (0, "gcs-errors/GC1IfWrongTypeBool.gc");
                (0, "gcs-errors/GC2DowrongTypeBool.gc");
                (0, "gcs-errors/GC3IfWrongTypeSubIfBool.gc");
                (0, "gcs-errors/GC4DoWrongTypeSubDoBool.gc");
                (0, "gcs-errors/GC5WrongTypeInsideDo.gc")
                ]
|> Testing.hideExnMsg |> Testing.failTypechecker "GC";;

// Test of programs covered by the second task (Section 4.3):
List.iter exec ["gcs/Ex7.gc"; "gcs/fact.gc"; "gcs/factRec.gc"; "gcs/factCBV.gc"];;

// Test of programs covered by the fourth task (Section 5.4):
List.iter exec ["gcs/A0.gc"; "gcs/A1.gc"; "gcs/A2.gc"; "gcs/A3.gc"];;

// Test of programs covered by the actual fifth task (Section 6.1)
List.iter exec ["gcs/MAsg1.gc"; "gcs/MAsg2.gc"; "gcs/MAsg3.gc"; "gcs/Masg4.gc"];;

// Test of programs covered by the fifth task (Section 6.1):
List.iter exec ["gcs/A4.gc"; "gcs/Swap.gc"; "gcs/QuickSortV1.gc"];;
(*
// Test of programs covered by the fifth task (Section 7.4):
List.iter exec ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["gcs/Ex1.gc"; "gcs/Ex2.gc"];;

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["par1.gc"; "factImpPTyp.gc"; "QuickSortV2.gc"; "par2.gc"];;

*)