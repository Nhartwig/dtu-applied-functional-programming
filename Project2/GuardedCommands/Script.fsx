﻿(* Take the three links to where you store your packages and paste into a file called "PersonalSettings.fsx"
It should look something like this:
#r @"<project>\GuardedCommands\packages\FsLexYacc.Runtime.7.0.6\lib\portable-net45+netcore45+wpa81+wp8+MonoAndroid10+MonoTouch10\FsLexYacc.Runtime.dll";
#r @"<project>\GuardedCommands\bin\Debug\Machine.dll";
#load "VirtualMachine.fs";
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

let ex0Code = CPOpt true ex0Tree;; 

let _ = goOpt ex0Tree;;

let _ = goOptTrace ex0Tree;;


// Parsing of Ex1.gc

let ex1Tree = parseFromFile "gcs/Ex1.gc";; 

// -- is typechecked as follows:

let _ = tcP ex1Tree;;

// obtain symbolic code:
let ex1Code = CP true ex1Tree;; 

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
fail |> Testing.hideExnMsg;;

let pass = Testing.test [   (0, "gcs/Ex0.gc"); 
                            (0, "gcs/Ex1.gc");   
                            (0, "gcs/Ex1.gc");  ];;

// Test of programs covered by the first task (Section 3.7):
List.iter exec ["gcs/Ex1.gc"; "gcs/Ex2.gc";"gcs/Ex3.gc"; "gcs/Ex4.gc"; "gcs/Ex5.gc"; "gcs/Ex6.gc"; "gcs/Skip.gc"];;

// Tests that should fail in typechecker
Testing.test [  (0, "gcs-errors/GC1IfWrongTypeBool.gc");
                (0, "gcs-errors/GC2DowrongTypeBool.gc");
                (0, "gcs-errors/GC3IfWrongTypeSubIfBool.gc");
                (0, "gcs-errors/GC4DoWrongTypeSubDoBool.gc");
                (0, "gcs-errors/GC5WrongTypeInsideDo.gc")
                ]
|> Testing.hideExnMsg |> Testing.failTC "GC";;

// Test of programs covered by the second task (Section 4.3):
List.iter exec ["gcs/Ex7.gc"; "gcs/fact.gc"; "gcs/factRec.gc"; "gcs/factCBV.gc"];;

// Tests that should fail in typechecker
Testing.test [  (0, "gcs-errors/F1WrongReturnType.gc");
                (0, "gcs-errors/F2WrongReturnType.gc");
                (0, "gcs-errors/F3TooFewArguments.gc");
                (0, "gcs-errors/F4TooManyArguments.gc");
                (0, "gcs-errors/F5ReturnInMain.gc");
                (0, "gcs-errors/F6WrongTypeAssInFunc.gc");
                (0, "gcs-errors/F7FuncDecInFuncDec.gc");
                (0, "gcs-errors/LD1TypeOverride.gc");
                (0, "gcs-errors/LD2FuncDec.gc")
                ]
|> Testing.hideExnMsg |> Testing.failTC "Func and LD";;

// Test of programs covered by the fourth task (Section 5.4):
List.iter exec ["gcs/A0.gc"; "gcs/A1.gc"; "gcs/A2.gc"; "gcs/A3.gc"];;

// Tests that should fail in typechecker
Testing.test [  (0, "gcs-errors/A1ArrayWithoutSize.gc");
                (0, "gcs-errors/A2AssignWrongType.gc");
                (0, "gcs-errors/A3ArrayWithSizeAsFuncArg.gc");
                (0, "gcs-errors/A4LocDecArrayWithoutSize.gc");
                (0, "gcs-errors/A5AssignWrongTypeToArrayIndex.gc");
                (0, "gcs-errors/A6PassWrongTypeArrayToFunc.gc");
                (0, "gcs-errors/A7BoolAsArrayIndex.gc");
                (0, "gcs-errors/A8IndexOfNonArray.gc");
                ]
|> Testing.hideExnMsg |> Testing.failTC "Array";;

// Test of programs covered by the actual fifth task (Section 6.1)
List.iter exec ["gcs/MAsg1.gc"; "gcs/MAsg2.gc"; "gcs/MAsg3.gc"; "gcs/MAsg4.gc";
                 "gcs/MAIntTypeIntTypeAssignments.gc";   
                    "gcs/MACorrectTypeToMultiple.gc"; 
                    "gcs/MACorrectSizeEitherSide1.gc"; 
                    "gcs/MACorrectSizeEitherSide2.gc";
                    "gcs/MAAssignmentToTwoInts#1.gc"; 
                    "gcs/MAAssignmentToTwoInts#2.gc" ];;

Testing.test [  (0, "gcs-errors/MAWrongTypeToOne.gc");   
                (0, "gcs-errors/MAWrongTypeToMultiple.gc"); 
                (0, "gcs-errors/MAWrongSizeEitherSide1.gc"); 
                (0, "gcs-errors/MAWrongSizeEitherSide2.gc"); ]
                
|> Testing.hideExnMsg  |> Testing.failTC "Multiple Assignments";; 


// Test of programs covered by the fifth task (Section 6.1):
List.iter exec ["gcs/A4.gc"; "gcs/Swap.gc"; "gcs/QuickSortV1.gc"; "gcs/P1IntProcedureType.gc" ];;

Testing.test [  (0, "gcs-errors/P1WrongProcedureType.gc");
                (0, "gcs-errors/P2WrongProcedureType.gc");
                (0, "gcs-errors/P3TooFewArgs.gc");
                (0, "gcs-errors/P4TooManyArgs.gc");
                (0, "gcs-errors/P5WrongTypeAssInProc.gc");
                (0, "gcs-errors/P6ProcDecInProcDec.gc");
                ]
|> Testing.hideExnMsg |> Testing.failTC "Procedure"

// Test of programs covered by the fifth task (Section 7.4):
List.iter exec ["gcs/Po1.gc"; "gcs/Po2.gc"; "gcs/Po3.gc"; "gcs/Po4.gc"; "gcs/Po5.gc"; "gcs/par1.gc"; "gcs/factImpPTyp.gc"; "gcs/QuickSortV2.gc"; "gcs/par2.gc"];;

Testing.test [ (0, "gcs-errors/Po1E.gc");
               (0, "gcs-errors/Po2E.gc");
               (0, "gcs-errors/Po3E.gc");
               (0, "gcs-errors/Po4E.gc");
               (0, "gcs-errors/Po5E.gc");
               (0, "gcs-errors/Po6E.gc");
               (0, "gcs-errors/Po7E.gc");
               (0, "gcs-errors/Po8E.gc");
               (0, "gcs-errors/Po9E.gc");
               (0, "gcs-errors/Po10E.gc");
               (0, "gcs-errors/Po11E.gc")
               ]
|> Testing.hideExnMsg |> Testing.failTC "Pointer"

// Test of programs covered by the fifth task using optimized compilation (Section 8.2):
List.iter execOpt ["gcs/Ex1.gc"; "gcs/Ex2.gc";"gcs/Ex3.gc"; "gcs/Ex4.gc"; "gcs/Ex5.gc"; "gcs/Ex6.gc"; "gcs/Skip.gc"];;

List.iter execOpt ["gcs/Ex7.gc"; "gcs/fact.gc"; "gcs/factRec.gc"; "gcs/factCBV.gc"];;

List.iter execOpt ["gcs/A0.gc"; "gcs/A1.gc"; "gcs/A2.gc"; "gcs/A3.gc"];;

List.iter execOpt ["gcs/MAsg1.gc"; "gcs/MAsg2.gc"; "gcs/MAsg3.gc"; "gcs/MAsg4.gc";
                 "gcs/MAIntTypeIntTypeAssignments.gc";   
                    "gcs/MACorrectTypeToMultiple.gc"; 
                    "gcs/MACorrectSizeEitherSide1.gc"; 
                    "gcs/MACorrectSizeEitherSide2.gc";
                    "gcs/MAAssignmentToTwoInts#1.gc"; 
                    "gcs/MAAssignmentToTwoInts#2.gc" ];;

List.iter execOpt ["gcs/A4.gc"; "gcs/Swap.gc"; "gcs/QuickSortV1.gc"; "gcs/P1IntProcedureType.gc" ];;

List.iter execOpt ["gcs/Po1.gc"; "gcs/Po2.gc"; "gcs/Po3.gc"; "gcs/Po4.gc"; "gcs/Po5.gc"; "gcs/par1.gc"; "gcs/factImpPTyp.gc"; "gcs/QuickSortV2.gc"; "gcs/par2.gc"];;

// execute n-queens
execOpt "gcs/nqueens.gc";;

let prints = getRunPrints "gcs/nqueens.gc";;

// Code for comparison
let comp1Tree = parseFromFile "gcs/Ex7.gc";;
let comp1C   = CP true comp1Tree;;
let comp1Opt = CPOpt true comp1Tree;; 

// execute array out of bounds example:
execOpt "gcs/AOUB.gc";;