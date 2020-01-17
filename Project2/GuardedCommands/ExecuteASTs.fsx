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

// Calculating factorial using AST
let factorial =
    P
        ([ VarDec(ITyp, "res")
           FunDec
               (Some(ITyp), "fact", [ VarDec(ITyp, "n") ],
                Block
                    ([ VarDec(ITyp, "x")
                       VarDec(ITyp, "y") ],
                     [ Ass([AVar("x")], [Access(AVar("n"))])
                       Ass([AVar("y")], [N(1)])
                       Do
                           (GC
                               ([ (Apply
                                       ("!",
                                        [ Apply
                                            ("=",
                                             [ Access(AVar("x"))
                                               N(0) ]) ]),
                                   [ Ass
                                       ([AVar("y")],
                                        [Apply
                                            ("*",
                                             [ Access(AVar("x"))
                                               Access(AVar("y")) ])])
                                     Ass
                                         ([AVar("x")],
                                          [Apply
                                              ("-",
                                               [ Access(AVar("x"))
                                                 N(1) ])]) ]) ]))
                       Return(Some(Access(AVar("y")))) ])) ],
         [ Ass([AVar("res")], [Apply("fact", [ N(4) ])])
           PrintLn(Access(AVar("res"))) ])

// Generating fibonazzi numbers using AST
let fibonacciNumbers =
    P
        ([ VarDec(ITyp, "res")
           FunDec
               (Some(ITyp), "fibonacci", [ VarDec(ITyp, "n") ],
                Block
                    ([ VarDec(ITyp, "x")
                       VarDec(ITyp, "idx")
                       VarDec(ITyp, "f0")
                       VarDec(ITyp, "f1") ],
                     [ Ass([AVar("x")], [N(0)])
                       Ass([AVar("idx")], [N(0)])
                       Ass([AVar("f0")], [N(0)])
                       Ass([AVar("f1")], [N(1)])
                       Do
                           (GC
                               ([ (Apply
                                       ("!",
                                        [ Apply
                                            ("=",
                                             [ Access(AVar("idx"))
                                               Access(AVar("n")) ]) ]),
                                   [ Ass
                                       ([AVar("x")],
                                        [Apply
                                            ("+",
                                             [ Access(AVar("f0"))
                                               Access(AVar("f1")) ])])
                                     Ass
                                         ([AVar("idx")],
                                          [Apply
                                              ("+",
                                               [ Access(AVar("idx"))
                                                 N(1) ])])
                                     Ass([AVar("f0")], [Access(AVar("f1"))])
                                     Ass([AVar("f1")], [Access(AVar("x"))]) ]) ]))
                       Return(Some(Access(AVar("x")))) ])) ],
         [ Ass([AVar("res")], [Apply("fibonacci", [ N(4) ])])
           PrintLn(Access(AVar("res"))) ]);;

go factorial;;

go fibonacciNumbers;;