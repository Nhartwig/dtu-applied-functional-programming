#load "translated.fsx"
open Translated
#load "AST.fsx"
open AST
#load "postscript.fsx"
open Postscript

let astToTree (program : Program ) =
    let rec expToTree exp =
        match exp with
        | N(x) -> Node("N", [Node(sprintf "%i" x, [])])
        | B(b) -> Node("B", [Node(sprintf "%b" b, [])])
        | Access(access) -> Node("Access", [accessToTree access])
        | Addr(access) -> Node("Addr", [accessToTree access])
        | Apply(str, exps) -> Node("Apply", [Node(str, [])] @ List.map expToTree exps)
    and accessToTree access =
        match access with
        | AVar(str) -> Node("Avar", [Node(sprintf "%s" str,[])])
        | AIndex (access,exp) -> Node("AIndex", [accessToTree(access)]@[expToTree exp])
        | ADeref (exp) -> Node("ADeref", [expToTree exp])

    let rec stmToTree stm =
        match stm with
        | PrintLn (exp) -> Node("PrintLn", [expToTree exp])
        | Ass (access, exp) -> Node("Ass", [accessToTree access] @ [expToTree exp])
        | Return (exp) ->   match exp with 
                            | Some(exp') -> Node("Return", [expToTree exp'])
                            | None -> Node("Return", [Node("None", [])])
        | Alt (gc) -> Node("Alt", gcToTree gc)
        | Do (gc) -> Node("Do", gcToTree gc)
        | Block (decs, stms) -> Node("Block", (List.map decToTree decs) @ (List.map stmToTree stms))
        | Call (str, exps) -> Node("Call", [Node(str,[])] @ (List.map expToTree exps))
    and gcToTree gc = 
        match gc with
        | GC(gcs) -> List.map (fun (exp, stms) -> Node("GC", [expToTree exp] @(List.map stmToTree stms))) gcs
    and decToTree dec =
        match dec with
        | VarDec (typ, str) -> Node("VarDec", [typToTree typ] @ [Node(str, [])])
        | FunDec (typ, str, decs, stm) ->
            let typNode = match typ with
                            | Some(typ') -> [typToTree typ']
                            | None -> []
            Node("FunDec", typNode @ [Node(str, [])] @ (List.map decToTree decs) @ [stmToTree stm])
    and typToTree typ =
        match typ with
        | ITyp -> Node("ITyp", [])
        | BTyp -> Node("BTyp", [])
        | ATyp (typ, x) ->  match x with
                            | Some(x') -> Node("ATyp", [typToTree typ] @ [Node(sprintf "%i" x', [])])
                            | None -> Node("ATyp", [typToTree typ])
        | PTyp (typ) -> Node("PTyp", [typToTree typ])
        | FTyp (typs, typ) ->   match typ with
                                | Some(typ') -> Node("FTyp", (List.map typToTree typs) @ [typToTree typ'])
                                | None -> Node("FTyp", List.map typToTree typs)
    match program with
    | P (decs, stms) -> Node("Program", (List.map decToTree decs) @ (List.map stmToTree stms))


// Calculating factorial using AST
let factorial =
    P ([
            VarDec(ITyp, "res");
            FunDec(Some(ITyp), "fact", [VarDec(ITyp, "n")], Block([
                VarDec(ITyp, "x");
                VarDec(ITyp, "y")
                    ],[
                        Ass(AVar("x"), Access(AVar("n")));
                        Ass(AVar("y"), N(1));
                        Do(GC([(Apply("!", [Apply("=", [Access(AVar("x")); N(0)])]), [
                            Ass(AVar("x"), Apply("*", [Access(AVar("x")); Access(AVar("y"))]));
                            Ass(AVar("x"), Apply("-", [Access(AVar("x")); N(1)]))
                        ])]))
                        Return(Some(Access(AVar("y"))))
            ]))
        ], 
        [
            Ass(AVar("res"),Apply("fact",[N(4)]));
            PrintLn (Access(AVar("res")))
])

// Prints all prime numbers to n using Sieve of Eratosthenes
let sieveOfEratosthenes = P([
    FunDec(None, "sieveOfEratosthenes", [VarDec(ITyp, "n")], Block([
        VarDec(ATyp(BTyp, None), "prime");
        VarDec(ITyp, "p");
        VarDec(ITyp, "p_tmp");
        VarDec(ITyp, "i")
            ], [
                Ass(AVar("p"), N(2));
                Do(GC([(Apply("<=", [Apply("*", [Access(AVar("p")); Access(AVar("p"))]); Access(AVar("n"))])), [
                       Do(GC([(Apply("=", [B(true); Access(AIndex(AVar("prime"), Access(AVar("p"))))]), [
                           Ass(AVar("p_tmp"), Apply("*", [Access(AVar("p")); Access(AVar("p"))]));
                           Do(GC([(Apply("<=", [Access(AVar("p_tmp")); Access(AVar("n"))]), [
                               Ass(AIndex(AVar("prime"), Access(AVar("p_tmp"))), B(false));
                               Ass(AVar("p_tmp"), Apply("+", [Access(AVar("p_tmp")); Access(AVar("p"))]))
                           ])]))
                       ])]))
                       Ass(AVar("p"), Apply("+", [Access(AVar("p")); N(1)]))
                ]]))
                Ass(AVar("i"), N(2));
                Do(GC([(Apply("<=", [Access(AVar("i")); Access(AVar("n"))]), [
                    Do(GC([Apply("=", [B(true); Access(AIndex(AVar("prime"), Access(AVar("i"))))]), [
                        PrintLn(Access(AVar("i")))
                    ]]))
                ])]))
    ]))
], [])

// Generating fibonazzi numbers using AST
let fibonacciNumbers = P([
                            VarDec(ITyp,"res");
                            FunDec(Some(ITyp), "fibonacci",[VarDec(ITyp, "n")], Block([
                                VarDec(ITyp, "x");
                                VarDec(ITyp, "idx");
                                VarDec(ITyp,"f0");
                                VarDec(ITyp,"f1")
                                    ],[
                                        Ass(AVar("x"), N(0));
                                        Ass(AVar("idx"), N(0));
                                        Ass(AVar("f0"), N(0));
                                        Ass(AVar("f1"), N(1));
                                        Do(GC([(Apply("!", [Apply("=", [Access(AVar("idx")); Access(AVar("n"))])]), [
                                            Ass(AVar("x"), Apply("+", [Access(AVar("f0")); Access(AVar("f1"))]));
                                            Ass(AVar("idx"), Apply("+", [Access(AVar("idx")); N(1)]));
                                            Ass(AVar("f0"), Access(AVar("f1")));
                                            Ass(AVar("f1"),Access(AVar("x")))
                                        ])]))
                                        Return(Some(Access(AVar("x"))))
                            ]))                                   
                        ],
                        [
                            Ass(AVar("res"),Apply("fibonacci", [N(4)]));
                            PrintLn (Access(AVar("res")))
])

let factTree = astToTree factorial
let sieveTree = astToTree sieveOfEratosthenes
let fibonacciTree = astToTree fibonacciNumbers

let factPostScript = postscript factTree
let sievePostScript = postscript sieveTree
let fibonacciPostScript = postscript fibonacciTree

open System.IO;

File.WriteAllText("customFactorial.ps", factPostScript)
File.WriteAllText("customSieveOfEratosthenes.ps", sievePostScript)
File.WriteAllText("customFibonacciNumbers.ps", fibonacciPostScript)