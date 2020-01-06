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
        | Alt (gc) -> Node("Alt", [gcToTree gc])
        | Do (gc) -> Node("Do", [gcToTree gc])
        | Block (decs, stms) -> Node("Block", (List.map decToTree decs) @ (List.map stmToTree stms))
        | Call (str, exps) -> Node("Call", [Node(str,[])] @ (List.map expToTree exps))
    and gcToTree gc = Node("TODO", [])
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


// Testing part
let program =
    P ([
    VarDec(ITyp, "reallylonglongname")
    ], 
    [
    PrintLn (Access(AVar("x")))
])

printf "%A" program

let rec scaletree n (Node(x', y)) =
    Node((fst x', (snd x') * n), List.map (scaletree n) y)

let tree = (design (astToTree program) |> (scaletree 25.0))

printf "And tree = %A" tree

let postscripttree = postscript tree

open System.IO;

File.WriteAllText("test.ps", postscripttree)