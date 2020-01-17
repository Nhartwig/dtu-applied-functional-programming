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

(* 2 Representing trees *)
type Tree<'a> = Node of 'a * ('a Tree list)

let movetree (Node((label, x), subtrees), x') = 
    Node((label, x+x'), subtrees)

(* 3 representing extents *)
type Extent = (float * float) list

let moveextent (e : Extent, x) = List.map (fun (p,q) -> (p+x, q+x) ) e

let rec merge (ps : Extent) (qs : Extent) =
    match (ps, qs) with
    | (_, []) -> ps
    | ([], _) -> qs
    | ((p,_)::ps', (_,q)::qs') -> (p,q) :: merge ps' qs'

let rec mergelist (es : Extent list) = 
    match es with
    | [] -> []
    | e::es' -> merge e (mergelist es')

(* 4 Fitting extents *)
let rmax (p, q) = if p > q then p else q
let rec fit (ps :Extent) (qs : Extent) = 
    match (ps, qs) with
    | ((_,p)::ps', (q,_)::qs') -> rmax(fit ps' qs', p - q + 1.0)
    | (_,_) -> 0.0

let fitlist1 es =
    let rec fitlist1' acc es =
        match es with
        | [] -> []
        | e::es' -> 
            let x = fit acc e
            x :: fitlist1' (merge acc (moveextent(e, x) )) es'
    fitlist1' [] es

let flipextent (e : Extent) : Extent = List.map (fun (p,q) -> (-q,-p)) e
let fitlistr es = 
    List.rev es
    |> List.map flipextent
    |> fitlist1
    |> List.map (~-)
    |> List.rev

let mean (x,y) = (x+y)/2.0
let fitlist (es: Extent list) = List.map mean (List.zip (fitlist1 es) (fitlistr es))

(* 5 Designing the tree *)

let design tree =
    let rec design' (Node(label, subtrees)) =
        let trees, extents = List.unzip (List.map design' subtrees)
        let positions = fitlist extents
        let ptrees = List.map movetree (List.zip trees positions)
        let pextents = List.map moveextent (List.zip extents positions)
        let resultextent = (0.0, 0.0) :: mergelist pextents
        let resulttree = Node((label, 0.0), ptrees)
        (resulttree, resultextent)
    fst (design' tree)

let astToTree (program: Program) =
    let rec expToTree exp =
        match exp with
        | N(x) -> Node("N", [ Node(sprintf "%i" x, []) ])
        | B(b) -> Node("B", [ Node(sprintf "%b" b, []) ])
        | Access(access) -> Node("Access", [ accessToTree access ])
        | Addr(access) -> Node("Addr", [ accessToTree access ])
        | Apply(str, exps) -> Node("Apply", [ Node(str, []) ] @ List.map expToTree exps)

    and accessToTree access =
        match access with
        | AVar(str) -> Node("Avar", [ Node(sprintf "%s" str, []) ])
        | AIndex(access, exp) -> Node("AIndex", [ accessToTree (access) ] @ [ expToTree exp ])
        | ADeref(exp) -> Node("ADeref", [ expToTree exp ])

    let rec stmToTree stm =
        match stm with
        | PrintLn(exp) -> Node("PrintLn", [ expToTree exp ])
        | Ass(access, exp) -> Node("Ass", List.map (fun (acc, e) -> Node(":=", [accessToTree acc] @ [expToTree e])) (List.zip access exp))
        | Return(exp) ->
            match exp with
            | Some(exp') -> Node("Return", [ expToTree exp' ])
            | None -> Node("Return", [ Node("None", []) ])
        | Alt(gc) -> Node("Alt", gcToTree gc)
        | Do(gc) -> Node("Do", gcToTree gc)
        | Block(decs, stms) -> Node("Block", (List.map decToTree decs) @ (List.map stmToTree stms))
        | Call(str, exps) -> Node("Call", [ Node(str, []) ] @ (List.map expToTree exps))

    and gcToTree gc =
        match gc with
        | GC(gcs) -> List.map (fun (exp, stms) -> Node("GC", [ expToTree exp ] @ (List.map stmToTree stms))) gcs

    and decToTree dec =
        match dec with
        | VarDec(typ, str) -> Node("VarDec", [ typToTree typ ] @ [ Node(str, []) ])
        | FunDec(typ, str, decs, stm) ->
            let typNode =
                match typ with
                | Some(typ') -> [ typToTree typ' ]
                | None -> []
            Node("FunDec", typNode @ [ Node(str, []) ] @ (List.map decToTree decs) @ [ stmToTree stm ])

    and typToTree typ =
        match typ with
        | ITyp -> Node("ITyp", [])
        | BTyp -> Node("BTyp", [])
        | ATyp(typ, x) ->
            match x with
            | Some(x') -> Node("ATyp", [ typToTree typ ] @ [ Node(sprintf "%i" x', []) ])
            | None -> Node("ATyp", [ typToTree typ ])
        | PTyp(typ) -> Node("PTyp", [ typToTree typ ])
        | FTyp(typs, typ) ->
            match typ with
            | Some(typ') -> Node("FTyp", (List.map typToTree typs) @ [ typToTree typ' ])
            | None -> Node("FTyp", List.map typToTree typs)

    match program with
    | P(decs, stms) -> Node("Program", (List.map decToTree decs) @ (List.map stmToTree stms))

[<Literal>]
let Padding = 2
[<Literal>]
let Charactersize = 7
[<Literal>]
let LabelStartY = 10

let treeWithCoords tree = 
    let rec coords x y (Node((label,offset), subtree)) =
        let x' = x + offset
        Node((label, (x',y)), List.map (coords x' (y-1.0)) subtree)
    coords 0.0 0.0 tree

let rec scalingTree (xMultiplier) (yMultiplier) (Node((label, (x,y)), subtree))=
    Node((label, (x * xMultiplier,y * yMultiplier)), List.map (scalingTree xMultiplier yMultiplier) subtree)

let rec floatTreeToInt (Node((label ,(x,y)), subtree)) =
    Node((label, ((int x), (int y))), List.map floatTreeToInt subtree)

let count x = Seq.filter ((=) x) >> Seq.length

//possible TODO extend so it takes into account already existing new lines
let rec splitIntoMultilineLabels (xMultiplier :float) (Node((label, x), subtree)) = 
    let insertNewLines maxCharsPerLine label =
        let rec insertNewLines' pos (label:string) =
            match pos with
            | [] -> label
            | x::xs -> insertNewLines' xs (label.Insert(x, "\n")) 
        insertNewLines' ([maxCharsPerLine .. maxCharsPerLine .. (String.length label - 1)] |> List.rev) label
    Node((insertNewLines (int (xMultiplier / (float Charactersize)) + 1) label, x), List.map (splitIntoMultilineLabels xMultiplier) subtree)

let makeSpaceForMultiLineLabels (yMultiplier: float) tree =
    let merge list1 list2 = 
        let rec zip list1 list2 =
            match (list1,list2) with
            | ([], y::ys) -> (0,y) :: (zip [] ys)
            | (x::xs, []) -> (x,0) :: (zip xs [])
            | (x::xs, y::ys) -> (x,y) :: (zip xs ys)
            | _ -> []
        (zip list1 list2) |> List.map (fun (x,y) -> if x >= y then x else y)

    let rec newLinesPerDepth (Node((label, _), subtree)) =
        (count '\n' label + 1) :: ( (List.map (newLinesPerDepth) subtree) |> List.fold (merge) [] )

    let rec yNeededPerDepth (offset:float) newlines =
        match newlines with
        | [] -> []
        | x::xs ->  let spaceNeeded = yMultiplier - float (x * Charactersize + 2 * Padding + 2 * LabelStartY) 
                    let spaceNeeded = if spaceNeeded < 0.0 then spaceNeeded else 0.0
                    let newOffset = spaceNeeded + offset 
                    newOffset :: (yNeededPerDepth newOffset xs)

    let rec addYToTree ys (Node((label, (x,y)), subtree)) =
        match ys with
        | [] -> failwith "Adding the tree failed because of wrong dimension of y needed"
        | y':: ys' -> Node((label, (x, y + float y')), List.map (addYToTree ys') subtree)

    let spaceNeeded = newLinesPerDepth tree
                        |> (yNeededPerDepth 0.0 )

    addYToTree (0.0 :: spaceNeeded) tree

type PostScript =
    | Line of (int * int) * (int * int)
    | Label of (int * int) * string

let rec treeToPostScript (Node((label, (x,y)), subtree)) =
    let amountOfNewLines = count '\n' label
    let labelEndY = LabelStartY + (Charactersize * (amountOfNewLines+1)) + 2*Padding
    let childrenX = List.map (fun (Node((_, (x,_)), _)) -> x) subtree
    [Line ((x,y), (x,y-LabelStartY));
    Label((x,y-LabelStartY-Charactersize-Padding), label)]
    @   match childrenX with
        | [] -> []
        | _ ->  let childLineBeginY = (List.map (fun (Node((_, (_,y)), _)) -> y) subtree) |> List.max
                [Line ((x,y-labelEndY), (x,childLineBeginY));
                Line ((List.min childrenX, childLineBeginY), (List.max childrenX, childLineBeginY))]
    @ List.collect (treeToPostScript) subtree

let toPostScript list =
    let start = ["%!";
                "<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice";
                "1 1 scale";
                "700 999 translate";
                "newpath";
                "/Times-Roman findfont 10 scalefont setfont"]
    let ending= ["stroke";"showpage"]
    let toStr ps =
        match ps with
        | Label ((x,y), label) ->   label.Split [|'\n'|]
                                    |> List.ofArray
                                    |> List.mapi (fun i label' ->
                                        [sprintf "%i %i moveto" x (y-i*Charactersize);
                                        sprintf "(%s) dup stringwidth pop 2 div neg 0 rmoveto show" label']
                                    )
                                    |> List.concat
        | Line ((x1,y1), (x2,y2)) ->[sprintf "%i %i moveto" x1 y1; 
                                    sprintf "%i %i lineto" x2 y2]
    start @ (List.collect toStr list) @ ending

let rec genericTreeToString (Node(x, subtree)) =
    Node(sprintf "%O" x, List.map genericTreeToString subtree)

let postscript' listToString tree =
    let xMultiplier = ((float Charactersize) * 5.0)
    let yMultiplier = 40.0

    assert (xMultiplier >= 31.0)
    assert (yMultiplier >= float Charactersize)

    genericTreeToString tree // 1
    |> design   //  1
    |> treeWithCoords   // 2 
    |> (scalingTree xMultiplier yMultiplier) // 3
    |> (splitIntoMultilineLabels xMultiplier) // 4
    |> (makeSpaceForMultiLineLabels yMultiplier) // 4
    |> floatTreeToInt // 5
    |> treeToPostScript // 5
    |> toPostScript  // 6
    |> listToString  // 7

let postscript tree =
    postscript' (String.concat "\n") tree

let generatePostscript file = 
    parseFromFile file
    |> astToTree
    |> postscript

open System.IO

let ps = generatePostscript "gcs/Ex7.gc"

File.WriteAllText("homemade.ps", ps)