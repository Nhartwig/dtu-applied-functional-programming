//#load "translated.fsx"
//open Translated
#load "Timing.fsx"
open Timing
open System

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


let timeTree' listToString tree = 
    let xMultiplier = ((float Charactersize) * 5.0)
    let yMultiplier = 40.0

    assert (xMultiplier >= 31.0)
    assert (yMultiplier >= float Charactersize)

    let t1 = timeOperation (fun() -> genericTreeToString tree |> design)
    let t1_time = t1.millisecondsTaken
    let t1_val = t1.returnedValue

    let t2 = timeOperation (fun() -> treeWithCoords t1_val)
    let t2_time = t2.millisecondsTaken
    let t2_val = t2.returnedValue

    let t3 = timeOperation (fun() -> (scalingTree xMultiplier yMultiplier) t2_val)
    let t3_time = t3.millisecondsTaken
    let t3_val = t3.returnedValue

    let t4 = timeOperation (fun() -> (splitIntoMultilineLabels xMultiplier) t3_val |> (makeSpaceForMultiLineLabels yMultiplier) )
    let t4_time = t4.millisecondsTaken
    let t4_val = t4.returnedValue

    let t5 = timeOperation (fun() -> floatTreeToInt t4_val |> treeToPostScript )
    let t5_time = t5.millisecondsTaken
    let t5_val = t5.returnedValue

    let t6 = timeOperation (fun() -> toPostScript t5_val)
    let t6_time = t6.millisecondsTaken
    let t6_val = t6.returnedValue


    let t7 = timeOperation (fun() -> listToString t6_val)
    let t7_time = t7.millisecondsTaken
    let t7_val = t7.returnedValue

    let result =String.Format("T1: {0} 
                            \n T2: {1}
                            \n T3: {2}
                            \n T4: {3}
                            \n T5: {4} 
                            \n T6: {5}
                            \n T7: {6}", 
                            t1_time, 
                            t2_time, 
                            t3_time, 
                            t4_time, 
                            t5_time, 
                            t6_time, 
                            t7_time)
    printfn "%s" result
    t7.returnedValue

let postscriptTimed tree =
    timeTree' (String.concat "\n") tree

let postscript tree =
    postscript' (String.concat "\n") tree