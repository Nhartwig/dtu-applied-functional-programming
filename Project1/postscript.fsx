//#load "translated.fsx"
//open Translated

[<Literal>]
let Padding = 2
[<Literal>]
let Charactersize = 7

let treeWithCoords tree = 
    let rec coords x y (Node((label,offset), subtree)) =
        let x' = x + offset
        Node((label, (x',y)), List.map (coords x' (y-1.0)) subtree)
    coords 0.0 0.0 tree

let rec scalingTree (xMultiplier) (yMultiplier) (Node((label, (x,y)), subtree))=
    assert (xMultiplier >= 31.0)
    assert (yMultiplier >= 7.0)
    Node((label, (x * xMultiplier,y * yMultiplier)), List.map (scalingTree xMultiplier yMultiplier) subtree)

let rec floatTreeToInt (Node((label ,(x,y)), subtree)) =
    Node((label, ((int x), (int y))), List.map floatTreeToInt subtree)

type PostScript =
    | Line of (int * int) * (int * int)
    | Label of (int * int) * string

let rec treeToPostScript (Node((label, (x,y)), subtree)) =
    let labelStartY = 10
    let labelEndY = labelStartY+Charactersize+2*Padding
    let childrenX = List.map (fun (Node((_, (x,_)), _)) -> x) subtree
    [Line ((x,y), (x,y-labelStartY));
    Label((x,y-labelStartY-Charactersize-Padding), label)]
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
        | Label ((x,y), label) ->   [sprintf "%i %i moveto" x y;
                                    sprintf "(%s) dup stringwidth pop 2 div neg 0 rmoveto show" label]
        | Line ((x1,y1), (x2,y2)) ->[sprintf "%i %i moveto" x1 y1; 
                                    sprintf "%i %i lineto" x2 y2]
    start @ (List.collect toStr list) @ ending

let postscript' listToString tree =
    treeWithCoords tree
    |> (scalingTree 7.0 40.0)
    |> floatTreeToInt
    // |> consider label height
    |> treeToPostScript
    |> toPostScript
    |> listToString

let postscript tree =
    postscript' (String.concat "\n") tree