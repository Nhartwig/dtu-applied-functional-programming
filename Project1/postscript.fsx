#load "translated.fsx"
open Translated
// function for generating postscript
let postscript tree =
    let start = "%!
    <</PageSize[1400 1000]/ImagingBBox null>> setpagedevice
    1 1 scale
    700 999 translate
    newpath
    /Times-Roman findfont 10 scalefont setfont\n"
    let ending = "stroke\nshowpage"

    let positions x (subtree: Tree<string * float> list) = List.map (fun (Node(x', _)) -> x + (int (snd x'))) subtree

    let drawLines pos y =
        List.map (fun x ->
            (sprintf "%i %i moveto\n" x (y))
            + (sprintf "%i %i lineto\n" x (y - 10))) pos
        |> List.fold (+) ""

    let rec postscript' (x: int, y: int) (Node((label, X'), subtree): Tree<string * float>) =
        match subtree with
        | [] ->
            sprintf "%i %i moveto\n" ((int X') + x) (y - 10)
            + sprintf "(%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" label
        | _ ->
            let X = int X'
            let pos = positions (X + x) subtree
            sprintf "%i %i moveto\n" (X + x) (y - 10)
            + sprintf "(%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" label
            + sprintf "%i %i moveto\n" (X + x) (y - 13) + sprintf "%i %i lineto\n" (X + x) (y - 40)
            + sprintf "%i %i moveto\n" (List.min pos) (y - 40) + sprintf "%i %i lineto\n" (List.max pos) (y - 40)
            + (drawLines pos (y - 40)) +
            //"stroke\n" +
            ((List.map (postscript' (X + x, y - 50)) subtree) |> List.fold (+) "")

    let middle = postscript' (0, 0) tree
    start + middle + ending

let postscriptConcat tree =
    let start = "%!
    <</PageSize[1400 1000]/ImagingBBox null>> setpagedevice
    1 1 scale
    700 999 translate
    newpath
    /Times-Roman findfont 10 scalefont setfont\n"
    let ending = "\nstroke\nshowpage"

    let positions x (subtree: Tree<string * float> list) = List.map (fun (Node(x', _)) -> x + (int (snd x'))) subtree

    let drawLines pos y =
        List.collect (fun x ->
            [sprintf "%i %i moveto" x (y); sprintf "%i %i lineto" x (y - 10)]) pos

    let rec postscript' (x: int, y: int) (Node((label, X'), subtree): Tree<string * float>) =
        match subtree with
        | [] ->
            String.concat "\n" [sprintf "%i %i moveto" ((int X') + x) (y - 10);
                sprintf "(%s) dup stringwidth pop 2 div neg 0 rmoveto show" label]
        | _ ->
            let X = int X'
            let pos = positions (X + x) subtree
            let strings = 
                List.concat [ [sprintf "%i %i moveto" (X + x) (y - 10);
                    sprintf "(%s) dup stringwidth pop 2 div neg 0 rmoveto show" label;
                    sprintf "%i %i moveto" (X + x) (y - 13); 
                    sprintf "%i %i lineto" (X + x) (y - 40);
                    sprintf "%i %i moveto" (List.min pos) (y - 40); 
                    sprintf "%i %i lineto" (List.max pos) (y - 40)];
                    (drawLines pos (y - 40));
                    //"stroke\n" +
                    ((List.map (postscript' (X + x, y - 50)) subtree) ) ]
            String.concat "\n" strings

    let middle = postscript' (0, 0) tree
    start + middle + ending