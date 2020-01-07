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
    
let postscriptStringBuilder tree =
    let sb = new System.Text.StringBuilder()
    
    let positions x (subtree: Tree<string * float> list) = List.map (fun (Node(x', _)) -> x + (int (snd x'))) subtree
    
    let drawLines pos y =
        List.map (fun x ->
            sb.Append(sprintf "%i %i moveto\n%i %i lineto\n" x y x (y - 10))) pos

    let rec postscript' (x: int, y: int) (Node((label, X'), subtree): Tree<string * float>) =
        match subtree with
        | [] ->
            sb.Append(sprintf "%i %i moveto\n(%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" ((int X') + x) (y - 10) label)
        | _ ->
            let X = int X'
            let pos = positions (X + x) subtree
            sb.Append(sprintf "%i %i moveto\n(%s) dup stringwidth pop 2 div neg 0 rmoveto show\n" (X + x) (y - 10) label) |> ignore
            sb.Append(sprintf "%i %i moveto\n%i %i lineto\n" (X + x) (y - 13) (X + x) (y - 40)) |> ignore
            sb.Append(sprintf "%i %i moveto\n%i %i lineto\n" (List.min pos) (y - 40) (List.max pos) (y - 40)) |> ignore
            drawLines pos (y - 40) |> ignore
            List.map (postscript' (X + x, y - 50)) subtree |> ignore
            sb.Append("")
    
    sb.Append("%!\n<</PageSize[1400 1000]/ImagingBBox null>> setpagedevice\n1 1 scale\n700 999 translate\nnewpath\n/Times-Roman findfont 10 scalefont setfont\n") |> ignore
    
    postscript' (0, 0) tree |> ignore
            
    sb.Append("stroke\nshowpage") |> ignore
    
    sb.ToString()
