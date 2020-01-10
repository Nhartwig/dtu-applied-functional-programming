#read "PersonalSettings.fsx"
#load "PersonalSettings.fsx"

#load "AST.fs"
#load "Parser.fs"
#load "Lexer.fs"
#load "TypeCheck.fs"
#load "CodeGen.fs"
#load "CodeGenOpt.fs"
#load "Util.fs"


open GuardedCommands.Util
open GuardedCommands.Frontend.TypeCheck
open GuardedCommands.Frontend.AST
open GuardedCommands.Backend.CodeGeneration

open ParserUtil
open CompilerUtil

open Machine
open VirtualMachine



System.IO.Directory.SetCurrentDirectory __SOURCE_DIRECTORY__;;


module Testing = 

    // want to catch three types of errors: fail on typecheck, fail on parser, fail on compilation
    // e.g on file: "gcs-errors/Ex0.gc"
    
    let testparseFromFile gcfile n = 
        match gcfile with
        | f -> try 
                   match (parseFromFile f) with
                   | _ -> "Parser good"
               with
                    | ex -> printfn "Exception occurred during %s Parsing: %s" n (ex.ToString()); "Parser error"
    let testtcP gcfile n = 
        match gcfile with
        | f -> try
                    if ((testparseFromFile f n) = "Parser good") then 
                        match (tcP (parseFromFile f)) with
                        | _ -> "TypeCheck good" 
                    else "TypeCheck didn't run due to Parse Error"
               with 
                    | ex -> printfn "Exception occurred during %s TypeCheck: %s" n (ex.ToString()); "TypeCheck Error"

    let testCP gcfile n = 
        match gcfile with
        | f -> try 
                    if ((testparseFromFile f n) = "Parser good") then 
                        match (CP (parseFromFile f)) with
                        | _ ->  "CodeGen good"
                    else "CodeGen didn't run due to Parse Error"
               with
                    | ex -> printfn "Exception occurred during %s CodeGeneration: %s" n (ex.ToString());  "CodeGen Error"

    let test' (gcFile:string) (testid:int) = 
        let n = (List.ofArray(gcFile.Split '/')).[1]
        match testid with
        | 1 -> [n] @ [testparseFromFile gcFile n]
        | 2 -> [n] @ [testtcP gcFile n]
        | 3 -> [n] @ [testCP gcFile n]
        | 0 -> [n] @ [(testparseFromFile gcFile n) ; (testtcP gcFile n) ; (testCP gcFile n)]
        | _ -> printfn "Invalid test id entered"; [n] @ ["Invalid test id entered"]

    let rec zip = function 
        | [],_ -> []
        | _,[] -> []
        | (x::xs),(y::ys) -> (x,y)::zip(xs,ys)
    
    let test gcFiles testNums = 
        let zTests = zip (gcFiles,testNums);
        let rec testList ls =
            match ls with 
            | [] -> []
            | (i,f)::ls -> [test' f i] @ (testList ls)
        testList zTests;






    




