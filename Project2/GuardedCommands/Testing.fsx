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
    type TestValue<'T> = {truthVal:bool; returnedVal:'T} 
    
    let trytest func =
        let trytest' func =
            try
                let res = func()
                {truthVal = true; returnedVal = ""}
                //{truthVal = true; returnedVal = (func arg)}
            with
                | ex -> {truthVal = false; returnedVal = ex.ToString()}
        let result = (trytest' func)
        [result.truthVal.ToString();result.returnedVal]    

    let testparseFromFile gcfile = 
        (trytest (fun _ -> parseFromFile gcfile))

    let testtcP gcfile = 
        (trytest (fun _ -> tcP (parseFromFile gcfile)))

    let testCP gcfile =
        (trytest (fun _ -> CP true (parseFromFile gcfile)))

    let test' (gcFile:string) (testid:int) = 
        let n = (List.ofArray(gcFile.Split '/')).[1]
        match testid with
        | 1 -> (n)::testparseFromFile gcFile
        | 2 -> (n)::testtcP gcFile
        | 3 -> (n)::testCP gcFile
        | 0 -> (n)::(testparseFromFile gcFile)@(testtcP gcFile)@(testCP gcFile)
        | _ -> printfn "Invalid test id entered"; (n)::[("Invalid test id entered")]
    
    
    let test zTests = 
        let rec testList ls =
            match ls with 
            | [] -> []
            | (i,f)::ls -> (test' f i) :: (testList ls)
        testList zTests;    

    let hideExnMsg tr = 
        let mapping (list: 'T list) = 
            let indices = [0;1;3;5]
            let res = [for i in indices do yield list.[i]]
            res
        List.map (mapping) (tr) 
                    
    let failTC err tr =
        if List.forall (fun x -> (List.item 1 x = "True") && (List.item 2 x = "False")) tr then () else failwith (sprintf "Typechecker passed in test that should fail: %s" err)




    



