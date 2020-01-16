#load "AST.fs"
#load "Testing.fsx"

// Nathaniel Hartwig, Magnus Hovman 16-01-2020
// This file is obtained by an adaption of the file MicroC/Interp.fs by Peter Sestoft
//
// It must preceed TypeChecks.fs, CodeGen.fs, CodeGenOpt.fs, Util.fs in Solution Explorer
//

open GuardedCommands.Frontend.AST

module Interpreter = 

    type 'data env = (string * 'data) list

    let rec lookup env x = 
        match env with
        | [] -> failwith (x + " not found ")
        | (y,v)::yr -> if x=y then v else lookup yr x
    
    type locEnv = int env * int

    type paramdecs = (Typ * string) list

    type funEnv = (paramdecs * Stm) env

    type gloEnv = int env * funEnv

    type address = int

    type store = Map<address, int>

    let emptyStore = Map.empty<address, int> 

    let setSto (store: store) addr value = store.Add(addr, value)

    let getSto (store: store) addr = store.Item addr 

    let rec initSto loc n store = 
        if n=0 then store else initSto (loc+1) (n-1) (setSto store loc -999) // initializes n stores incrementally with value -999

    // Environment and store operations

    (* Extend local variable environment so that it maps x to nextloc 
        (next store location) and set store[nextloc] = v .
    *)

    let bindVar x v (env, nextloc) store : locEnv * store = 
        let env1 = (x, nextloc)::env
        ((env1, nextloc + 1), setSto store nextloc v)

    let rec bindVars xs vs locEnv store : locEnv * store =
        match (xs, vs) with
        | ([], [])      -> (locEnv, store)
        | (x1::xr, v1::vr) -> let (locEnv1, sto1) = bindVar x1 v1 locEnv store
                              bindVars xr vr locEnv1 sto1
        | _ -> failwith "parameter/argument mismatch"

    (* Allocate variable (int or pointer array): extend environment 
    so that it maps variable to next available store location, and initialize store location(s).
    *)                           
    let rec allocate (Typ, x) (env0, nextloc) sto0: locEnv*store = 
        let (nextloc1, v, sto1) = 
            match Typ with
            | ATyp (t, Some i) -> (nextloc+i, nextloc, initSto nextloc i sto0)
            | _ -> (nextloc, -1, sto0)
        bindVar x v (env0, nextloc1) sto1

    (* 
    Build global environment of variables and functions. 
    For global variables, store locations are reserved; for global functions, 
    just add to global function environment. 
    *)
    let initEnvandStore (decList: Dec list) : locEnv * funEnv * store =
        let rec addv decs locEnv funEnv store =
            match decs with 
            | [] -> (locEnv, funEnv, store)
            | VarDec(typ, x)::decr -> let (locEnv1, sto1) = allocate (typ, x) locEnv store
                                      addv decr locEnv1 funEnv sto1
            | FunDec(_, f, xs, body)::decr -> addv decr locEnv ((f, (xs, body))::funEnv) store
        addv decList (Map.empty, 0) [] (emptyStore:Map.empty)                               
/////////////////////////////////////////////////////////////////////
  


/////////////////////////////////////////////////////////////////////

    let rec exec stmt locEnv gloEnv store = 
        match stmt with
        | PrintLn(e) ->  printfn (eval e locEnv gloEnv store)    (* Print                          *) 
        | Ass(accList, eList) -> List.map (fun acc,e -> let (loc, store1) = access acc locEnv gloEnv store
                                    let (res, store2) = eval e locEnv gloEnv store1
                                        (res, setSto (store2 loc res))) List.zip(accList, eList)          (* x:=e  or  p^:=e  or  a[e]:=e   *)
        | Return (Some e)                                                           (* Return from function           *)   
        | Alt gc ->                                                                 (* Alternative statement          *) 
        | Do gc ->                                                                  (* Repetition statement           *) 
        | Block([],stmList) ->   List.collect (exec locEnv gloEnv store) stmList    (* Block: grouping and scope      *)
        | Block(decList, stmList) -> List.coll
        | Call(str,eList) -> 


    // Evaluate Expression: expr -> locEnv -> gloEnv -> store -> int*store 

    and eval e locEnv gloEnv store = 
        match e with                         
        | Access acc -> let (loc, store1) = access acc locEnv gloEnv store
                            ((getSto store1 loc), store1) // return val, store1                      (* x    or  ^p    or  a[e]     *)
        | Addr acc ->               (* &x   or  &p^   or  &a[e]    *)
        | Apply(str,eList) -> // apply the function
    // Access: access -> locEnv -> gloEnv -> store -> addr*store




  
