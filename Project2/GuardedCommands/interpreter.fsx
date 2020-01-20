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
    let rec allocate (Typ, x) (env0, nextloc) sto0 : locEnv*store = 
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
    let rec getVarDecs' decList =
        let rec getVarDecs (dec:Dec) paramlist= //: (Typ * string) list = 
            match dec, paramlist with
            | VarDec(typ,str), pList -> (typ,str)::pList
            | FunDec(tyOpt, str, decList, stm), pList -> failwith " no func dec in func dec "
        let res = List.collect (fun dec -> getVarDecs dec []) decList
        res

    let initEnvandStore (decs: Dec list) = //(decs: Dec list) : locEnv * funEnv * store =
        let rec addv decs locEnv funEnv store =
            match decs with 
            | [] -> locEnv, funEnv, store
            | VarDec(typ, x)::decr -> let (locEnv1, sto1) = allocate (typ, x) locEnv store
                                      addv decr locEnv1 funEnv sto1
            | FunDec(_, f, xs, body)::decr -> addv decr locEnv ((f, (xs, body))::funEnv) store         
        let loc, fenv, stor = addv decs ([], 0) [] emptyStore 
        let (funenv:funEnv) = List.map (fun (s,((dList:Dec list),(stm:Stm))) -> (s,((getVarDecs' dList),stm)) ) fenv 
        loc,funenv,stor          
/////////////////////////////////////////////////////////////////////
  
(*Executing Statements*)

/////////////////////////////////////////////////////////////////////

    let rec exec stmt (locEnv:locEnv) (gloEnv:gloEnv) (store:store) : ('a option) *store = 
        match stmt with
        | PrintLn e -> let (i1,store1) = (eval e locEnv gloEnv store)
                       printf "%O\n" i1 
                       (None,store1)
        | Ass (acc, e) -> let rec loop ac ex store = 
                            match (ac,ex) with  
                            | [],[] -> (None,store) 
                            | (ac::accTail,ex::exTail) -> let loc1, store1 = (access ac locEnv gloEnv store) 
                                                          let res, store2 = (eval ex locEnv gloEnv store1)                      
                                                          let store' = (setSto store2 loc1 res) 
                                                          loop accTail exTail store'                
                          loop acc e store 
                              
        | Block([],stmts) -> let rec execBlock stmts store =
                                match stmts with
                                | [] -> (None, store)
                                | stmt::stmts -> let (i, store1) = (exec stmt locEnv gloEnv store)
                                                 if (i<>(None)) then (i,store)
                                                 else execBlock stmts (store1)
                             execBlock stmts store

        | Block(decs,stmts) ->  let rec execBlock' decs stmts store locEnv=
                                  match decs,stmts with 
                                  | [],[] -> (None, store)                       
                                  | VarDec(typ, x)::decList, stmt::stmtList -> let (locEnv1, store1) = (allocate (typ, x) locEnv store) 
                                                                               let (i,store2) = exec stmt locEnv1 gloEnv store1 
                                                                               
                                                                               if (i<>(None)) then (i,store2) 
                                                                               else (execBlock' decList stmtList (store2) locEnv1)
                                                                               // check if non neg value returned
                                  | _, stmt::stmtList -> let (i,store1) = exec stmt locEnv gloEnv store
                                                         if (i<>(None)) then (i, store1)
                                                         else execBlock' [] stmtList (store1) locEnv
                                execBlock' decs stmts store locEnv 

        | Alt(GC (gc)) -> List.fold (fun _ gc -> (interpretGC gc "ALT" locEnv gloEnv store)) (None, emptyStore) gc

        // take the first statement true, then execute. 
        // if none are true , then abort

        | Do(GC(gc)) -> match gc with
                        | [] -> (None, store)
                        | gcList -> List.fold (fun _ gc -> (interpretGC gc "DO" locEnv gloEnv store)) (None, emptyStore) gc

        // execute first true statement, then start all over, until condition/expression is false

        | Return (Some e) -> let (i,store1) = eval e locEnv gloEnv store 
                             (Some i, store1)

        | Return None -> (None, store)

        | Call(f, es) -> let (i, store1) = callfun f es locEnv gloEnv store
                         (None, store1)                         

                       
    and interpretGC gc gcArg locEnv gloEnv store = 
        match gc with
        | (exp, stms) when (gcArg = "ALT") ->   let (i,store1) = eval exp locEnv gloEnv (store)
                                                if i=1 then let (i,store2) = List.fold (fun _ stm -> (exec stm locEnv gloEnv store1)) (None, emptyStore) stms                                          
                                                            (i, store2)
                                                else (None,store1)

        | (exp, stms) when (gcArg = "DO") ->  let store1 = List.fold (fun _ stm -> (exec stm locEnv gloEnv store)) (None, emptyStore) stms 
                                              let (i,store2) = eval exp locEnv gloEnv (snd store1)
                                              (Some i, store2)                                  

        // rewrite for DO and ALT statements                                    


    // Evaluate Expression: expr -> locEnv -> gloEnv -> store -> int*store 

    and eval e locEnv gloEnv store : int * store = 
        match e with     
        | N n -> (n, store)    
        | B b -> if b then (1, store) else (0, store)                
        | Access acc -> let (loc, store1) = access acc locEnv gloEnv store
                        ((getSto store1 loc), store1)

        | Apply("-",[e]) -> let (i,store1) = eval e locEnv gloEnv store 
                            (-i, store1)
        | Apply("!",[e]) -> let (i,store1) = eval e locEnv gloEnv store 
                            if i=0 then (1,store1) else (0, store1)
        | Apply("&&",[b1;b2]) -> let b1R, b2R, store1 = (checkBool b1 b2 locEnv gloEnv store)
                                 if ((b1R =1) && (b1R= 1)) then (1, store1) else (0, store1) 

        | Apply("||", [b1;b2]) -> let b1R, b2R, store1 = (checkBool b1 b2 locEnv gloEnv store)
                                  if ((b1R = 1)||(b2R=1)) then (1, store) else (0, store)

        | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["+"; "*"; "="; "-"; "<>"; "<"; ">"; "<="; ">="]
                             -> let (i1, store1) = (eval e1 locEnv gloEnv store)        
                                let (i2, store2) = (eval e2 locEnv gloEnv store1)                          
                                match o with
                                  | "+"  -> (((+) i1 i2), store2)
                                  | "*"  -> (((*) i1 i2), store2)
                                  | "-"  -> (((-) i1 i2), store2)
                                  | "="  -> if ((=) i1 i2) then (1, store2) else (0, store2)
                                  | "<>" -> if ((<>) i1 i2) then (1, store2) else (0, store2)
                                  | "<"  -> if ((<) i1 i2) then (1, store2) else (0, store2)
                                  | ">"  -> if ((>) i1 i2) then (1, store2) else (0, store2)
                                  | "<=" -> if ((<=) i1 i2) then (1, store2) else (0, store2)
                                  | ">=" -> if ((>=) i1 i2) then (1, store2) else (0, store2)
                                  | _ -> failwith " this case is not possible "

        | Apply(f, es) -> let (i, store1) = callfun f es locEnv gloEnv store
                          (i,store1)    
       
        | _            -> failwith "CE: not supported yet"                               
                                                                  


    and checkBool b1 b2 locEnv gloEnv store =
        let b1Result, store1 = (eval b1 locEnv gloEnv store)
        let b2Result, store2 = (eval b2 locEnv gloEnv store1)
        (b1Result, b2Result, store2)

    // Access: access -> locEnv -> gloEnv -> store -> addr*store

    and access (acc: Access) (locEnv:locEnv) (gloEnv:gloEnv) (store:store) = 
        match acc with
        | AVar x -> (lookup (fst locEnv) x, store)
        | AIndex (acc,e) ->  // implement out of bounds checking?
                             let (a, store1) = (access acc locEnv gloEnv store)
                             let aVal = getSto store1 a
                             let (i, store2) = eval e locEnv gloEnv store1
                             (aVal+i, store2)
        | ADeref e -> (eval e locEnv gloEnv store)

    and evals es locEnv gloEnv store : int list * store = 
        match es with 
        | []     -> ([], store)
        | e1::er ->
          let (v1, store1) = eval e1 locEnv gloEnv store
          let (vr, storer) = evals er locEnv gloEnv store1 
          (v1::vr, storer)     

    and callfun f es locEnv gloEnv store : int * store = 
        let (_, nextloc) = locEnv
        let (varEnv, funEnv) = gloEnv
        let (paramdecs, fBody) = lookup funEnv f
        let (vs, store1) = evals es locEnv gloEnv store
        let (fBodyEnv, store2) = 
            bindVars (List.map snd paramdecs) vs (varEnv, nextloc) store
        let (i,store3) = exec fBody fBodyEnv gloEnv store2 
        match i with
        | None -> (-1, store3)
        | Some i -> (i, store3)
       

// test whether you need array out-of-bounds checking

(*
    Interpret a complete GC program by initializing 
    store and global environments, then invoking on Begin function
*)
    let run (P(decs, stmts)) vs = 
       let (varEnv, nextloc), (fEnv), store0 = initEnvandStore decs
       
      // let (mainParams, mainBody) = lookup fEnv ""
        
       (*let (mainBodyEnv, store1) = 
            bindVars (List.map snd (mainParams)) vs (varEnv, nextloc) store0
       *)
       
      // exec mainBody mainBodyEnv ((varEnv, fEnv)) store1
       let res = List.fold (fun (_,store) stm-> exec stm (varEnv,nextloc) (varEnv,fEnv) store) (None, store0) stmts
       res



        //let (name,fxnEnv)::fxnEnvList = funEnv
       // let fn = (((name,fxnEnv)::fxnEnvList) : ((string * (('a * string) list * 'b)) list))

  // exec stmt (locEnv:locEnv) (gloEnv:gloEnv) (store:store) : store = 
