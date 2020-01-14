namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016, 04-01-2018
// This file is obtained by an adaption of the file MicroC/Comp.fs by Peter Sestoft
open System
open Machine

open GuardedCommands.Frontend.AST
module CodeGeneration =


(* A global variable has an absolute address, a local one has an offset: *)
   type Var = 
     | GloVar of int                   (* absolute address in stack           *)
     | LocVar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and 
   keeps track of next available offset for local variables *)

   type varEnv = Map<string, Var*Typ> * int

(* The function environment maps function name to label and parameter decs *)

   type ParamDecs = (Typ * string) list
   type funEnv = Map<string, label * Typ option * ParamDecs>

   // Abnormal stop label
   let Abnormalstop = ref ""

   // Whatever out of bounds should be dynamically checked for arrays
   let checkOUB = ref true

/// CE vEnv fEnv e gives the code for an expression e on the basis of a variable and a function environment
   let rec CE vEnv fEnv = 
       function
       | N n          -> [CSTI n]
       | B b          -> [CSTI (if b then 1 else 0)]
       | Access acc   -> CA vEnv fEnv acc @ [LDI] 

       | Apply("-", [e]) -> CE vEnv fEnv e @  [CSTI 0; SWAP; SUB]

       | Apply("!", [e]) -> CE vEnv fEnv e @ [NOT]

       | Apply("&&",[b1;b2]) -> let labend   = newLabel()
                                let labfalse = newLabel()
                                CE vEnv fEnv b1 @ [IFZERO labfalse] @ CE vEnv fEnv b2
                                @ [GOTO labend; Label labfalse; CSTI 0; Label labend]

       | Apply("||",[b1;b2]) -> let labtrue = newLabel()
                                let labend  = newLabel()
                                CE vEnv fEnv b1 @ [IFNZRO labtrue] @ CE vEnv fEnv b2
                                @ [GOTO labend; Label labtrue; CSTI 1; Label labend]

       | Apply(o,[e1;e2]) when List.exists (fun x -> o=x) ["+"; "*"; "="; "-"; "<>"; "<"; ">"; "<="; ">="]
                             -> let ins = match o with
                                          | "+"  -> [ADD]
                                          | "*"  -> [MUL]
                                          | "-"  -> [SUB]
                                          | "="  -> [EQ] 
                                          | "<>" -> [EQ; NOT]
                                          | "<"  -> [LT]
                                          | ">"  -> [SWAP; LT]
                                          | "<=" -> [CSTI 1; ADD; LT]
                                          | ">=" -> [SWAP; CSTI 1; ADD; LT]
                                          | _    -> failwith "CE: this case is not possible"
                                CE vEnv fEnv e1 @ CE vEnv fEnv e2 @ ins 

       | Apply(f, es) -> let (label, _, _) = Map.find f fEnv
                         List.collect (CE vEnv fEnv) es
                         @ [CALL (List.length es, label)]
       
       | _            -> failwith "CE: not supported yet"
       

/// CA vEnv fEnv acc gives the code for an access acc on the basis of a variable and a function environment
   and CA vEnv fEnv = function | AVar x         -> match Map.find x (fst vEnv) with
                                                   | (GloVar addr,_) -> [CSTI addr]
                                                   | (LocVar addr,_) -> [GETBP; CSTI addr; ADD]
                               | AIndex(acc, e) -> if !checkOUB then
                                                     CA vEnv fEnv acc @ 
                                                     [DUP; DUP; LDI; SUB] @ 
                                                     CE vEnv fEnv e @ [DUP; CSTI 0; LT;  IFNZRO !Abnormalstop]
                                                     @ [SWAP; GETSP; CSTI 1; SUB; LDI; CSTI 1; ADD; LT; IFNZRO !Abnormalstop]
                                                     @ [SWAP; LDI; SWAP; ADD]
                                                   else
                                                    CA vEnv fEnv acc @ [LDI] @ CE vEnv fEnv e @ [ADD]
                               | ADeref e       -> failwith "CA: pointer dereferencing not supported yet"

(* Bind declared variable in env and generate code to allocate it: *)   
   let allocate (kind : int -> Var) (typ, x) (vEnv : varEnv)  =
    let (env, fdepth) = vEnv 
    match typ with
    | ATyp (ATyp _, _) -> 
      raise (Failure "allocate: array of arrays not permitted")
    | ATyp (t, Some i) -> 
      let newEnv = (Map.add x (kind (fdepth+i), typ) env, fdepth+i+1) 
      let code = [INCSP i; GETSP; CSTI (i-1); SUB]
      (newEnv, code)
    | _ -> 
      let newEnv = (Map.add x (kind fdepth, typ) env, fdepth+1)
      let code = [INCSP 1]
      (newEnv, code)

                      
/// CS vEnv fEnv s gives the code for a statement s on the basis of a variable and a function environment                          
   let rec CS vEnv fEnv = function
       | PrintLn e        -> CE vEnv fEnv e @ [PRINTI; INCSP -1] 

       | Ass(accs,es)     -> let len = List.length accs
                             List.collect (CE vEnv fEnv) es
                             @ List.collect (CA vEnv fEnv) accs
                             @ List.collect (fun i -> [GETSP; CSTI (len-i-1); SUB; LDI; GETSP; CSTI (len+len-i); SUB; LDI; STI; INCSP -1]) [0 .. len-1]
                             @ [INCSP (-len*2)]

       | Block([],stms) ->   CSs vEnv fEnv stms
       
       | Block(decs, stms) -> let (vEnv, code) = List.fold (fun (env, c) (VarDec(t,x)) -> let (e, c') = allocate LocVar (t,x) env
                                                                                          (e, c @ c')) (vEnv, []) decs
                              code @ CSs vEnv fEnv stms @ [INCSP -(List.length decs)]

       | Alt (GC gcs) ->  match gcs with
                          | [] -> [GOTO !Abnormalstop]
                          | xs -> let labelend = newLabel()
                                  List.collect (CSGC vEnv fEnv labelend) xs 
                                  @ [GOTO !Abnormalstop] @ [Label labelend]

       | Do (GC gcs) ->   match gcs with 
                          | [] -> []
                          | xs -> let labelstart = newLabel()
                                  [Label labelstart]
                                  @ List.collect (CSGC vEnv fEnv labelstart) xs

       | Return (Some e) -> CE vEnv fEnv e @ [RET (snd vEnv)]

       | Return None     -> [RET (snd vEnv - 1)]

       | Call(f, es) -> let (label, _, _) = Map.find f fEnv
                        List.collect (CE vEnv fEnv) es
                        @ [CALL (List.length es, label); INCSP -1]

       | _                -> failwith "CS: this statement is not supported yet"

   and CSGC vEnv fEnv goto (exp, stms) =
        let nextLabel = newLabel()
        (CE vEnv fEnv exp)
        @ [IFZERO nextLabel]
        @ (List.collect (CS vEnv fEnv) stms)
        @ [GOTO goto; Label nextLabel]
   
   and CSs vEnv fEnv stms = List.collect (CS vEnv fEnv) stms 



(* ------------------------------------------------------------------- *)

(* Build environments for global variables and functions *)

   let makeGlobalEnvs decs = 
       let rec addv decs vEnv fEnv = 
           match decs with 
           | []         -> (vEnv, fEnv, [])
           | dec::decr  -> 
             match dec with
             | VarDec (typ, var) -> let (vEnv1, code1) = allocate GloVar (typ, var) vEnv
                                    let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv
                                    (vEnv2, fEnv2, code1 @ code2)
             | FunDec (tyOpt, f, xs, body) 
                -> addv decr vEnv (Map.add f (newLabel(), tyOpt, xs) fEnv)

       addv decs (Map.empty, 0) Map.empty

/// CP prog gives the code for a program prog
   let CP checkOutOfBounds (P(decs,stms)) = 
       let _ = resetLabels ()
       Abnormalstop := newLabel()
       checkOUB := checkOutOfBounds
       let ((gvM,_) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs
       let compilefun (tyOpt, f, xs, body) =
        let (labf, _, paras) = Map.find f fEnv
        let (envf, fdepthf) = List.fold (fun (env, fdepth) (VarDec(t,x)) -> (Map.add x (LocVar fdepth, t) env, fdepth+1)) (gvM, 0) paras
        let code = CS (envf, fdepthf) fEnv body
        [Label labf] @ code 
        @ match tyOpt with
          | Some _ -> [GOTO !Abnormalstop]
          | None -> [RET (List.length paras-1)]
       let functions = 
        List.choose (function
                     | FunDec (rTy, name, argTy, body) -> Some(compilefun(rTy, name, argTy, body))
                     | VarDec _ -> None)
                     decs
       
       initCode @ CSs gvEnv fEnv stms @ [STOP]
       @ [Label !Abnormalstop] @ List.collect (fun x -> [CSTI (int x); PRINTC]) ['E';'R';'R';'O';'R'] @ [STOP]
       @ List.concat functions



