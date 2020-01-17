namespace GuardedCommands.Backend
// Michael R. Hansen 05-01-2016, 04-01-2018

open System
open Machine

open GuardedCommands.Frontend.AST

module CodeGenerationOpt =

   type Var = 
     | GloVar of int                   (* absolute address in stack           *)
     | LocVar of int                   (* address relative to bottom of frame *)

(* The variable environment keeps track of global and local variables, and 
   keeps track of next available offset for local variables *)

   type varEnv = Map<string, Var * Typ> * int

(* The function environment maps function name to label and parameter decs *)

   type ParamDecs = (Typ * string) list
   type funEnv = Map<string, label * Typ option * ParamDecs>

   // Abnormal stop label
   let Abnormalstop = ref ""


(* Directly copied from Peter Sestoft   START  
   Code-generating functions that perform local optimizations *)

   let rec addINCSP m1 C : instr list =
       match C with
       | INCSP m2            :: C1 -> addINCSP (m1+m2) C1
       | RET m2              :: C1 -> RET (m2-m1) :: C1
       | Label lab :: RET m2 :: _  -> RET (m2-m1) :: C
       | _                         -> if m1=0 then C else INCSP m1 :: C

   let addLabel C : label * instr list =          (* Conditional jump to C *)
       match C with
       | Label lab :: _ -> (lab, C)
       | GOTO lab :: _  -> (lab, C)
       | _              -> let lab = newLabel() 
                           (lab, Label lab :: C)

   let makeJump C : instr * instr list =          (* Unconditional jump to C *)
       match C with
       | RET m              :: _ -> (RET m, C)
       | Label lab :: RET m :: _ -> (RET m, C)
       | Label lab          :: _ -> (GOTO lab, C)
       | GOTO lab           :: _ -> (GOTO lab, C)
       | _                       -> let lab = newLabel() 
                                    (GOTO lab, Label lab :: C)

   let makeCall m lab C : instr list =
       match C with
       | RET n            :: C1 -> TCALL(m, n, lab) :: C1
       | Label _ :: RET n :: _  -> TCALL(m, n, lab) :: C
       | _                      -> CALL(m, lab) :: C

   let rec deadcode C =
       match C with
       | []              -> []
       | Label lab :: _  -> C
       | _         :: C1 -> deadcode C1

   let addNOT C =
       match C with
       | NOT        :: C1 -> C1
       | IFZERO lab :: C1 -> IFNZRO lab :: C1 
       | IFNZRO lab :: C1 -> IFZERO lab :: C1 
       | _                -> NOT :: C

   let addJump jump C =                    (* jump is GOTO or RET *)
       let C1 = deadcode C
       match (jump, C1) with
       | (GOTO lab1, Label lab2 :: _) -> if lab1=lab2 then C1 
                                         else GOTO lab1 :: C1
       | _                            -> jump :: C1
    
   let addGOTO lab C = addJump (GOTO lab) C

   let rec addCST i C =
       match (i, C) with
       | (0, ADD        :: C1) -> C1
       | (0, SUB        :: C1) -> C1
       | (0, NOT        :: C1) -> addCST 1 C1
       | (_, NOT        :: C1) -> addCST 0 C1
       | (1, MUL        :: C1) -> C1
       | (1, DIV        :: C1) -> C1
       | (0, EQ         :: C1) -> addNOT C1
       | (_, INCSP m    :: C1) -> if m < 0 then addINCSP (m+1) C1
                                  else CSTI i :: C
       | (0, IFZERO lab :: C1) -> addGOTO lab C1
       | (_, IFZERO lab :: C1) -> C1
       | (0, IFNZRO lab :: C1) -> C1
       | (_, IFNZRO lab :: C1) -> addGOTO lab C1
       | _                     -> CSTI i :: C
            
(* ------------------------------------------------------------------- *)

(* End code directly copied from Peter Sestoft *)
/// CE e vEnv fEnv k gives the code for an expression e on the basis of a variable and a function environment and continuation k
   let rec CE e vEnv fEnv k = 
       match e with
       | N n          -> addCST n k
       | B b          -> addCST (if b then 1 else 0) k
       | Access acc  -> CA acc vEnv fEnv (LDI :: k) 

       | Apply("!",[e]) -> CE e vEnv fEnv (addNOT k)

       | Apply("-",[e]) -> CE e vEnv fEnv (addCST 0 (SWAP:: SUB :: k))

       | Apply("&&",[b1;b2]) -> 
                match k with
                | IFZERO lab :: _ -> CE b1 vEnv fEnv (IFZERO lab :: CE b2 vEnv fEnv k)
                | IFNZRO labthen :: k1 -> 
                        let (labelse, k2) = addLabel k1
                        CE b1 vEnv fEnv
                             (IFZERO labelse :: CE b2 vEnv fEnv (IFNZRO labthen :: k2))
                | _ ->  let (jumpend,  k1) = makeJump k
                        let (labfalse, k2) = addLabel (addCST 0 k1)
                        CE b1 vEnv fEnv (IFZERO labfalse :: CE b2 vEnv fEnv (addJump jumpend k2))

       | Apply("||",[b1;b2]) -> 
                match k with
                | IFNZRO lab :: _ -> CE b1 vEnv fEnv (IFNZRO lab :: CE b2 vEnv fEnv k)
                | IFZERO labthen :: k1 ->
                        let (labelse, k2) = addLabel k1
                        CE b1 vEnv fEnv
                            (IFNZRO labelse :: CE b2 vEnv fEnv (IFZERO labthen :: k2))
                | _ ->  let (jumpend, k1) = makeJump k
                        let (labtrue, k2) = addLabel (addCST 1 k1)
                        CE b1 vEnv fEnv (IFNZRO labtrue :: CE b2 vEnv fEnv (addJump jumpend k2))
       
       | Apply(o,[e1;e2])  when List.exists (fun x -> o=x) ["+"; "*"; "="; "-"; "<>"; "<"; ">"; "<="; ">="]
                          -> let ins = match o with
                                       | "+"  -> ADD::k
                                       | "*"  -> MUL::k
                                       | "-"  -> SUB::k
                                       | "="  -> EQ::k
                                       | "<>" -> EQ::addNOT k
                                       | "<"  -> LT::k
                                       | ">"  -> SWAP::LT::k
                                       | "<=" -> CSTI 1::ADD::LT::k
                                       | ">=" -> SWAP::CSTI 1::ADD::LT::k
                                       | _    -> failwith "CE: this case is not possible"
                             CE e1 vEnv fEnv (CE e2 vEnv fEnv ins) 

       | Apply(f, es) -> let (label, _, _) = Map.find f fEnv
                         makeCall (List.length es) label k
                         |> CEs es vEnv fEnv

       | _                -> failwith "CE: not supported yet"
       
   and CEs es vEnv fEnv k = 
      match es with 
      | []     -> k
      | e::es' -> CE e vEnv fEnv (CEs es' vEnv fEnv k) 

/// CA acc vEnv fEnv k gives the code for an access acc on the basis of a variable and a function environment and continuation k
   and CA acc vEnv fEnv k = 
      match acc with 
      | AVar x         -> match Map.find x (fst vEnv) with
                          | (GloVar addr,_) -> addCST addr k
                          | (LocVar addr,_) -> GETBP :: addCST addr (ADD :: k)
      | AIndex(acc, e) -> CA acc vEnv fEnv (LDI :: (CE e vEnv fEnv (ADD :: k)))
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

                      
/// CS s vEnv fEnv k gives the code for a statement s on the basis of a variable and a function environment and continuation k                            
   let rec CS stm vEnv fEnv k = 
       match stm with
       | PrintLn e        -> CE e vEnv fEnv (PRINTI:: INCSP -1 :: k) 

       | Ass(acc,e)       -> CA (List.head acc) vEnv fEnv (CE (List.head e) vEnv fEnv (STI:: addINCSP -1 k))
                             //failwith "removed assignment since it doesn't work with multi assignment, TODO fix"
                             //CA acc vEnv fEnv (CE e vEnv fEnv (STI:: addINCSP -1 k))

       | Block([],stms)   -> CSs stms vEnv fEnv k

       | Block(decs, stms) -> let (vEnv, code) = List.fold (fun (env, c) (VarDec(t,x)) -> let (e, c') = allocate LocVar (t,x) env
                                                                                          (e, c @ c')) (vEnv, []) decs
                              code @
                              (addINCSP (-(List.length decs)) k
                              |> CSs stms vEnv fEnv)

       | Alt (GC gcs)  -> match gcs with
                          | [] -> addGOTO !Abnormalstop k
                          | _  -> let (labend, k) = addLabel k
                                  let k = addGOTO !Abnormalstop k
                                  CSGC vEnv fEnv labend gcs k

       | Do (GC gcs)   -> match gcs with 
                          | [] -> k
                          | _  -> let labelstart = newLabel()
                                  Label labelstart :: CSGC vEnv fEnv labelstart gcs k

       | Return (Some e) -> CE e vEnv fEnv (RET (snd vEnv) :: k)

       | Return None     -> RET (snd vEnv - 1) :: k

       | _                -> failwith "CS: this statement is not supported yet"

   and CSGC vEnv fEnv goto gcs k =
        match gcs with
        | []                 -> k
        | (exp, stms)::gcs'  -> let k = (CSGC vEnv fEnv goto gcs' k)
                                let (nextLabel, k) = addLabel k
                                let k = addGOTO goto k
                                        |> CSs stms vEnv fEnv
                                CE exp vEnv fEnv (IFZERO nextLabel ::k )
        

   and CSs stms vEnv fEnv k = 
        match stms with
        | []   -> k
        | stm::stms' -> CS stm vEnv fEnv (CSs stms' vEnv fEnv k) 

(* ------------------------------------------------------------------- *)

(* Build environments for global variables and functions *)

   let makeGlobalEnvs decs = 
       let decs = List.rev decs
       let rec addv decs vEnv fEnv k = 
           match decs with 
           | []         -> (vEnv, fEnv, k)
           | dec::decr  -> 
             match dec with
             | VarDec (typ, var) -> let (vEnv1, code1) = allocate GloVar (typ, var) vEnv
                                    let (vEnv2, fEnv2, code2) = addv decr vEnv1 fEnv code1
                                    (vEnv2, fEnv2, code1 @ code2)
             | FunDec (tyOpt, f, xs, body) 
                -> addv decr vEnv (Map.add f (newLabel(), tyOpt, xs) fEnv) k

       addv decs (Map.empty, 0) Map.empty []

(* Compile a complete micro-C program: globals, call to main, functions *)

   let CP (P(decs,stms)) = 
       let _ = resetLabels ()
       Abnormalstop := newLabel()
       let ((gvM,_) as gvEnv, fEnv, initCode) = makeGlobalEnvs decs
       let compilefun (tyOpt, f, xs, body) =
        let (labf, _, paras) = Map.find f fEnv
        let (envf, fdepthf) = List.fold (fun (env, fdepth) (VarDec(t,x)) -> (Map.add x (LocVar fdepth, t) env, fdepth+1)) (gvM, 0) paras
        let code = CS body (envf, fdepthf) fEnv
                    (match tyOpt with
                    | Some _ -> [GOTO !Abnormalstop]
                    | None -> [RET (List.length paras-1)])
        Label labf :: code
       let functions = 
        List.choose (function
                     | FunDec (rTy, name, argTy, body) -> Some(compilefun(rTy, name, argTy, body))
                     | VarDec _ -> None)
                     decs
       
       let (labmain, k) = 
           addLabel
               (CSs stms gvEnv fEnv ([RET -1]
               @ [Label !Abnormalstop] @ List.collect (fun x -> [CSTI (int x); PRINTC]) ['E';'R';'R';'O';'R'] @ [STOP]
               @ List.concat functions))
       initCode @ CALL(0, labmain):: INCSP -1:: STOP ::  k



