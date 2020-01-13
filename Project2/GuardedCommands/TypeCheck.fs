namespace GuardedCommands.Frontend
// Michael R. Hansen 06-01-2016 , 04-01-2018

open System
open Machine
open GuardedCommands.Frontend.AST

module TypeCheck = 

   let ftypcomp t1 t2 =
      let basic t =
         match t with
         | ATyp(atyp,_) -> atyp
         | x -> x
      basic t1 = basic t2
/// tcE gtenv ltenv e gives the type for expression e on the basis of type environments gtenv and ltenv
/// for global and local variables 
   let rec tcE gtenv ltenv = function                            
         | N _              -> ITyp   
         | B _              -> BTyp   
         | Access acc       -> tcA gtenv ltenv acc     
                   
         | Apply(f,[e]) when List.exists (fun x ->  x=f) ["-"; "!"]  
                            -> tcMonadic gtenv ltenv f e        

         | Apply(f,[e1;e2]) when List.exists (fun x ->  x=f) ["+";"*"; "-"; "="; "<>"; "<"; ">"; "<="; ">="; "&&"; "||"]        
                            -> tcDyadic gtenv ltenv f e1 e2   

         | Apply(f, es)     -> tcNaryFunction gtenv ltenv f es

         | _                -> failwith "tcE: not supported yet"

   and tcMonadic gtenv ltenv f e = match (f, tcE gtenv ltenv e) with
                                   | ("-", ITyp) -> ITyp
                                   | ("!", BTyp) -> BTyp
                                   | _           -> failwith "illegal/illtyped monadic expression" 
   
   and tcDyadic gtenv ltenv f e1 e2 = match (f, tcE gtenv ltenv e1, tcE gtenv ltenv e2) with
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["+";"*";"-"]  -> ITyp
                                      | (o, ITyp, ITyp) when List.exists (fun x ->  x=o) ["="; "<>"; "<"; ">"; "<="; ">="] -> BTyp
                                      | (o, BTyp, BTyp) when List.exists (fun x ->  x=o) ["&&"; "||"; "="; "<>"]     -> BTyp 
                                      | _                      -> failwith("illegal/illtyped dyadic expression: " + f)

   and tcNaryFunction gtenv ltenv f es = tcFuncProc gtenv ltenv true f es
 
   and tcNaryProcedure gtenv ltenv f es = tcFuncProc gtenv ltenv false f es |> ignore

   and tcFuncProc gtenv ltenv isFunc f es= match Map.tryFind f gtenv with
                                           | None     -> failwith (sprintf "Illegal call to unknown procedure/function %s" f)
                                           | Some(f') -> match f' with 
                                                         | FTyp(es', x) -> if List.length es = List.length es' then () else failwith "Illtyped function/procedure call with too few arguments"
                                                                           List.map (tcE gtenv ltenv) es
                                                                           |> List.iter2 (fun e1 e2 -> if ftypcomp e1 e2 then () else failwith "Illtyped function/procedure argument of wrong type") es'
                                                                           match x with
                                                                           | Some(t) -> if not isFunc then failwith "Illegal call of function as procedure" else t
                                                                           | None -> if isFunc then failwith "Illegal call of procedure as function" else BTyp
                                                         | _ -> failwith (sprintf "Illegal call of %s as procedure/function" f)
/// tcA gtenv ltenv e gives the type for access acc on the basis of type environments gtenv and ltenv
/// for global and local variables 
   and tcA gtenv ltenv = 
         function 
         | AVar x         -> match Map.tryFind x ltenv with
                             | None   -> match Map.tryFind x gtenv with
                                         | None   -> failwith ("no declaration for : " + x)
                                         | Some t -> t
                             | Some t -> t            
         | AIndex(acc, e) -> match tcA gtenv ltenv acc with
                             | ATyp (atyp,x) -> if (tcE gtenv ltenv e) = ITyp then atyp else failwith "illtyped array index"
                             | _             -> failwith "Illegal indexing of non array"
                             
         | ADeref e       -> failwith "tcA: pointer dereferencing not supported yes"
 

/// tcS gtenv ltenv retOpt s checks the well-typeness of a statement s on the basis of type environments gtenv and ltenv
/// for global and local variables and the possible type of return expressions 
   and tcS gtenv ltenv = function                           
                         | PrintLn e -> ignore(tcE gtenv ltenv e)
                         | Ass(acc,e) -> if tcA gtenv ltenv acc = tcE gtenv ltenv e 
                                         then ()
                                         else failwith "illtyped assignment"                                

                         | Block([],stms) -> List.iter (tcS gtenv ltenv) stms
                         | Block(decs, stms) -> let ltenv = tcLDecs false ltenv decs
                                                List.iter (tcS gtenv ltenv) stms
                         | Alt (GC(gcs)) -> List.iter (tcGC gtenv ltenv) gcs
                         | Do (GC(gcs))  -> List.iter (tcGC gtenv ltenv) gcs
                         | Return x         -> let e = match x with
                                                       | Some(x') -> tcE gtenv ltenv x'
                                                       | None -> (FTyp([],None))
                                               match Map.tryFind "function" ltenv with
                                               | Some(t) -> if t = e then () else failwith "illegal return type"
                                               | None    -> failwith "illegal call of return outside function"
                         | Call (f, es)   -> tcNaryProcedure gtenv ltenv f es
                         | _              -> failwith "tcS: this statement is not supported yet"

   and tcGC gtenv ltenv (exp, stms) = match tcE gtenv ltenv exp with
                                      | BTyp -> List.iter (tcS gtenv ltenv) stms
                                      | _    -> failwith "illegal GC expression, it has to be a boolean"

   and addToEnv isFuncDec env s = 
      function
      | ATyp(_, None) when not isFuncDec -> failwith "Illegal declaration of array without size"
      | ATyp(_, (Some _)) when isFuncDec -> failwith "ill-typed function argument, array size not allowed"
      | t -> Map.add s t env

   and tcLDec isFuncDec ltenv  = 
      function
      | VarDec(t,s) -> addToEnv isFuncDec ltenv s t 
      | _           -> failwith "ill-typed, sub functions is not allowed"

   and tcLDecs isFuncDec ltenv decs = List.fold (tcLDec isFuncDec) ltenv decs

   and unpackFunDecs decs = List.map (function
                                      | (VarDec(t,_)) -> t
                                      | _ -> failwith "Illegal function declaration inside function declaration") decs

   and tcGDec gtenv = function  
                      | VarDec(t,s)                   -> addToEnv false gtenv s t
                      | FunDec(x, f, decs, stm) -> let t = match x with
                                                           | Some(x') -> x'
                                                           | None     -> (FTyp([],None))
                                                   let ltenv = Map.add "function" t Map.empty
                                                   let ltenv = tcLDecs true ltenv decs
                                                   let gtenv = Map.add f (FTyp(unpackFunDecs decs, Some(t))) gtenv
                                                   tcS gtenv ltenv stm
                                                   gtenv

   and tcGDecs gtenv decs = List.fold (tcGDec) gtenv decs 


/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                            List.iter (tcS gtenv Map.empty) stms

  
