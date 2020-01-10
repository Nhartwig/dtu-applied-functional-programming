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

   and tcNaryFunction gtenv ltenv f es = match Map.tryFind f gtenv with
                                         | None     -> failwith "Illegal call to undefined function/procedure"
                                         | Some(f') -> match f' with
                                                       | FTyp(es', Some(t)) -> if List.length es = List.length es' then () else failwith "illtyped function call with too few arguments"
                                                                               List.map (tcE gtenv ltenv) es
                                                                               |> List.iter2 (fun e1 e2 -> if ftypcomp e1 e2 then () else failwith "illtyped function argument of wrong type") es'
                                                                               t
                                                       | _                  -> failwith "Illegal call of procedure as a function"
 
   and tcNaryProcedure gtenv ltenv f es = failwith "type check: procedures not supported yet"
      

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
                         | Block(decs, stms) -> let ltenv = tcLDecs ltenv decs
                                                List.iter (tcS gtenv ltenv) stms
                         | Alt (GC(gcs)) -> List.iter (tcGC gtenv ltenv) gcs
                         | Do (GC(gcs))  -> List.iter (tcGC gtenv ltenv) gcs
                         | Return (Some(e)) -> match Map.tryFind "function" ltenv with
                                               | Some(t) -> if t = (tcE gtenv ltenv e) then () else failwith "illegal return type"
                                               | None -> failwith "illegal call of return outside function"
                         | _              -> failwith "tcS: this statement is not supported yet"

   and tcGC gtenv ltenv (exp, stms) = match tcE gtenv ltenv exp with
                                      | BTyp -> List.iter (tcS gtenv ltenv) stms
                                      | _    -> failwith "illegal GC expression, it has to be a boolean"

   and addToEnv env s = function
                        | ATyp(_, None) -> failwith "Illegal declaration of array without size"
                        | t -> Map.add s t env

   and tcLDec ltenv = function
                      | VarDec(t,s) -> addToEnv ltenv s t 
                      | _           -> failwith "ill-typed, sub functions is not allowed"

   and tcLDecs ltenv = function
                       | dec::decs -> tcLDecs (tcLDec ltenv dec) decs
                       | _         -> ltenv

   and tcGDec gtenv = function  
                      | VarDec(t,s)                   -> addToEnv gtenv s t
                      | FunDec(Some(t), f, decs, stm) -> let ltenv = Map.add "function" t Map.empty
                                                         let ltenv = tcLDecs ltenv decs
                                                         let gtenv = Map.add f (FTyp(List.map (fun (VarDec(t,_)) -> t) decs, Some(t))) gtenv
                                                         tcS gtenv ltenv stm
                                                         gtenv    
                      | FunDec(None   , f, decs, stm) -> failwith "type check: procedure declarations not yet supported"

   and tcGDecs gtenv = function
                       | dec::decs -> tcGDecs (tcGDec gtenv dec) decs
                       | _         -> gtenv


/// tcP prog checks the well-typeness of a program prog
   and tcP(P(decs, stms)) = let gtenv = tcGDecs Map.empty decs
                            List.iter (tcS gtenv Map.empty) stms

  
