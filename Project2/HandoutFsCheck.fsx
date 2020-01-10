// Michael R. Hansen   07-01-2020
// install FsCheck on your labtop and adapt the following line so that you refer to the dll-file FsCheck.dll 
#I @"C:\Users\mire\.nuget\packages\fscheck\2.14.0\lib\net452"
#r @"FsCheck.dll"


//open Microsoft.FSharp.Core.Operators.Checked
open FsCheck


let rec sumA xs acc = match xs with
                      | [] -> acc
                      | x::xs -> sumA xs (x+acc);;   

let rec sumC xs k = match xs with
                    | [] -> k 0 
                    | x::rst -> sumC rst (fun v -> k(x+v));;


// Testing an implementation sumA wrt a reference model List.sum: for all xs: List.sum xs = sumA xs 0

let sumRefProp xs = List.sum xs = sumA xs 0;;

let _ = Check.Quick sumRefProp;;

// A verbose form of the above test is acheived as follows:


let _ = Check.Verbose sumRefProp;;


// Testing correctness properties of tail-recursive functions
let simpleSumProp xs = sumA xs 0 = sumC xs id;;

let sumProp xs n = sumA xs n = sumC xs (fun r -> r+n)

let _ = Check.Quick simpleSumProp;;
let _ = Check.Quick sumProp;;


// Testing functions that should respect the invariant of a datastructure
// Example: Binary search trees 
type Tree = | Lf
            | Br of Tree*int*Tree;;

let rec insert i = 
   function
   | Lf                ->  Br(Lf,i,Lf)
   | Br(t1,j,t2) as tr -> 
         match compare i j with
         | 0           -> tr
         | n when n<0  -> Br(insert i t1 , j, t2)
         | _           -> Br(t1,j, insert i t2);;


let invLeft v  = function 
                 | Lf           -> true
                 | Br(t1,v',t2) -> v'<v 

let invRight v  = function 
                 | Lf           -> true
                 | Br(t1,v',t2) -> v<v';;

let rec invariant = function 
                    | Lf -> true
                    | Br(t1,v,t2) -> invLeft v t1 && invRight v t2 && invariant t1 && invariant t2;;


// the insert must respect the invariant: for all n and t: invariant t implies invariant(insert n t)
let insertInvProp n t = not (invariant t) || invariant(insert n t);; 

let _ = Check.Quick insertInvProp;;


// Many ramdomlyly generated trees do not respect the invariant. More than 100 tests can be achieved by:
let _ = Check.One ({ Config.Quick with MaxTest = 400; }, insertInvProp)


let rec contains  i = 
   function
   | Lf                  -> false
   | Br(_,j,_)  when i=j -> true
   | Br(t1,j,_) when i<j -> contains i t1
   | Br(_,j,t2)          -> contains i t2;;


// Test of a fundamental relationship between insert and contains:
let insertProp i t = not (invariant t) || contains i (insert i t);;
Check.Quick insertProp

// random Tree values
let treeGenerator = Arb.generate<Tree>;; 

Gen.sample 10 50 treeGenerator;;

