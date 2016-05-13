(*** CS 51 Problem Set 1 ***)

(* Problem 1 - Fill in types:
 *
 * Replace each ??? with the appropriate type of the corresponding expression.
 * Be sure to remove the comments from each subproblem and to type check it
 * before submission. *)

let prob0 : int = 42 ;;

let prob1a : string = let greet y = "Hello " ^ y in greet "World!";;

let prob1b : int option list  = [Some 4; Some 2; None; Some 3];;

let prob1c : ('a option * float option) * bool = ((None, Some 42.0), true);;

let prob0 =  "int";;

let prob1a = "string";;

let prob1b = "int option list";;

let prob1c = "('a option * float option) * bool";;

(*Error: Parentheses are required to group types for tuple lists.*)

let prob1d : (string * int) list = [("CS", 51); ("CS", 50)];;
  
(*Error: Compare takes in two identical types, and float != int.*)

let prob1e : int = 
  let compare (x,y) = x < y in
  if compare (4, 3) then 4 else 2;;

(*Error: Elements of a list must be the same type, but the tuples have
differing 2nd types.*)

let prob1f : (string * int option) list =
  [("January", None); ("February", Some 1); ("March", None); ("April", None);
   ("May", None); ("June", Some 1); ("July", None); ("August", None);
   ("September", Some 3); ("October", Some 1); ("November", Some 2); ("December", Some 3)] ;;


(* Problem 2 - Write the following functions *)

(*>* Problem 2a *>*)
let rec reversed (lst: int list): bool =  
  match lst with
  | []         -> true
  | [_]        -> true
  | h::next::t -> if h < next then false else reversed(next::t);;

(*>* Problem 2b *>*)
let rec merge (list1: int list) (list2: int list): int list =
  match list1, list2 with
  |[],[]          -> []
  |_,[]           -> list1
  |[],_           -> list2
  |h1::t1, h2::t2 -> if h1 <= h2 then h1::merge t1 list2 else h2::merge t2 list1;; 

(*>* Problem 2c *>*)
let rec unzip (pairs: (int * int) list): int list * int list = 
  match pairs with
  |[]            -> ([],[])
  |(a,b)::t  -> let x, y = unzip t in 
  (a::x, b::y);;

(*>* Problem 2d *>*)
let variance (lst: float list): float option = 
  (* square all elements of a float list *)
  let rec square_all (lst0: float list): float list = 
    match lst0 with 
    | []   -> []
    | h::t -> (h*.h) :: square_all t in 
  (* sum all elements of a float list *)
  let rec sum_fl (lst1: float list): float =
    match lst1 with
    | [] -> 0.0
    | h::t -> h +. sum_fl t in
  (* find the length of a float list *)
  let rec length_fl (lst2: float list): float = 
    match lst2 with 
    | [] -> 0.0
    | h::t -> 1.0 +. length_fl t in
  (* find the mean of a float list *)
  let mean_fl (lst3: float list): float =
    sum_fl(lst3)/.length_fl(lst3) in
  (* find the deviation of each float list element *)
  let rec dev (lst4: float list): float list =
    match lst4 with 
    | [] -> []
    | h::t -> h -.mean_fl(lst4):: dev t in
  (* compute variance *)
  match lst with
  | h::t  -> Some (sum_fl(square_all(dev(lst)))/.(length_fl(lst) -. 1.0))
  | _ -> None ;;


(*>* Problem 2e *>*)
let few_divisors (n: int) (m: int): bool = 
  if n = 0 then false
  else 
    (* find the number of divisors then compare to m *)
    let rec find_divisors (x: int) (y: int): int = 
      if x = y then 1
      else if x mod y = 0 then 1 + find_divisors x (y + 1) 
      else find_divisors x (y + 1) in
      if find_divisors n 1 < m then true else false ;;
  

(*>* Problem 2f *>*)
let rec concat_list (sep: string) (lst: string list) : string = 
  match lst with
  | []    -> ""
  | [x] -> x 
  | h::t  -> h ^ sep ^ concat_list sep t ;;


(*>* Problem 2g *>*)

(* reverses a list *)
let reverse (lst0: 'a list): 'b list =
  let rec backwards acc lst = 
    match lst with 
    | []   -> acc
    | h::t -> backwards (h::acc) t in
  backwards [] lst0 ;; 

let to_run_length (characters: char list): (int * char) list =
    (* counts number of instances to create tuple *)
    let rec run_encode counter a = function
      | []                    -> [] 
      | [x]                   -> (counter + 1, x) :: a
      | h :: (next :: _ as t) -> if h = next then run_encode (counter + 1) a t
                                 else run_encode 0 ((counter + 1, h) :: a) t in
    reverse (run_encode 0 [] characters) ;;

(* COULDN'T GET MY CODE BELOW TO COMPILE :( *)
let from_run_length (characters: (int * char) list): char list =
  ['a'; 'b'; 'c'] ;;

(* create a type for the run-length encoded tuples *)
(*
type char =
  | One of char
  | Many of int * char ;;

let from_run_length (characters: (int * char) list): char list =
  (* ensure that the character occurs *) 
  let rec decompose h num t =
    if num = 0 then h else decompose (t::h) (num - 1) t in
  (* split characters by occurrences, then rebuild *)
  let rec run_decode a = function
    | [] -> a
    | One t::last -> run_decode (t::a) last
    | Many (num, t) :: last -> run_decode (decompose a num t) last in
  run_decode [] (reverse characters) ;; *)
