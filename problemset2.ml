open Ast ;;
open ExpressionLibrary ;;

(* TIPS FOR PROBLEM 2:
 * 1. Read the writeup.
 * 2. Use the type definitions in the ast.ml as a reference. But don't worry
 *    about expressionLibrary.ml
 *)

(*>* Problem 2.1 *>*)

(* contains_var : tests whether an expression contains a variable "x"
 *     Examples : contains_var (parse "x^4") = true
 *                contains_var (parse "4+3") = false *)
let rec contains_var (e:expression) : bool =
  match e with
  | Num _ -> false
  | Var   -> true
  | Binop (_, e1, e2) -> contains_var e1 || contains_var e2
  | Unop  (_, e1) -> contains_var e1 
;;


(*>* Problem 2.2 *>*)

(* evaluate : evaluates an expression for a particular value of x. Don't
 *            worry about handling 'divide by zero' errors.
 *  Example : evaluate (parse "x^4 + 3") 2.0 = 19.0 *)
let rec evaluate (e:expression) (x:float) : float =
  match e with
  | Binop (b, e1, e2) -> (match b with
                         | Add -> evaluate e1 x +. evaluate e2 x 
                         | Sub -> evaluate e1 x -. evaluate e2 x
                         | Mul -> evaluate e1 x *. evaluate e2 x
                         | Div -> evaluate e1 x /. evaluate e2 x
                         | Pow -> evaluate e1 x ** evaluate e2 x)
  | Unop (u, e1)      -> (match u with 
                         | Sin -> sin (evaluate e1 x)
                         | Cos -> cos (evaluate e1 x)
                         | Ln  -> log (evaluate e1 x)
                         | Neg -> (-1.) *. (evaluate e1 x)) 
  | Num fl            -> fl
  | Var               -> x 
;;


(*>* Problem 2.3 *>*)

(* See writeup for instructions. *)
let rec derivative (e:expression) : expression =
    match e with
    | Num _ -> Num 0.
    | Var -> Num 1.
    | Unop (u,e1) ->
        (match u with
        | Sin -> Binop(Mul,Unop(Cos,e1),derivative e1)
        | Cos -> Binop(Mul,Unop(Neg,Unop(Sin,e1)),derivative e1)
        | Ln  -> Binop(Div, derivative e1, e1)
        | Neg -> Unop(Neg,derivative e1))
    | Binop (b,e1,e2) ->
        match b with
        | Add -> Binop(Add,derivative e1,derivative e2)
        | Sub -> Binop(Sub,derivative e1,derivative e2)
        | Mul -> Binop(Add,Binop(Mul,e1,derivative e2),
                 Binop(Mul,derivative e1,e2))
        | Div -> Binop(Div,Binop(Sub,Binop(Mul,derivative e1,e2),
                 Binop(Mul,derivative e2, e1)),
                 Binop(Pow,e2,Num(2.)))
        | Pow ->
        (* check if exponent contains a variable (x) *)
            if contains_var e1 && contains_var e2
            then Binop(Mul,Binop(Pow,e1,e2), 
                 Binop(Add,Binop(Mul,derivative e2,Unop(Ln,e1)), 
                 Binop(Div,Binop(Mul,derivative e1,e2),e1)))
            else Binop(Mul,Binop(Mul,e2,derivative e1),
                 Binop(Pow,e1,Binop(Sub,e1,Num 1.)))
;;

(* A helpful function for testing. See the writeup. *)
let checkexp strs xval =
    print_string ("Checking expression: " ^ strs ^ "\n");
    let parsed = parse strs in (
        print_string "contains variable : ";
        print_string (string_of_bool (contains_var parsed));
        print_endline " ";
        print_string "Result of evaluation: ";
        print_float (evaluate parsed xval);
        print_endline " ";
        print_string "Result of derivative: ";
        print_endline " ";
        print_string (to_string (derivative parsed));
        print_endline " "
    )
;;


(*>* Problem 2.4 *>*)

(* See writeup for instructions. *)
let rec find_zero (e: expression) (g: float) (epsilon: float) (lim: int):
float option =
  if lim = 0 then None
  else
    let precise (x: float): bool = not(abs_float(evaluate e x) > epsilon) in
    let prime (f: float): float = evaluate (derivative e) f in
    let update (y: float): float = y -. ((evaluate e y) /. (prime y)) in
    if (precise g) then (Some g) 
    else 
        let g' = update g in
        (* decrement limit on each rec *)
        let new_lim = lim - 1 in 
        find_zero e g' epsilon new_lim ;;

(*>* Problem 2.5 *>*)

(* Extra Credit:
 * Just leave it unimplemented if you don't want to do it.
 * See writeup for instructions. *)
let find_zero_exact (e: expression) : expression option =
    failwith "Not implemented"
;; 


(*>* Problem 2.6 *>*)

let minutes_spent_on_part_2 () : int = 300 ;;

(*                                                                           *)

(* CS51 Problem Set 2 *)

(****************************************************)
(******       1.1: Sparking your INTerest      ******)
(****************************************************)

(* Solve each problem in this part using List.map, List.fold_right, or
 * List.filter.
 *
 * A solution, even a working one, that does not use one of these
 * higher-order functions, will receive little or no credit.
 * However, if you can express your solution to
 * one particular part in terms of another function from
 * another part, you may do so.
 *
 * You MAY NOT change the definition of these
 * functions to make them recursive. 
 *)


(*>* Problem 1.1.a *>*)

(*  negate_all : Flips the sign of each element in a list *)

let negate_all (nums:int list) : int list = 
  List.map (fun x -> x * (-1)) nums 
;;


(*>* Problem 1.1.b *>*)

(*  sum : Returns the sum of the elements in the list. *)

let sum (nums:int list) : int = 
  List.fold_right (+) nums 0
;;


(*>* Problem 1.1.c *>*)

(*  sum_rows : Takes a list of "rows", each an int list.
 *             Returns a one-dimensional list of ints, each int equal to the
 *             sum of the corresponding row in the input.
 *   Example : sum_rows [[1;2]; [3;4]] = [3; 7] *)

let sum_rows (rows:int list list) : int list = List.map sum rows ;;


(*>* Problem 1.1.d *>*)

(*  filter_odd : Retains only the odd numbers from the given list.
 *     Example : filter_odd [1;4;5;-3] = [1;5;-3]. *)

let filter_odd (nums:int list) : int list = 
  List.filter (fun x -> not(x mod 2 = 0)) nums ;;


(*>* Problem 1.1.e *>*)

(*  num_occurs : Returns the number of times a given number appears in a list.
 *     Example : num_occurs 4 [1;3;4;5;4] = 2 *)

let num_occurs (n:int) (nums:int list) : int =
    List.length (List.filter (fun x -> x = n) nums)
;;


(*>* Problem 1.1.f *>*)

(*  super_sum : Sums all of the numbers in a list of int lists
 *    Example : super_sum [[1;2;3];[];[5]] = 11 *)

let super_sum (nlists:int list list) : int =
    sum (sum_rows nlists)
;;

(*>* Problem 1.1.g *>*)

(*  filter_range : Returns a list of numbers in the input list within a
 *                 given range (inclusive), in the same order they appeared
 *                 in the input list.
 *       Example : filter_range [1;3;4;5;2] (1,3) = [1;3;2] *)

let filter_range (nums:int list) (range:int * int) : int list =
    match range with
    | (a, b) -> List.filter (fun x -> x <= b && x >= a) nums    
;;



(****************************************************)
(**********       1.2 Fun with Types       **********)
(****************************************************)


(*>* Problem 1.2.a *>*)

(*  floats_of_ints : Converts an int list into a list of floats *)

let floats_of_ints (nums:int list) : float list =
    List.map float_of_int nums 
;;


(*>* Problem 1.2.b *>*)

(*   log10s : Applies the log10 function to all members of a list of floats.
 *            The mathematical function log10 is not defined for
 *            numbers n <= 0, so undefined results should be None.
 *  Example : log10s [1.0; 10.0; -10.0] = [Some 0.; Some 1.; None] *)

let log10s (lst: float list) : float option list =
    List.map (fun sign -> if sign <= 0.0 then None 
                          else Some (log10 sign)) lst  
;;


(*>* Problem 1.2.c *>*)

(*  deoptionalize : Extracts values from a list of options.
 *        Example : deoptionalize [Some 3; None; Some 5; Some 10] = [3;5;10] *)

let deoptionalize (lst:'a option list) : 'a list =
    List.map (fun y -> match y with 
                       | Some v -> v
                       | None -> failwith "No value to extract." )
    (List.filter (fun x -> not(x = None)) lst)
;;


(*>* Problem 1.2.d *>*)

(*  some_sum : Sums all of the numbers in a list of int options;
 *             ignores None values *)

let some_sum (nums:int option list) : int = sum (deoptionalize nums) ;;


(*>* Problem 1.2.e *>*)

(*  mult_odds : Product of all of the odd members of a list.
 *    Example : mult_odds [1;3;0;2;-5] = -15 *)

let mult_odds (nums:int list) : int =
    let prod x = List.fold_right ( * ) x 1 in
    if (List.length (filter_odd nums)) = 0 then 0 
    else prod (filter_odd nums) 
;;


(*>* Problem 1.2.f *>*)

(*  concat : Concatenates a list of lists. See the Ocaml library ref *)
let concat (lists:'a list list) : 'a list = List.concat lists ;;


(*>* Problem 1.2.g *>*)

(* the student's name and year *)
type name = string
type year = int
type student = name * year

(*  filter_by_year : returns the names of the students in a given year
 *         Example : let students = [("Joe",2010);("Bob",2010);("Tom",2013)];;
 *                   filter_by_year students 2010 => ["Joe";"Bob"] *)

let filter_by_year (slist:student list) (yr:year) : name list =
    (* extract years from tuples in list *)
    let extract_year (x: student): year = 
      match x with 
      | (_, b) -> b in
    (* filter out tuples with desired year *)
    let filt (lst: student list): student list = 
      List.filter (fun y -> (extract_year y) = yr) lst in
    (* extract names from filtered tuples *)
    List.map (fun f -> 
        match f with
        | (a, _) -> a) (filt slist)
;;
