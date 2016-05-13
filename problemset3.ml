(**********************************************************************
 * CS51 Problem Set 3
 * Bignums and RSA
 *)

(* ====================================================================
 * Section 1: Bignums
 *)

type bignum = {neg: bool; coeffs: int list} ;;
  
let base = 1000 ;;

(* ....................................................................
 * Basics 
 *)

(* global function used in toInt and times *)
let add_zeros (c: float): int = int_of_float((float_of_int base) ** c) ;;


(* Problem 1 *)
  
let negate (b : bignum) : bignum =
  if b.neg then {neg = false; coeffs = b.coeffs}
  else {neg = true; coeffs = b.coeffs} 
;;
  
(* Problem 2 *)
  
let equal (b1 : bignum) (b2 : bignum) : bool = 
  b1 = b2 
;;

let less (b1 : bignum) (b2 : bignum) : bool =
  if equal b1 b2 then false
  else if b1.neg <> b2.neg then b1.neg
  else
    let rec contrast (x: int list) (y: int list): bool =
    match x, y with
    | [h1], [h2] -> h1 < h2
    | h1::t1, h2::t2 -> if h1 = h2 then contrast t1 t2 
                        else h1 < h2
    | _, _ -> false in
    if b1.neg then
      List.length b1.coeffs > List.length b2.coeffs ||
      List.length b1.coeffs = List.length b2.coeffs && 
        not (contrast b1.coeffs b2.coeffs)
    else
      List.length b1.coeffs < List.length b2.coeffs ||
      List.length b1.coeffs = List.length b2.coeffs && 
        contrast b1.coeffs b2.coeffs
;;

let greater (b1 : bignum) (b2 : bignum) : bool =
  not (equal b1 b2 || less b1 b2)
;;

(* Problem 3 *)
  
let fromInt (n: int) : bignum =
  let rec decompose (x: int): int list = 
    if x <> 0 then (abs(x) mod base :: decompose (x/base))
    else [] in
  {neg = (n < 0); coeffs = List.rev (decompose n)} 
;;

let toInt (b : bignum) : int option =
  if b.coeffs = [] then Some 0
  else 
    let rec compose (x: int list) (c: float): int =
      match List.rev x with
      | [h] -> h * (add_zeros c)
      | h::t  -> h * (add_zeros c) + compose (List.rev t) (c +. 1.)
      | []     -> 0  in
    if (compose b.coeffs 0.) > max_int then None 
    else
      if b.neg then Some (0 - compose b.coeffs 0.) 
      else Some (compose b.coeffs 0.) 
;;

(* ....................................................................
 * Some helpful functions 
 *)

(* Removes zero coefficients from the beginning of the coefficients in
 * a bignum representation *)
let rec stripzeroes (b : int list) : int list =
  match b with
  | 0 :: t -> stripzeroes t
  | _ -> b ;;

(* Removes zero coefficients from the beginning of a bignum
 * representation *)
let clean (b : bignum) : bignum =
  {neg = b.neg; coeffs = stripzeroes b.coeffs} ;;

(* Returns a random bignum from 0 to bound (inclusive).
 * Can use this to help randomly test functions. *)
let randbignum (bound: bignum) =
  let randbase = List.map (fun _ -> Random.int base) in
  let rec randbignum_rec (bound: int list) =
    match bound with
      | [] -> []
      | h::t -> let r = Random.int (h+1) in
          r::((if r = h then randbignum_rec else randbase) t)
  in {neg = false; coeffs = stripzeroes (randbignum_rec bound.coeffs)} ;;
       
(* Splits a string into a list of its characters. *)
let rec explode (s : string) : char list =
  let len = String.length s in
  if len = 0 then []
  else s.[0] :: explode (String.sub s 1 (len - 1)) ;;

(* Condenses a list of characters into a string. *)
let rec implode (cs : char list) : string =
  match cs with
    | [] -> ""
    | c :: t -> String.make 1 c ^ implode t ;;
					  
(* Returns the first n elements of list l (or the whole list if too short) *)
let rec take_first (l : 'a list) (n : int) : 'a list =
  match l with
    | [] -> []
    | h :: t -> if n <= 0 then [] else h :: take_first t (n - 1) ;;

(* Returns a pair
 * (first n elements of lst, rest of elements of lst) *)
let rec split lst n =
  if n = 0 then ([], lst)
  else match lst with
    | [] -> ([], [])
    | h :: t -> let (lst1, lst2) = split t (n - 1) in
                (h :: lst1, lst2) ;;

(* Returns the floor of the base 10 log of an integer *)
let intlog (base : int) : int =
  int_of_float (log10 (float_of_int base)) ;;

(* fromString and toString assume the base is a power of 10 *)
(* Converts a string representing an integer to a bignum. *)
let fromString (s : string) : bignum =
  let rec fromString_rec (cs : char list) : int list =
    if cs = [] then [] else
    let (chars_to_convert, rest) = split cs (intlog base) in
    let string_to_convert = implode (List.rev chars_to_convert) in
    int_of_string string_to_convert :: fromString_rec rest
  in
  match explode s with
    | [] -> fromInt 0
    | h :: t -> if h = '-' || h = '~' then
        {neg = true; coeffs = (List.rev (fromString_rec (List.rev t)))}
      else {neg = false;
            coeffs = (List.rev (fromString_rec (List.rev (h :: t))))}

(* Converts a bignum to its string representation.
 * Returns a string beginning with ~ for negative integers. *)
let toString (b : bignum) : string =
  let rec pad_with_zeroes_left (s : string) (len : int) =
    if String.length s >= len then s else
      "0" ^ pad_with_zeroes_left s (len - 1) in
  let rec stripstrzeroes (s : string) (c : char) =
    if String.length s = 0 then
      "0"
    else if String.get s 0 = '0' then
      stripstrzeroes (String.sub s 1 (String.length s - 1)) c
    else s in
  let rec coeffs_to_string (coeffs : int list) : string =
    match coeffs with
      | [] -> ""
      | h :: t -> pad_with_zeroes_left (string_of_int h) (intlog base)
                  ^ coeffs_to_string t in
  let stripped = stripzeroes b.coeffs in
  if List.length stripped = 0 then "0"
  else let from_coeffs = stripstrzeroes (coeffs_to_string stripped) '0' in
       if b.neg then "~" ^ from_coeffs else from_coeffs ;;

(* ....................................................................
 * Arithmetic functions 
 *)

(* Returns a bignum representing b1 + b2.
 * Assumes that b1 + b2 > 0. *)
let plus_pos (b1 : bignum) (b2 : bignum) : bignum =
  let pair_from_carry (carry : int) =
    if carry = 0 then (false, [])
    else if carry = 1 then (false, [1])
    else (true, [1])
  in
  let rec plus_with_carry (neg1, coeffs1) (neg2, coeffs2) (carry : int)
            : bool * int list =
    match (coeffs1, coeffs2) with
      | ([], []) -> pair_from_carry carry
      | ([], _) -> if carry = 0 then (neg2, coeffs2) else
          plus_with_carry (neg2, coeffs2) (pair_from_carry carry) 0
      | (_, []) -> if carry = 0 then (neg1, coeffs1) else
          plus_with_carry (neg1, coeffs1) (pair_from_carry carry) 0
      | (h1 :: t1, h2 :: t2) ->
          let (sign1, sign2) =
            ((if neg1 then -1 else 1), (if neg2 then -1 else 1)) in
          let result = h1 * sign1 + h2 * sign2 + carry in
          if result < 0 then
            let (negres, coeffsres) =
                  plus_with_carry (neg1, t1) (neg2, t2) (-1)
            in (negres, result + base :: coeffsres)
          else if result >= base then
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 1
            in (negres, result - base :: coeffsres)
          else
            let (negres, coeffsres) = plus_with_carry (neg1, t1) (neg2, t2) 0
            in (negres, result :: coeffsres)
  in
  let (negres, coeffsres) =
        plus_with_carry (b1.neg, List.rev b1.coeffs)
                        (b2.neg, List.rev b2.coeffs)
                        0
  in {neg = negres; coeffs = stripzeroes (List.rev coeffsres)} ;;

(* Problem 4 *)
       
(* Returns a bignum representing b1 + b2.
 * Does not make the assumption from plus_pos.
 * Hint: How can you use plus_pos to implement this?
*)

let plus (b1 : bignum) (b2 : bignum) : bignum =
  if b1.neg = b2.neg && not b1.neg || 
     b1.neg && not b2.neg && not (greater (negate b1) b2) then
     plus_pos b1 b2
  else negate (plus_pos (negate b1) (negate b2))
;;


(* Problem 5 *)
  
(* Returns a bignum representing b1*b2.
 * Things to think about:
 * 1. How did you learn to multiply in grade school?
 * (In case you were never taught to multiply and instead started with
 * set theory in third grade, here's an example):
 *
 *      543
 *     x 42
 *     ____
 *     1086
 *   +21720
 *   =22806
 *
 * 2. Can you use any functions you've already written?
 * 3. How can you break this problem into a few simpler, easier-to-implement
 * problems?
 * 4. If your code is buggy, test helper functions individually instead of
 * the entire set at once.
 * 5. Assuming positivity in some of your helper functions is okay to
 * simplify code, as long as you respect that invariant.
*)

let times (b1 : bignum) (b2 : bignum) : bignum =
  if b1.coeffs = [] || b2.coeffs = [] then fromInt 0
  else if b1.coeffs = [1] then b2
  else if b2.coeffs = [1] then b1
  else 
    (* sum list of products *)
    let sum_big (big: bignum list): bignum =
      List.fold_right plus big (fromInt 0) in
    (* multiply a bignum by an int *)
    let mult (b: bignum) (n: int): bignum =
      if n = 0 || b = fromInt 0 then fromInt 0
      else 
        let rec add_again (x: bignum) (i: int): bignum list =
          if i = n then [x]
          else x :: add_again x (i + 1) in
        sum_big (add_again b 1) in
    (* make a tuple list of a bignum's coeffs and their indices *)
    let index (bn: bignum): (int * int ) list = 
      List.mapi (fun i x -> (i, x)) (List.rev bn.coeffs) in
    (* construct list of products of each coeff and b1 from index tuples *)
    let rec breakdown (lst: (int * int) list): bignum list = 
      match lst with
      | [(x1, y1)] -> [mult (mult b1 y1) 
                    (add_zeros (float_of_int x1))]
      | (x1,y1)::t  -> mult (mult b1 y1) 
                     (add_zeros (float_of_int x1))
                     :: breakdown t 
      | [] -> [] in
    if b1.neg <> b2.neg then negate (sum_big (breakdown (index b2)))
    else sum_big (breakdown (index b2)) 
;;

(* Returns a bignum representing b/n, where n is an integer less than base *)
let divsing (b : int list) (n : int) : int list * int =
  let rec divsing_rec (b : int list) (r : int) : int list * int =
    match b with
      | [] -> [], r
      | h :: t ->
          let dividend = r * base + h in
          let quot = dividend / n in
          let (q, r) = divsing_rec t (dividend-quot * n) in
          (quot :: q, r) in
    match b with
      | [] -> [], 0
      | [a] -> [a / n], a mod n
      | h1 :: h2 :: t -> if h1 < n then divsing_rec (h1 * base + h2 ::t) 0
        else divsing_rec b 0 ;;

(* Returns a pair (floor of b1/b2, b1 mod b2), both bignums *)
let divmod (b1 : bignum) (b2 : bignum): bignum * bignum =
  let rec divmod_rec m n (psum : bignum) : bignum * bignum =
    if less m n then (psum, m) else
      let mc = m.coeffs in
      let nc = n.coeffs in
      match nc with
        | [] -> failwith "Division by zero"
        | ns :: _ -> let (p, _) =
            if ns + 1 = base then
              (take_first mc (List.length mc - List.length nc), 0)
            else
              let den = ns + 1 in
              let num = take_first mc (List.length mc - List.length nc + 1)
              in divsing num den
          in
          let bp = clean {neg = false; coeffs = p} in
          let p2 = clean (if equal bp (fromInt 0) then fromInt 1 else bp) in
            divmod_rec (clean (plus m (negate (times n p2))))
                       (clean n)
                       (clean (plus psum p2))
  in
  divmod_rec (clean b1) (clean b2) (fromInt 0) ;;