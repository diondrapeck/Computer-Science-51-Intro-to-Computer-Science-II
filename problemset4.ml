(**********************************************************************
 * CS51 Problem Set 4
 * Modules, Functors, and Priority Queues
 *)

(* ====================================================================
 * Section 1: Binary trees introduced
 *)

(* NOTE: Please read (and understand) all of the comments in this file! 
 * As well, *commit often*! This problem set can be difficult in that it 
 * may stop compiling at points if you are approaching something
 * in the wrong way. 
 * If that happens, you may want to be able to get it
 * back to a point where it compiled. 
 *
 * COMMIT, COMMIT, COMMIT! *)

(* Some code related to the TreeQueue module in this file are commented out
 * because this file would not compile otherwise.
 *
 * Please uncomment them as you get to them. *)

type order = Equal | Less | Greater ;;

(* ====================================================================
 * Section 2: Binary search trees 
 *)

(* A better signature for a binary tree, avoiding the comparison function
 * found in FIRSTBINTREE in the problem set writeup. *)
module type BINTREE =
sig
  exception EmptyTree
  exception NodeNotFound

  (* The type of an element in the tree *)
  type elt

  (* What this type actually looks like is left up to the
   * particular BINTREE implementation (i.e. the struct) *)
  type tree

  (* Returns an empty tree *)
  val empty : tree

  (* Search a binary tree for the given value. *)
  val search : elt -> tree -> bool

  (* Insert elt into tree *)
  val insert : elt -> tree -> tree

  (* Delete the given value from a binary tree.
   * May raise NodeNotFound exception. *)
  val delete : elt -> tree -> tree

  (* Return the minimum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmin : tree -> elt

  (* Return the maximum value of a binary tree.
   * May raise EmptyTree exception *)
  val getmax : tree -> elt

  (* Return a string of the given tree. *)
  val to_string : tree -> string
 
  (* Run invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(* A signature for a module that defines a type and how to compare
 * values of that type, as well as ways of generating values of that
 * type. *)
module type COMPARABLE =
  sig
    type t
    val compare : t -> t -> order
    val to_string : t -> string

    (* See the tests.ml file for an explanation of
     * what these "generate*" functions do, and why we included them in
     * this signature. *)

    (* Generate a value of type t *)
    val generate: unit -> t
          
    (* Generate a value of type t that is greater than the argument. *)
    val generate_gt: t -> unit -> t
            
    (* Generate a value of type t that is less than the argument. *)
    val generate_lt: t -> unit -> t

    (* Generate a value of type t that is between argument 1 and argument 2.
     * Returns None if there is no value between argument 1 and argument 2. *)
    val generate_between: t -> t -> unit -> t option
  end

(* An example implementation of the COMPARABLE signature. In this
 * example, the value of the integer also gives its priority. *)
module IntCompare : (COMPARABLE with type t = int) =
  struct
    type t = int
         
    let compare x y = if x < y then Less else if x > y then Greater else Equal
                     
    let to_string = string_of_int
          
    let generate () = 0
      
    let generate_gt x () = x + 1
         
    let generate_lt x () = x - 1
         
    let generate_between x y () =
      let (lower, higher) = (min x y, max x y) in
      if higher - lower < 2 then None else Some (higher - 1)
  end
    
(* Another example implementation for int*string pairs. It only uses
 * the int part of the tuple in comparisons. *)
module IntStringCompare : (COMPARABLE with type t = int * string) =
  struct
    type t = int * string
    let compare (p1,_) (p2,_) =
      if p1 < p2 then Less else if p1 > p2 then Greater else Equal
                     
    let to_string (p, s) = "(" ^ string_of_int p ^ "," ^ s ^ ")"
                     
                     
    let () = Random.self_init ()
            
    let generate () = (0, string_of_int (Random.int max_int))
      
    let generate_gt (p,s) () = (p+1, s)
         
    let generate_lt (p,s) () = (p-1, s)
         
    let generate_between (p1,_) (p2,s2) () =
      let (lower, higher) = (min p1 p2, max p1 p2) in
      (* Reuse the string from the second argument in the output value *)
      if higher - lower < 2 then None else Some (higher - 1, s2)
  end
    
(* BinSTree is a *functor*, which takes an argument C, a module
 * that implements the COMPARABLE signature. BinSTree ultimately
 * must return a module which matches the BINTREE signature.
 *
 * Now that we are passing in a COMPARABLE module, which separately
 * defines a type and comparison for that type, we can just implement something
 * matching BINTREE's signature in terms of that type and comparison function,
 * and can wait until later to actually say what that type and comparison
 * function are.
 *
 * Here, you'll fill in the implementation of a binary search tree. Unlike a
 * usual binary search tree, this implementation keeps a list with each node in
 * the tree that contains each instance of the value inserted into the tree. For
 * example, if the integer 3 is inserted into an Int BinSTree 5 times, then
 * there will be a node with [3;3;3;3;3] in the tree, and the node will only be
 * removed after 5 deletions on 3 (assuming no further intermediate insertions
 * of 3).
 *)

module BinSTree(C : COMPARABLE) : (BINTREE with type elt = C.t) =
  struct
    (* Inside of here, you can use C.t to refer to the type defined in
     * the C module (which matches the COMPARABLE signature), and
     * C.compare to access the function which compares elements of type
     * C.t
     *)
    exception EmptyTree
    exception NodeNotFound
    
    (* Grab the type of the tree from the module C that's passed in
     * this is the only place you explicitly need to use C.t; you
     * should use elt everywhere else *)
    type elt = C.t
     
    (* One possible type for a tree *)
    type tree = Leaf | Branch of tree * elt list * tree
                 
    (* Representation of the empty tree *)
    let empty = Leaf
      
    (* **** Problem 1 *)
      
    (* Define a method to insert element x into the tree t.
     * The left subtree of a given node should only have "smaller"
     * elements than that node, while the right subtree should only have
     * "greater". Remember that "equal" elements should all be stored in
     * a list. *The most recently inserted elements should be at the front
     * of the list* (this is important for later).
     *
     * Hint: use C.compare. See delete for inspiration
     *)
    let rec insert (x : elt) (t : tree): tree =
      match t with
      | Leaf -> Branch (Leaf, [x], Leaf)
      | Branch (trL, lst, trR) -> 
        match lst with 
        | []   -> failwith "Invalid tree: empty list as node"
        | h::_ -> 
          match C.compare x h with
          | Equal   -> Branch (trL, x::lst, trR)
          | Less    -> Branch (insert x trL, lst, trR)
          | Greater -> Branch (trL, lst, insert x trR)

    (* Returns true if the element x is in tree t, else false *)
    (* Hint: multiple values might compare Equal to x, but
     * that doesn't necessarily mean that x itself is in the
     * tree.
     *)
    let rec search (x : elt) (t : tree): bool =
      match t with
      | Leaf -> false
      | Branch (trL, lst, trR) -> 
          match lst with
          | [] -> failwith "Invalid tree: empty list as node"
          | h::_ -> 
                  match C.compare x h with
                  | Equal -> List.mem x lst
                  | Less -> search x trL
                  | Greater -> search x trR
                
    (* A useful function for removing the node with the minimum value from
     * a binary tree, returning that node and the new tree.
     *
     * Note that the pull_min function is not defined in the signature BINTREE.
     * When you're working on a structure that implements a signature like
     * BINTREE, you may write "helper" functions for your implementation
     * (such as pull_min) that are not defined in the signature.  
     *
     * Note, however, that if a function foo *is* defined in a signature BAR,
     * and you attempt to make a structure satisfying the signature BAR,
     * then you *must* define the function foo in your structure.
     * Otherwise the compiler will complain that your structure does not,
     * in fact, satisfy the signature BAR (but you claim that it does).
     * So, if it's in the signature, it needs to be in the structure.  But if
     * it's in the structure, it doesn't necessarily need to show up in the
     * signature.
     *)
    let rec pull_min (t : tree) : elt list * tree =
      match t with
      | Leaf -> raise EmptyTree
      | Branch (Leaf, v, r) -> (v, r)
      | Branch (l, v, r) -> let min, t' = pull_min l in (min, Branch (t', v, r))
                
    (* Removes an element from the tree. If multiple elements are in the list,
     * removes the one that was inserted first.  *)
    let rec delete (x : elt) (t : tree) : tree =
      match t with
      | Leaf -> raise NodeNotFound
      | Branch (l, lst, r) ->
    (* Reverse the list so that we pop off the last element in the list *)
    match List.rev lst with
    | [] -> failwith "Invalid tree: empty list as node"
    | hd::tl ->
            match C.compare x hd with
            | Less -> Branch (delete x l, lst, r)
            | Greater -> Branch (l, lst, delete x r)
            | Equal ->
               match tl with
               | _::_ -> Branch (l, List.rev tl, r)
               (* The list in the node is empty, so we have to
    * remove the node from the tree.  *)
               | [] ->
      match l, r with
      | Leaf, _ -> r
      | _, Leaf -> l
      | _ -> let v, r' = pull_min r in Branch (l,v,r')
                
    (* Simply returns the minimum value of the tree t. If there are multiple
     * minimum values, it should return the one that was inserted first (note
     * that, even though the list might look like [3;3;3;3;3], you should
     * return the *last* 3 in the list. This is because we might pass in
     * a module to this functor that defines a type and comparison function
     * where each element in the list *is* distinct, but are Equal
     * from the perspective of the comparison function (like IntStringCompare).
     *
     * The exception "EmptyTree", defined within this module, might come in
     * handy. *)
                
    let rec getmin (t : tree) : elt = 
      match t with
      | Leaf -> raise EmptyTree
      | Branch (lt, lst, _) -> 
        match lt with
        | Branch(_, _ ,_) -> getmin lt
        | Leaf -> 
          match List.rev lst with
          | [] -> failwith "Invalid tree: empty list as node"
          | h::_ -> h

    (* Simply returns the maximum value of the tree t. Similarly should
     * return the last element in the matching list. *)
    let rec getmax (t : tree) : elt = 
      match t with
      | Leaf -> raise EmptyTree
      | Branch (_, lst, rt) -> 
        match rt with
        | Branch(_, _, _) -> getmax rt
        | Leaf -> 
          match List.rev lst with
          | [] -> failwith "Invalid tree: empty list as node"
          | h::_ -> h
                 
    (* Prints binary search tree as a string - nice for testing! *)
    let to_string (t: tree) = 
      let list_to_string (lst: 'a list) =
        match lst with 
        | [] -> "[]"
        | [hd] -> "[" ^ (C.to_string hd) ^ "]"
        | hd :: tl -> "[" ^ List.fold_left
            (fun a b -> a
            ^ "; "
            ^ (C.to_string b))
            (C.to_string hd) tl ^ "]"
      in
      let rec to_string' (t: tree) = 
        match t with 
        | Leaf -> "Leaf"
        | Branch (l, m, r) ->
                 "Branch (" ^ (to_string' l) ^ ", "
           ^ (list_to_string m) ^ ", " ^ (to_string' r) ^ ")"
      in to_string' t
        
    let test_insert () =
      let x = C.generate () in
      let t = insert x empty in
      assert (t = Branch(Leaf, [x], Leaf));
      let t = insert x t in
      assert (t = Branch(Leaf, [x;x], Leaf));
      let y = C.generate_gt x () in
      let t = insert y t in
      assert (t = Branch(Leaf, [x;x], Branch(Leaf, [y], Leaf)));
      let z = C.generate_lt x () in
      let t = insert z t in
      assert (t = Branch(Branch(Leaf, [z], Leaf),[x;x],
       Branch(Leaf, [y], Leaf)));
      (* Can add further cases here *)
      ()
  
    (* Insert a bunch of elements, and test to make sure that we
     * can search for all of them. *)
    let test_search () =
      let x = C.generate () in
      let t = insert x empty in
      assert (search x t);
      let order = [ true; false; true; true; true; false; false] in
      let full_tree, values_inserted =
        List.fold_right
          (fun current_order (tree_so_far, values_so_far) ->
           let prev_value =
             match values_so_far with
             | [] -> x
             | hd :: _ -> hd
           in
           let value =
             if current_order
             then C.generate_gt prev_value ()
             else C.generate_lt prev_value ()
           in
           insert value tree_so_far, value :: values_so_far
          ) order (t, [])
      in
      List.iter (fun value -> assert (search value full_tree)) values_inserted
    
    (* None of these tests are particularly exhaustive.
     * For instance, we could try varying the order in which we insert
     * values, and making sure that the result is still correct.
     * So, the strategy here is more to try to build up a reasonable degree
     * of coverage across the various code-paths, rather than it is to
     * test exhaustively that our code does the right thing on every single
     * possible input. *)
    let test_getmax () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      assert (getmax (insert x4 (insert x3 (insert x2 (insert x empty)))) = x)
       
    let test_getmin () =
      let x = C.generate () in
      let x2 = C.generate_gt x () in
      let x3 = C.generate_gt x2 () in
      let x4 = C.generate_gt x3 () in
      assert (getmin (insert x2 (insert x4 (insert x (insert x3 empty)))) = x)
       
    let test_delete () =
      let x = C.generate () in
      let x2 = C.generate_lt x () in
      let x3 = C.generate_lt x2 () in
      let x4 = C.generate_lt x3 () in
      let after_ins = insert x4 (insert x3 (insert x2 (insert x empty))) in
      assert (delete x (delete x4 (delete x3 (delete x2 after_ins))) = empty)
       
    let run_tests () =
      test_insert ();
      test_search ();
      test_getmax ();
      test_getmin ();
      test_delete ();
      ()
  
  end
    
(* Here is how you would define an int binary tree using the BinSTree
 * functor, which expects a module to be passed in as an argument.
 * You should write tests using the IntTree module (or you can
 * give the module a different type), and you should use
 * this call to a functor as an example for how to test modules further
 * down in the pset.
 *)
    
module IntTree = BinSTree(IntCompare)
       
(* Please read the entirety of "tests.ml" for an explanation of how
 * testing works.
 *)
       
(* ====================================================================
 * Section 3: Priority queues
 *)

(* A signature for a priority queue. See the pset specification on the
 * course website for more information if you are unfamiliar with 
 * priority queues.
 *
 * IMPORTANT: In your implementations of priority queues, the MINIMUM
 * valued element corresponds to the HIGHEST priority. For example,
 * in an integer priority queue, the integer 4 has lower priority than
 * the integer 2.
 *)
module type PRIOQUEUE =
sig
  exception QueueEmpty

  (* What's being stored in the priority queue *)
  type elt

  (* The queue itself (stores things of type elt) *)
  type queue

  (* Returns an empty queue *)
  val empty : queue

  (* Takes a queue, and returns whether or not it is empty *)
  val is_empty : queue -> bool

  (* Takes an element and a queue, and returns a new queue with the
   * element added *)
  val add : elt -> queue -> queue

  (* Pulls the highest priority element out of the passed-in queue,
   * returning the highest priority elements paired with the queue 
   * with that element removed. Can raise the QueueEmpty exception. *)
  val take : queue -> elt * queue
  
  val to_string : queue -> string

  (* Runs invariant checks on the implementation of this binary tree.
   * May raise Assert_failure exception *)
  val run_tests : unit -> unit
end

(* **** Problem 2 *)
  
(* Implement a priority queue using lists. Feel free to use anything
 * from the List module. 
 *)
module ListQueue(C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
  struct
    (* Remember to use the "C" (COMPARABLE) module! You may want to
     * look above at BinSTree for inspiration *)
    exception QueueEmpty
    (* exception UnexpectedException *)
    
    type elt = C.t
     
    type queue = elt list
         
    (* Prints list as string - nice for testing! *)
    let to_string (q: queue) = 
      let rec to_string' q = 
        match q with 
        | [] -> ""
        | [hd] -> (C.to_string hd)
        | hd :: tl -> (C.to_string hd) ^ ";" ^ (to_string' tl)
      in
      let qs = to_string' q in "[" ^ qs ^ "]"
              
    let empty = []
       
    let is_empty (t : queue) = 
      List.length t = 0
          
    (* Like with getmin and getmax in your binary tree, you should implement
     * add and take such that, if all elements have the same priority, this
     * module simply becomes a regular queue (i.e., elements inserted earlier
     * should be removed before elements of the same priority inserted later)
     *)
    let rec add (e : elt) (q : queue) : queue =
      match q with
      | [] -> [e]
      | h::t ->  
        match C.compare e h with
        | Less -> e::q
        | Equal | Greater -> h :: add e t 
            
    let take (q : queue): elt * queue = 
      match q with
      | [] -> raise QueueEmpty
      | h::t -> (h, t)

    let empty_tests () = 
     assert (is_empty []);
     let x = C.generate () in 
     assert (not(is_empty [x])); ()

    let add_and_take_tests () = 
     let x = C.generate () in
     let one = add x empty in 
     assert (one = [x]); 
     let x2 = C.generate_gt x () in 
     let two = add x2 one in 
     assert (two = [x2; x]); 
     let three = add x2 two in
     assert (three = [x2; x2; x]); 
     let x3 = C.generate_lt x () in 
     let four = add x3 three in
     assert (four = [x2; x2; x; x3]); 
     let remove1 = take four in
     assert (remove1 = (x2, [x2; x; x3])); ()

    (* let take_exception_test () = 
    assert (try 
      take empty;
      false 
    with 
      QueueEmpty -> true
      | _ -> raise UnexpectedException); () *)

    let run_tests () = 
    empty_tests (); 
    add_and_take_tests ();
    (* take_exception_test () ; *)
    ()

  end
    
(* IMPORTANT: Don't forget to actually *call* run_tests, as with
 * IntTree above! *)
    
(* **** Problem 3 *)
    
(* Now implement a functor TreeQueue that generates implementations 
 * of the priority queue signature PRIOQUEUE using a Binary Search Tree. 
 * Luckily, you should be able to use *a lot* of your code from the 
 * work with BINARYTREE above! 
 *  
 * If you run into problems implementing TreeQueue, you can at least
 * add stub code for each of the values you need to implement so that
 * this file will compile and will work with the unit testing
 * code. That way you'll be able to submit the problem set so that it
 * compiles cleanly. *)

(* You'll want to uncomment this before working on this section! *)

module TreeQueue(C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
  struct
    exception QueueEmpty
    
    (* You can use the module T to access the functions defined in BinSTree,
     * e.g. T.insert *)
    module T = (BinSTree(C) : (BINTREE with type elt = C.t))
     
     (* Implement the remainder of the module. *)
    type elt = C.t

    type queue = T.tree

    let empty = T.empty

    let is_empty(tree: queue) = tree = T.empty

    let add = T.insert

    let to_string = T.to_string
    
    let take(t:queue): elt*queue  = let min = T.getmin t in 
      (min, T.delete min t)

    let test_is_empty () = 
      assert (is_empty T.empty);
      let x = C.generate () in
      assert (not(is_empty (T.insert x T.empty))); ()

    let add_tests () = 
     let x = C.generate () in
     let one = add x empty in 
     assert (one = T.insert x T.empty); 
     let x2 = C.generate_gt x () in 
     let two = add x2 one in 
     assert (two = T.insert x2 one); ()

    let run_tests () =
      test_is_empty ();
      add_tests ();
      ()
     
  end

(* **** Problem 4 *)

(* Implement a priority queue using a binary heap. See the problem set 
 * writeup for more info.
 *
 * You should implement a min-heap, i.e., the top of your heap stores the
 * smallest element in the entire heap.
 *
 * Note that, unlike for your tree and list implementations of priority queues,
 * you do *not* need to worry about the order in which elements of equal
 * priority are removed. Yes, this means it's not really a "queue", but
 * it is easier to implement without that restriction.
 *)
module BinaryHeap(C : COMPARABLE) : (PRIOQUEUE with type elt = C.t) =
  struct
    
    exception QueueEmpty
    exception ImpossibleCase
    (* exception UnexpectedException
    *)

    type elt = C.t
     
    (* **********************************************************
     * Be sure to read the pset spec for hints and clarifications.
     * **********************************************************
     * Remember the invariants of the tree that make up your queue:
     * 1) A tree is ODD if its left subtree has 1 more node than its right
     * subtree. It is EVEN if its left and right subtrees have the same number
     * of nodes. The tree can never be in any other state. This is the WEAK
     * invariant, and should never be false.
     *
     * 2) All nodes in the subtrees of a node should be *greater* than 
     * (or equal to) the value of that node. 
     * This, combined with the previous invariant, makes a STRONG invariant.
     * Any tree that a user passes in to your module and receives back from it
     * should satisfy this invariant.  However, in the process of, say, 
     * adding a node to the tree, the tree may intermittently
     * not satisfy the order invariant. If so, you *must* fix the tree before
     * returning it to the user.  Fill in the rest of the module below!
     *)

    (* A node in the tree is either even or odd *)
    type balance = Even | Odd
          
    (* A tree either:
     *   1) is one single element,
     *
     *   2) has one branch, where:  
              the first elt in the tuple is the element at this node,
     *        and the second elt is the element down the branch,
     *
     *   3) or has two branches (with the node being even or odd) *)
    type tree =
      TwoBranch of balance * elt * tree * tree
        | OneBranch of elt * elt
        | Leaf of elt
          
    (* A queue is either empty, or a tree *)
    type queue = Empty | Tree of tree

    let empty = Empty
      
    (* Prints binary heap as a string - nice for testing! *)
    let to_string (q: queue) = 
      let rec to_string' (t: tree) = 
        match t with 
        | Leaf e1 -> "Leaf " ^ C.to_string e1
        | OneBranch(e1, e2) -> 
                 "OneBranch (" ^ C.to_string e1 ^ ", "
           ^ C.to_string e2 ^ ")"
        | TwoBranch(Odd, e1, t1, t2) -> 
                 "TwoBranch (Odd, " ^ C.to_string e1 ^ ", "
           ^ to_string' t1 ^ ", " ^ to_string' t2 ^ ")"
        | TwoBranch(Even, e1, t1, t2) -> 
                 "TwoBranch (Even, " ^ C.to_string e1 ^ ", "
           ^ to_string' t1 ^ ", " ^ to_string' t2 ^ ")"
      in
      match q with
      | Empty -> "Empty"
      | Tree t -> to_string' t
           
    let is_empty (q : queue) = q = Empty
             
    (* Adds element e to the queue q *)
    let add (e : elt) (q : queue) : queue =
      (* Given a tree, where e will be inserted is deterministic based on the
       * invariants. If we encounter a node in the tree where its value is 
       * greater than the element being inserted, then we place the new elt 
       * in that spot and propagate what used to be at that spot down
       * toward where the new element would have been inserted *)
      let rec add_to_tree (e : elt) (t : tree) : tree =
        match t with
        (* If the tree is just a Leaf, then we end up with a OneBranch *)
        | Leaf e1 ->
                 (match C.compare e e1 with
                  | Equal | Greater -> OneBranch (e1, e)
                  | Less -> OneBranch (e, e1))
       
        (* If the tree was a OneBranch, it will now be a TwoBranch *)
        | OneBranch(e1, e2) ->
                 (match C.compare e e1 with
                  | Equal | Greater -> TwoBranch (Even, e1, Leaf e2, Leaf e)
                  | Less -> TwoBranch (Even, e, Leaf e2, Leaf e1))
       
        (* If the tree was even, then it will become an odd tree
         * (and the element is inserted to the left *)
        | TwoBranch(Even, e1, t1, t2) ->
                 (match C.compare e e1 with
                  | Equal | Greater -> TwoBranch(Odd, e1, add_to_tree e t1, t2)
                  | Less -> TwoBranch(Odd, e, add_to_tree e1 t1, t2))
       
        (*If the tree was odd, then it will become an even tree(and the element
         * is inserted to the right *)
        | TwoBranch(Odd, e1, t1, t2) ->
                 match C.compare e e1 with
                 | Equal | Greater -> TwoBranch(Even, e1, t1, add_to_tree e t2)
                 | Less -> TwoBranch(Even, e, t1, add_to_tree e1 t2)
      in
      (* If the queue is empty, then e is the only Leaf in the tree.
       * Else, insert it into the proper location in the pre-existing tree *)
      match q with
      | Empty -> Tree (Leaf e)
      | Tree t -> Tree (add_to_tree e t)
           
    (* Simply returns the top element of the tree t (i.e., just a single pattern
     * match in *)
    let get_top (t : tree) : elt =
        match t with
        | TwoBranch (_, e, _, _) -> e
        | OneBranch (e, _) -> e
        | Leaf e -> e
         
    (* Takes a tree, and if the top node is greater than its children, fixes
     * it. If fixing it results in a subtree where the node is greater than its
     * children, then you must (recursively) fix this tree too. *)  

    (* This helper function returns Less if e is
     * less than the value in tl . It returns
     * Greater if e is Greater than it else
     * it will return Equal if *)
    let fix_helper (e:elt) (tl:tree): order = 
      match tl with
      | TwoBranch(_,value,_,_) |OneBranch(value,_) |Leaf value -> 
        C.compare e value  

    (*Another helper function that will switch nodes*)
    let switch_val (e:elt) (t:tree): tree =
      match t with
      | TwoBranch(x,_,tl,tr) -> TwoBranch(x,e,tl,tr)
      | OneBranch(_,y) -> OneBranch(e,y)
      | Leaf _ -> Leaf e

    let rec fix (t : tree) : tree = 
      match t with
      | TwoBranch(y,root,t_left,t_right) -> 
        (match (fix_helper root t_left),(fix_helper root t_right) with
                    (*If the nodes are equal or greater, return tree
                     * the tree is already correct*)
          | Equal,Equal | Less,Less | Equal,Less | Less,Equal -> t
                    (* If the left node is less than the root,
                     * switch their positions and make sure to 
                     * fix the left node*)
          | Greater,Less | Greater,Equal -> TwoBranch(y, get_top t_left, 
                                      fix (switch_val root t_left), t_right)
                    (*Same as above just with right tree*)
          | Less,Greater | Equal,Greater -> TwoBranch(y, get_top t_right,
                                      t_left, fix (switch_val root t_right))
                    (*If both left and right trees are less we 
                     * can only make the smaller element the root*)
          | Greater,Greater -> 
                    (match C.compare (get_top t_left) (get_top t_right) with
                    | Less | Equal -> TwoBranch(y, get_top t_left, 
                           fix (switch_val root t_left), t_right)
                    | Greater -> TwoBranch(y, get_top t_right,
                           t_left, fix (switch_val root t_right))))
        | Leaf e -> Leaf e
        | OneBranch(root,e) -> (match (C.compare e root) with
                    | Equal | Greater -> OneBranch(root,e)
                    | Less -> OneBranch(e,root))
        
    
         
    let extract_tree (q : queue) : tree =
      match q with
      | Empty -> raise QueueEmpty
      | Tree t -> t
        
    (* Takes a tree, and returns the item that was most recently inserted into
     * that tree, as well as the queue that results from removing that element.
     * Notice that a queue is returned. 
     * (This happens because removing an element from just a leaf
     * would result in an empty case, which is captured by the queue type).
     *
     * By "item most recently inserted", we don't mean the
     * most recently inserted *value*, but rather the newest node that was
     * added to the bottom-level of the tree. If you follow the implementation
     * of add carefully, you'll see that the newest value may end up somewhere
     * in the middle of the tree, but there is always *some* value brought
     * down into a new node at the bottom of the tree. *This* is the node
     * that we want you to return.
     *)

    let rec get_last_helper (t:tree): tree =
      match t with
      (*Match if the *)
      | TwoBranch(Even,e,tl,tr) -> 
                  (match tr with
                   | OneBranch(x,_) -> TwoBranch(Odd,e, tl,Leaf(x))
                   | Leaf _ -> OneBranch(e,get_top tl)
                   | TwoBranch(_,_,_,_) -> 
                     TwoBranch(Odd,e,tl,get_last_helper tr))
      | TwoBranch(Odd,e,tl,tr)->
                 (match tl with
                  | OneBranch(x,_) -> TwoBranch(Even,e,Leaf(x), tr)
                  | Leaf _ -> OneBranch(e,get_top tr)
                  | TwoBranch(_,_,_,_) -> 
                    TwoBranch(Even,e,get_last_helper tl,tr))
      | OneBranch(x,_) -> Leaf x
      | Leaf _ -> raise ImpossibleCase

    let rec get_last_value (t:tree): elt =
      match t with
      | TwoBranch(Even,_,_,tr) -> 
                  (match tr with
                   | OneBranch(_,y) -> y
                   | Leaf x -> x
                   | TwoBranch(_,_,_,_) -> get_last_value tr)
      | TwoBranch(Odd,_,tl,_)->
                 (match tl with
                  | OneBranch(_,y) -> y
                  | Leaf x -> x
                  | TwoBranch(_,_,_,_) -> get_last_value tl)
      | OneBranch(_,y) -> y
      | Leaf x -> x   

    let get_last (t : tree) : elt * queue =
      match t with
      | Leaf x -> (x,Empty)
      | _ -> let q = get_last_helper t in 
             let l = get_last_value t in
             (l,Tree(q))       
         
    (* Implements the algorithm described in the writeup. You must finish this
     * implementation, as well as the implementations of get_last and fix, which
     * take uses *)
    let take (q : queue) : elt * queue =
      match extract_tree q with
      (* If the tree is just a Leaf, then return the value of that leaf, and the
       * new queue is now empty *)
      | Leaf e -> e, Empty
           
      (* If the tree is a OneBranch, then the new queue is just a Leaf *)
      | OneBranch (e1, e2) -> e1, Tree (Leaf e2)
               
      (* Removing an item from an even tree results in an odd tree. This
       * implementation replaces the root node with the most recently inserted
       * item, and then fixes the tree that results if it is violating the
       * strong invariant *)
      | TwoBranch (Even, e, t1, t2) ->
          let (last, q2') = get_last t2 in
         (match q2' with
          (* If one branch of the tree was just a leaf, we now have just
                 * a OneBranch *)
          | Empty -> (e, Tree (fix (OneBranch (last, get_top t1))))
          | Tree t2' -> (e, Tree (fix (TwoBranch (Odd, last, t1, t2')))))
            (* Implement the odd case! *)
      | TwoBranch (Odd, e, t1, t2) -> 
          let (last, q1') = get_last t1 in 
          (match q1' with
          | Empty -> (e, Tree (fix (OneBranch (last, get_top t2))))
          | Tree t1' -> (e, Tree (fix (TwoBranch (Even, last, t1', t2)))))


    (* let exception_tests () = 
    let x = C.generate () in 
    assert (try 
      get_last_helper (Leaf x); 
      false 
    with 
      ImpossibleCase -> true
      | _ -> raise UnexpectedException); () *)

    let take_tests () = 
      let a = C.generate () in
      let b = add a empty in
      assert (b = Tree(Leaf a));
      let c = C.generate_lt a () in
      let d = C.generate_gt c () in
      let b = add c b in
      assert (b = Tree(OneBranch(c, a)));
      let b = add d b in
      let e = take b in
      assert (e = (a, Tree(OneBranch(c, d)))); 
      assert (b = Tree(TwoBranch(Even, a, Leaf c, Leaf d)));
      assert (get_top (Leaf a) = a);
      assert (get_last (Leaf a) = (a, Empty));
      assert (fix (Leaf a) = (Leaf a));
      assert (fix (TwoBranch(Even, a, Leaf c, Leaf d)) = 
        (TwoBranch(Even, a, Leaf c, Leaf d)));
      ()

    let run_tests () =
      take_tests ();
      (*exception_tests (); *)
      ()
  end
    
(* Now to actually use our priority queue implementations for something useful!
 *
 * Priority queues are very closely related to sorts. Remember that removal of
 * elements from priority queues removes elements in highest priority to lowest
 * priority order. So, if your priority for an element is directly related to
 * the value of the element, then you should be able to come up with a simple
 * way to use a priority queue for sorting...
 *
 * In OCaml 3.12 and above, modules can be turned into first-class
 * values, and so can be passed to functions! Here, we're using that to avoid
 * having to create a functor for sort. Creating the appropriate functor
 * is a challenge problem :-)
 *)
    
(* The following code is simply using the functors and passing in a
 * COMPARABLE module for integers, resulting in priority queues
 * tailored for ints.
 *)
module IntListQueue = (ListQueue(IntCompare) :
                         PRIOQUEUE with type elt = IntCompare.t)
      
module IntHeapQueue = (BinaryHeap(IntCompare) :
                         PRIOQUEUE with type elt = IntCompare.t)    

module IntTreeQueue = (TreeQueue(IntCompare) :
                        PRIOQUEUE with type elt = IntCompare.t)

(* Store the whole modules in these variables *)
let list_module = (module IntListQueue : PRIOQUEUE with type elt = IntCompare.t)
let heap_module = (module IntHeapQueue : PRIOQUEUE with type elt = IntCompare.t)
let tree_module = (module IntTreeQueue : PRIOQUEUE with type elt = IntCompare.t)

(* Implements sort using generic priority queues. *)
let sort (m : (module PRIOQUEUE with type elt=IntCompare.t)) (lst : int list) =
  let module P = (val (m) : PRIOQUEUE with type elt = IntCompare.t) in
  let rec extractor pq lst =
    if P.is_empty pq then lst
    else
      let (x, pq') = P.take pq in
      extractor pq' (x::lst) in
  let pq = List.fold_right P.add lst P.empty in
  List.rev (extractor pq [])
     

(* Hurray!! Now, we can pass in the modules into sort and get out
 * different sorts!! *)

(* Sorting with a priority queue with an underlying heap
 * implementation is equivalent to heap sort! *)
let heapsort = sort heap_module

(* Sorting with a priority queue with your underlying tree
 * implementation is *almost* equivalent to treesort;
 * a real treesort relies on self-balancing binary search trees *)


let treesort = sort tree_module


(* Sorting with a priority queue with an underlying unordered list
 * implementation is equivalent to heap sort! If your implementation of
 * ListQueue used ordered ilsts, then this is really insertion sort *)
let selectionsort = sort list_module

(* You should test that these sorts all correctly work, and that
 * lists are returned in non-decreasing order! *)

let test_sort () = 
  assert (selectionsort [1; 32 ; 4] = [1; 4; 32]);
  assert (selectionsort [] = []);
  assert (selectionsort [2] = [2]);
  assert (heapsort [3] = [3]);
  assert (heapsort [23; 123; 34; 12932] = [23; 34; 123; 12932]);
  assert (heapsort [] = []);
  assert (treesort [3] = [3]);
  assert (treesort [23; 123; 34; 12932] = [23; 34; 123; 12932]);
  assert (treesort [] = []);  
  ()

let run_tests () =
  test_sort ();
  ()