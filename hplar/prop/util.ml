(*
 * 
 * Utility functions, some taken from or adapted from the file lib.ml 
 * in the book's code.
 *
 *)


let non p x = not @@ p x 

let rec range m n = if m > n then [] else m :: (range (m+1) n)

(** Merging of sorted lists (maintaining repetitions). *)
let rec merge ord l1 l2 =
  match l1 with
    [] -> l2
  | h1::t1 -> match l2 with
                [] -> l1
              | h2::t2 -> if ord h1 h2 then h1::(merge ord t1 l2)
                          else h2::(merge ord l1 t2);;

(** bottom-up mergesort *)
let sort ord =
  let rec mergepairs l1 l2 =
    match (l1,l2) with
        ([s],[]) -> s
      | (l,[]) -> mergepairs [] l
      | (l,[s1]) -> mergepairs (s1::l) []
      | (l,(s1::s2::ss)) -> mergepairs ((merge ord s1 s2)::l) ss in
  fun l -> if l = [] then [] else mergepairs [] (List.map (fun x -> [x]) l);;

(* predicates to use with sort *)
let increasing f x y = Pervasives.compare (f x) (f y) < 0;;

let decreasing f x y = Pervasives.compare (f x) (f y) > 0;;

(** [allpairs f l1 l2] combines all possible pairs of 
    elements from [l1] and [l2] using [f]; returns the list of results. *)
let rec allpairs f l1 l2 =
  match l1 with
  | [] -> []
  | h1::t1 -> List.fold_right (fun x a -> f h1 x :: a) l2 (allpairs f t1 l2)  

(** [uniq l] returns a list similar to [l] but with duplicated elements 
    eliminated. *)
let uniq l = 
  List.fold_right (fun i l -> if List.mem i l then l else i :: l) l []

(** A fold right using the last element of the list as a starting value. *)
let rec foldr_last f l = 
  match l with
  | [] -> failwith "foldr_last"
  | [x] -> x
  | x :: xs -> f x (foldr_last f xs)

(** Create a set from the elements of a list. A set is just a sorted list 
    without duplicate elements. *)
let setify =
  let rec canonical lis =
     match lis with
       x::(y::_ as rest) -> Pervasives.compare x y < 0 && canonical rest
     | _ -> true in
  fun l -> if canonical l then l
           else uniq (sort (fun x y -> Pervasives.compare x y <= 0) l);;

let union =
  let rec union l1 l2 =
    match (l1,l2) with
        ([],l2) -> l2
      | (l1,[]) -> l1
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then h1::(union t1 t2)
          else if h1 < h2 then h1::(union t1 l2)
          else h2::(union l1 t2) in
  fun s1 s2 -> union (setify s1) (setify s2);;

let intersect =
  let rec intersect l1 l2 =
    match (l1,l2) with
        ([],l2) -> []
      | (l1,[]) -> []
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then h1::(intersect t1 t2)
          else if h1 < h2 then intersect t1 l2
          else intersect l1 t2 in
  fun s1 s2 -> intersect (setify s1) (setify s2);;

let subtract =
  let rec subtract l1 l2 =
    match (l1,l2) with
        ([],l2) -> []
      | (l1,[]) -> l1
      | ((h1::t1 as l1),(h2::t2 as l2)) ->
          if h1 = h2 then subtract t1 t2
          else if h1 < h2 then h1::(subtract t1 l2)
          else subtract l1 t2 in
  fun s1 s2 -> subtract (setify s1) (setify s2);;

let subset,psubset =
  let rec subset l1 l2 =
    match (l1,l2) with
        ([],l2) -> true
      | (l1,[]) -> false
      | (h1::t1,h2::t2) ->
          if h1 = h2 then subset t1 t2
          else if h1 < h2 then false
          else subset l1 t2
  and psubset l1 l2 =
    match (l1,l2) with
        (l1,[]) -> false
      | ([],l2) -> true
      | (h1::t1,h2::t2) ->
          if h1 = h2 then psubset t1 t2
          else if h1 < h2 then false
          else subset l1 t2 in
  (fun s1 s2 -> subset (setify s1) (setify s2)),
  (fun s1 s2 -> psubset (setify s1) (setify s2));;

(** [image f s] calculates the image of function [f] over set [s]. *)
let image f s = setify (List.map f s);;

(* ------------------------------------------------------------------------- *)
(* Finding all subsets or all subsets of a given size.                       *)
(* ------------------------------------------------------------------------- *)

let rec allsets m l =
  if m = 0 then [[]] else
  match l with
    [] -> []
  | h::t -> union (image (fun g -> h::g) (allsets (m - 1) t)) (allsets m t);;
