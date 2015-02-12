open OUnit2
open Prop

(* A few test formulas *)
let f1 = Props.(p ++ q ** r)
let f2 = Props.(p ** q ** r)
let f3 = Props.(p ** q ==> q ** r)

(* Valuations for P, Q & R *)
let val1 p = 
  match p with 
  | P "P" -> true
  | P "Q" -> false
  | P "R" -> false
  | _ -> failwith "undefined valuation for proposition"

let val2 p = 
  match p with
  | P "P" -> true
  | P "Q" -> false
  | P "R" -> true
  | _ -> failwith "undefined valuation for proposition"

let val3 p = 
  match p with
  | P "P" -> true
  | P "Q" -> true
  | P "R" -> false
  | _ -> failwith "undefined valuation for proposition"

let test_show ctxt = 
  assert_equal (show_prop_formula f1) "(P ++ (Q ** R))"

let test_conjuncts ctxt = 
  assert_equal (conjuncts f2) Props.([p; q; r])

let test_onatoms ctxt = 
  assert_equal (onatoms (fun x -> Atom x) f1) f1  (* simple sanity check *)

let test_atomunion ctxt = 
  assert_equal (atom_union (fun x -> [x]) f1) [P "P"; P "Q"; P "R"];
  assert_equal (atom_union (fun x -> [x]) f3) [P "P"; P "Q"; P "R"]

let test_eval ctxt = 
  assert_equal (eval f1 val1) true;
  assert_equal (eval f2 val1) false;
  assert_equal (eval f3 val2) true;
  assert_equal (eval f3 val3) false

let test_tautology ctxt = 
  let open Props in
  assert_equal (tautology (p ++ ~~p)) true;
  assert_equal (tautology (p ++ q ==> p)) false;
  assert_equal (tautology (p ++ q ==> q ++ (p <=> q))) false;
  assert_equal (tautology ((p ++ q) ** ~~(p ** q) ==> (~~p <=> q))) true;
  assert_equal (tautology (p ++ (q <=> r) <=> (p ++ q <=> p ++ r))) true;
  assert_equal (tautology ((p ==> q) <=> (q ==> p))) false

let test_subst ctxt = 
  let open Props in
  assert_equal (psubst p_prop (p ** q) (p ** q ** q ** p)) 
               ((p ** q) ** q ** q ** (p ** q))

let suite1 = 
  "Tests" >:::
    [ 
      "show_prop_formula" >:: test_show;
      "conjuncts" >:: test_conjuncts;
      "onatoms" >:: test_onatoms;
      "atomunion (and overatoms)" >:: test_atomunion;
      "evaluation" >:: test_eval;
      "tautology checking" >:: test_tautology;
      "substitution" >:: test_subst
    ]

let () = 
  run_test_tt_main suite1



