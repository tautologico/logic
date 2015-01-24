open OUnit2
open Prop

let f1 = Props.(p ++ q ** r)
let f2 = Props.(p ** q ** r)

let test_show ctxt = 
  assert_equal (show_prop_formula f1) "(P ++ (Q ** R))"

let test_conjuncts ctxt = 
  assert_equal (conjuncts f2) Props.([p; q; r])

let test_onatoms ctxt = 
  assert_equal (onatoms (fun x -> Atom x) f1) f1  (* simple sanity check *)

let test_atomunion ctxt = 
  assert_equal (atom_union (fun x -> [x]) f1) [P "P"; P "Q"; P "R"]

let test_eval ctxt = 
  let val1 p = 
    match p with 
    | P "P" -> true
    | P "Q" -> false
    | P "R" -> false
    | _ -> failwith "undefined valuation for proposition"
  in
  assert_equal (eval f1 val1) true;
  assert_equal (eval f2 val1) false

let suite1 = 
  "Tests" >:::
    [ 
      "show_prop_formula" >:: test_show;
      "conjuncts" >:: test_conjuncts;
      "onatoms" >:: test_onatoms;
      "atomunion (and overatoms)" >:: test_atomunion;
      "evaluation" >:: test_eval 
    ]

let () = 
  run_test_tt_main suite1



