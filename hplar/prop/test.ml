open OUnit2
open Prop

let f = Props.(p ++ q ** r)

let test_show ctxt = 
  assert_equal (show_prop_formula f) "(P ++ (Q ** R))"

let suite1 = 
  "Tests" >:::
    [ "show_prop_formula" >:: test_show ]

let () = 
  run_test_tt_main suite1



