
(* 

  Propositional logic and SAT solving

*)

(* type for primitive propositions *)
type prop = P of string

let pname (P s) = s

let show_propvar p = pname p 
let print_propvar p = print_string @@ show_propvar p

(* A few builtin propositions as formulas *)
module Props = struct 
  let p = Atom (P "P")
  let q = Atom (P "Q")
  let r = Atom (P "R")

  let pi i = Atom (P ("P" ^ i))
  let qi i = Atom (P ("Q" ^ i))
end

(* we're only interested in propositional logic here, 
   so the only atoms are primitive propositions, and we 
   drop the quantifiers *)
type formula = 
  | False
  | True
  | Atom of prop
  | Not of formula
  | And of formula * formula
  | Or of formula * formula
  | Imp of formula * formula
  | Iff of formula * formula

(* Operators for simplifying surface syntax (without camlp4/camlp5) *)
let ( ~~ ) f = Not f

let ( ++ ) f1 f2 = Or(f1, f2)

let ( ** ) f1 f2 = And(f1, f2)

let (==>) f1 f2 = Imp(f1, f2)

let (<=>) f1 f2 = Iff(f1, f2)

(* A very simple pretty-printer *)
let rec show_prop_formula f = 
  match f with
  | True -> "True"
  | False -> "False"
  | Atom p -> show_propvar p 
  | Not f1 -> "( ~~ " ^ (show_prop_formula f1) ^ ")"
  | And(f1, f2) -> "(" ^ (show_prop_formula f1) ^ " ** " ^ (show_prop_formula f2) ^ ")"
  | Or(f1, f2) -> "(" ^ (show_prop_formula f1) ^ " ++ " ^ (show_prop_formula f2) ^ ")"
  | Imp(f1, f2) -> "(" ^ (show_prop_formula f1) ^ " ==> " ^ (show_prop_formula f2) ^ ")"
  | Iff(f1, f2) -> "(" ^ (show_prop_formula f1) ^ " <=> " ^ (show_prop_formula f2) ^ ")"

let print_prop_formula f = print_string @@ show_prop_formula f

