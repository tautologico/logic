
(* 

  Propositional logic and SAT solving

*)

(* type for primitive propositions *)
type prop = P of string

let pname (P s) = s

let show_propvar p = pname p 
let print_propvar p = print_string @@ show_propvar p

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

(* A few builtin propositions as formulas *)
module Props = struct 
  let p_prop = P "P"
  let p = Atom p_prop
  let q_prop = P "Q"
  let q = Atom q_prop
  let r_prop = P "R"
  let r = Atom r_prop
  let s_prop = P "S"
  let s = Atom s_prop

  let pi i = Atom (P ("P" ^ i))
  let qi i = Atom (P ("Q" ^ i))
end

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

(* syntax operations *)

let mk_and p q = And(p, q)
let mk_or p q = Or(p, q)
let mk_imp p q = Imp(p, q)
let mk_iff p q = Iff(p, q)

let dest_iff fm = 
  match fm with Iff(p, q) -> (p, q) | _ -> failwith "dest_iff"

let dest_and fm = 
  match fm with And(p, q) -> (p, q) | _ -> failwith "dest_and"

let rec conjuncts fm = 
  match fm with 
    And(p, q) -> conjuncts p @ conjuncts q 
  | _ -> [fm]

let dest_or fm = 
  match fm with Or(p, q) -> (p, q) | _ -> failwith "dest_or"

let rec disjuncts fm = 
  match fm with 
    Or(p, q) -> disjuncts p @ disjuncts q 
  | _ -> [fm]

let dest_imp fm = 
  match fm with Imp(p, q) -> (p, q) | _ -> failwith "dest_imp"

let antecedent fm = fst @@ dest_imp fm
let consequent fm = snd @@ dest_imp fm

let rec onatoms f fm = 
  match fm with
  | Atom a -> f a
  | Not(p) -> Not(onatoms f p)
  | And(p, q) -> And(onatoms f p, onatoms f q)
  | Or(p, q) -> Or(onatoms f p, onatoms f q)
  | Imp(p, q) -> Imp(onatoms f p, onatoms f q)
  | Iff(p, q) -> Iff(onatoms f p, onatoms f q)
  | True | False -> fm

let rec overatoms f fm b = 
  match fm with
  | Atom(a) -> f a b 
  | Not(p) -> overatoms f p b
  | And(p, q) | Or(p, q) | Imp(p, q) | Iff(p, q) -> overatoms f p (overatoms f q b)
  | True | False -> b

let atom_union f fm = Util.setify @@ overatoms (fun h t -> (f h) @ t) fm []


(* The semantics of propositional logic *)

let rec eval fm v = 
  match fm with
  | False -> false
  | True -> true
  | Atom x -> v x
  | Not p -> not @@ eval p v
  | And(p, q) -> (eval p v) && (eval q v)
  | Or(p, q) -> (eval p v) || (eval q v)
  | Imp(p, q) -> (not @@ eval p v) || (eval q v) 
  | Iff(p, q) -> (eval p v) = (eval q v)


let atoms fm = atom_union (fun x -> [x]) fm

let rec onallvaluations subfn v ats = 
  match ats with
  | [] -> subfn v
  | p :: ps -> let v' t q = if q = p then t else (v q) in
               onallvaluations subfn (v' false) ps &&
                 onallvaluations subfn (v' true) ps

let print_truthtable fm = 
  let ats = atoms fm in
  let width = List.fold_right (fun p -> max @@ String.length @@ pname p) ats (5+1) in
  let fixw s = s ^ Bytes.make (width - String.length s) ' ' in
  let truthstring p = fixw (if p then "true" else "false") in
  let mk_row v = 
    let lis = List.map (fun x -> truthstring @@ v x) ats
    and ans = truthstring @@ eval fm v in
    print_endline @@ List.fold_right (^) lis ("| " ^ ans); true 
  in
  let separator = String.make (width * (List.length ats) + 9) '-' in
  print_endline @@ List.fold_right (fun s t -> fixw (pname s) ^ t) ats "| formula";
  print_endline separator;
  let _ = onallvaluations mk_row (fun x -> false) ats in
  print_endline separator

(* Tautology and satisfiability checking *)

let tautology fm = 
  onallvaluations (eval fm) (fun p -> false) (atoms fm)

let unsatisfiable fm = tautology (Not fm)

let satisfiable fm = not @@ unsatisfiable fm


(* Substitution *)

(* substitute the proposition p with formula fm in phi *)
let psubst p fm phi = 
  onatoms (fun p' -> if p = p' then fm else Atom p') phi


(* Duality *)

let rec dual fm = 
  match fm with
  | True -> False
  | False -> True
  | Atom p -> fm
  | Not f -> Not (dual f)
  | And(f1, f2) -> Or(dual f1, dual f2)
  | Or(f1, f2) -> And(dual f1, dual f2)
  | _ -> failwith "No dual formula for connectives ==> or <=>"

