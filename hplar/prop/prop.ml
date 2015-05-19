
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


(* 2.5 Simplification and negation normal form *)

let psimplify1 fm = 
  match fm with
  | Not False -> True
  | Not True -> False
  | Not (Not f) -> f
  | And(f, False) | And(False, f) -> False
  | And(f, True) | And(True, f) -> f
  | Or(f, True) | Or(True, f) -> True
  | Or(f, False) | Or(False, f) -> f
  | Imp(False, f) | Imp(f, True) -> True
  | Imp(True, f) -> f
  | Imp(f, False) -> Not f
  | Iff(f, True) | Iff(True, f) -> f
  | Iff(f, False) | Iff(False, f) -> Not f
  | _ -> fm

let rec psimplify fm = 
  match fm with
  | Not f -> psimplify1 @@ Not (psimplify f)
  | And(f1, f2) -> psimplify1 @@ And(psimplify f1, psimplify f2)
  | Or(f1, f2)  -> psimplify1 @@ Or(psimplify f1, psimplify f2)
  | Imp(f1, f2) -> psimplify1 @@ Imp(psimplify f1, psimplify f2)
  | Iff(f1, f2) -> psimplify1 @@ Iff(psimplify f1, psimplify f2)
  | _ -> fm

let negative = function (Not p) -> true | _ -> false

let positive lit = not @@ negative lit

let negate = function (Not p) -> p | p -> Not p

let rec nnf1 fm = 
  match fm with
  | And(f1, f2) -> And(nnf1 f1, nnf1 f2)
  | Or(f1, f2)  -> Or(nnf1 f1, nnf1 f2)
  | Imp(f1, f2) -> Or(nnf1 @@ Not f1, nnf1 f2)
  | Iff(f1, f2) -> Or(And(nnf1 f1, nnf1 f2), And(nnf1 @@ Not f1, nnf1 @@ Not f2))
  | Not (Not f) -> nnf1 f
  | Not (And(f1, f2)) -> Or(nnf1 @@ Not f1, nnf1 @@ Not f2)
  | Not (Or(f1, f2))  -> And(nnf1 @@ Not f1, nnf1 @@ Not f2)
  | Not (Imp(f1, f2)) -> And(nnf1 f1, nnf1 @@ Not f2)
  | Not (Iff(f1, f2)) -> Or(And(nnf1 f1, nnf1 @@ Not f2), And(nnf1 @@ Not f1, nnf1 f2))
  | _ -> fm

let nnf fm = nnf1 @@ psimplify fm

let rec nenf1 fm = 
  match fm with
  | Not (Not f) -> nenf1 f
  | Not (And(f1, f2)) -> Or(nenf1 @@ Not f1, nenf1 @@ Not f2)
  | Not (Or(f1, f2)) -> And(nenf1 @@ Not f1, nenf1 @@ Not f2)
  | Not (Imp(f1, f2)) -> And(nenf1 f1, nenf1 @@ Not f2)
  | Not (Iff(f1, f2)) -> Iff(nenf1 f1, nenf1 @@ Not f2)
  | And(f1, f2) -> And(nenf1 f1, nenf1 f2)
  | Or(f1, f2)  -> Or(nenf1 f1, nenf1 f2)
  | Imp(f1, f2) -> Or(nenf1 @@ Not f1, nenf1 f2)
  | Iff(f1, f2) -> Iff(nenf1 f1, nenf1 f2)
  | _ -> fm

let nenf fm = nenf1 @@ psimplify fm

(* 2.6 Disjunctive and conjunctive normal forms *)

(* DNF via truth tables *)

let list_conj = function [] -> True | l -> Util.foldr_last mk_and l

let list_disj = function [] -> False | l -> Util.foldr_last mk_or l

let mk_lits pvs v = 
  list_conj @@ List.map (fun p -> if eval p v then p else Not p) pvs

let rec allsatvaluations subfn v pvs = 
  match pvs with
  | [] -> if subfn v then [v] else []
  | p :: ps -> let v' t q = if q = p then t else (v q) in
               allsatvaluations subfn (v' false) ps @
                 allsatvaluations subfn (v' true) ps

let dnf1 fm = 
  let pvs = atoms fm in
  let satvals = allsatvaluations (eval fm) (fun s -> false) pvs in
  list_disj @@ List.map (mk_lits @@ List.map (fun p -> Atom p) pvs) satvals

(* DNF via transformation *)

let rec distrib fm = 
  match fm with
  | And(p, Or(q, r)) -> Or(distrib @@ And(p, q), distrib @@ And(p, r))
  | And(Or(p, q), r) -> Or(distrib @@ And(p, r), distrib @@ And(q, r))
  | _ -> fm

let rec rawdnf fm =
  match fm with
  | And(p, q) -> distrib @@ And(rawdnf p, rawdnf q)
  | Or(p, q) -> Or(rawdnf p, rawdnf q)
  | _ -> fm

(* Set-based representation *)

let distribute s1 s2 = Util.(setify @@ allpairs union s1 s2)

let rec purednf fm = 
  match fm with
  | And(p, q) -> distribute (purednf p) (purednf q)
  | Or(p, q) -> Util.union (purednf p) (purednf q)
  | _ -> [[fm]]

let trivial lits = 
  let pos, neg = List.partition positive lits in 
  (Util.intersect pos (Util.image negate neg)) <> []

(* shortcut for notational convenience *)
let non = Util.non

let simpdnf fm = 
  match fm with
  | False -> []
  | True -> [[]]
  | _ -> let djs = List.filter (non trivial) (purednf @@ nnf fm) in
         List.filter (fun d -> not (List.exists (fun d' -> Util.psubset d' d) djs)) djs

let dnf fm = list_disj @@ List.map list_conj (simpdnf fm)


(* CNF *)
let purecnf fm = Not fm |> nnf |> purednf |> Util.image (Util.image negate)

let simpcnf fm = 
  match fm with
  | False -> [[]]
  | True -> []
  | _ -> let cjs = List.filter (non trivial) (purecnf fm) in
         List.filter (fun c -> not (List.exists (fun c' -> Util.psubset c' c) cjs)) cjs

let cnf fm = list_conj @@ List.map list_disj (simpcnf fm)


(* 2.7 Applications of propositional logic *)

(* Ramsey's theorem *)

let ramsey s t n = 
  let vertices = Util.range 1 n in
  let yesgrps = List.map (Util.allsets 2) (Util.allsets s vertices) in
  let nogrps = List.map (Util.allsets 2) (Util.allsets t vertices) in
  let e [m;n] = Atom (P ("p_" ^ (string_of_int m) ^"_"^ (string_of_int n))) in
  Or (list_disj @@ List.map (fun g -> list_conj @@ List.map e g) yesgrps,
      list_disj @@ List.map (fun g -> list_conj @@ List.map (fun p -> Not (e p)) g) nogrps)

(* Digital circuits *)

let halfsum x y = Iff(x, Not y)

let halfcarry x y = And(x, y)

let ha x y s c = And(Iff(s, halfsum x y), Iff(c, halfcarry x y))

let carry x y z = Or(And(x, y), And(Or(x, y), z))

let sum x y z = halfsum (halfsum x y) z

let fa x y z s c = And(Iff(s, sum x y z), Iff(c, carry x y z))

let conjoin f l = list_conj (map f l)

(* Addition *)

let ripplecarry x y c out n = 
  conjoin (fun i -> fa (x i) (y i) (c i) (out i) (c (i+1)))
          Util.range 0 (n-1)

let mk_index x i = Atom(P (x ^ "_" ^ (string_of_int i)))

let mk_index2 x i j = 
  Atom(P (x ^"_"^ (string_of_int i) ^"_"^ (string_of_int j)))

let ripplecarry0 x y c out n = 
  psimplify @@
    ripplecarry x y (fun i -> if i = 0 then False else c i) out n

let ripplecarry1 x y c out n = 
  psimplify @@
    ripplecarry x y (fun i -> if i = 0 then True else c i) out n

let mux sel in0 in1 = Or(And(Not sel, in0), And(sel, in1))

let offset n x i = x (n + i)

let rec carryselect x y c0 c1 s0 s1 c s n k = 
  let k' = min n k in
  let fm = 
    And(And(ripplecarry0 x y c0 s0 k', ripplecarry1 x y c1 s1 k'),
        And(Iff(c k', mux (c 0) (c0 k') (c1 k')),
            conjoin (fun i -> Iff(s i, mux (c 0) (s0 i) (s1 i)))
                    Util.range 0 (k' - 1) 
  in
  if k' < k then fm else
    And(fm, carryselect (offset k x) (offset k y) (offset k c0) (offset k c1)
                        (offset k s0) (offset k s1) (offset k c) (offset k s)
                        (n - k) k)

let mk_adder_test n k = 
  let [x; y; c; s; c0; s0; c1; s1; c2; s2] = 
    List.map mk_index ["x"; "y"; "c"; "s"; "c0"; "s0"; "c1"; "s2"; "c2"; "s2"] 
  in
  Imp(And(And(carryselect x y c0 c1 s0 s1 c s n k, Not (c 0)),
          ripplecarry0 x y c2 s2 n),
      And(Iff(c n, c2 n),
          conjoin (fun i -> Iff(s i, s2 i)) (Util.range 0 (n-1))

(* Multiplication *)

let rippleshift u v c z w n = 
  ripplecarry0 u v (fun i -> if i = n then w (n-1) else c (i+1))
               (fun i -> if i = 0 then z else w (i-1)) n

let multiplier x u v out n = 
  if n = 1 then And(Iff(out 0, x 0 0), Not (out 1)) 
  else psimplify @@
         And(Iff(out 0, x 0 0),
             And(rippleshift (fun i -> if i = n-1 then False else x 0 (i+1))
                             (x 1) (v 2) (out 1) (u 2) n,
                 if n = 2 then And(Iff(out 2, u 2 0), Iff(out 3, u 2 1))
                 else conjoin (fun k -> rippleshift (u k) (x k) (v (k+1)) (out k)
                                                    (if k = n-1 then fun i -> out (n+i)
                                                     else u (k+1)) n) 
                              (Util.range 2 (n-1))))

(* Primality and factorization *)

