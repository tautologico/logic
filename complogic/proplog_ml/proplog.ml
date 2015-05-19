
(* Representacao para proposicoes *)
type prop = string

(* Clausulas: fatos e regras *)
type clause = { 
  head : prop;
  body : prop list 
}


(* Parsing: analise sintatica da entrada *)
let is_lower c = 
  c >= 'a' && c <= 'z'

let is_upper c = 
  c >= 'A' && c <= 'Z'

let is_digit c = 
  c >= '0' && c <= '9'

let is_whitespace c = 
  String.contains " \t\r\n" c

let prop_char c = 
  (is_lower c) || (is_upper c) || (is_digit c)

let string_chop s i = 
  let len = String.length s in
  String.sub s i (len-i)

let string_split s i = 
  (String.sub s 0 i, string_chop s i)

(** Encontra o primeiro indice na string s que nao satisfaz o predicado p. *)
let string_while_ix p s = 
  let len = String.length s in
  let rec loop i = 
    if i >= len then None
    else if (p s.[i]) then loop (i+1) else Some i
  in
  loop 0

let skip_whitespace s = 
  match string_while_ix is_whitespace s with
    None -> ""
  | Some i -> string_chop s i

let parse_prop s = 
  let s' = skip_whitespace s in 
  if not @@ is_lower s'.[0] then None
  else 
    match string_while_ix prop_char s' with
      None -> None
    | Some i -> Some (string_split s' i)

let skip_turnstile s = 
  let s' = skip_whitespace s in
  if s'.[0] = ':' && s'.[1] = '-' then Some (string_chop s' 2)
  else None
