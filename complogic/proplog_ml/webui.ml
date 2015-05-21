(* 
  
  Interface web para interpretador Proplog usando Js_of_ocaml

  Andrei de A. Formiga - 2015-05-17

 *)

module Html = Dom_html
let js = Js.string
let doc = Html.window##document

let base = ref []

let text_input name value =
  let res = document##createDocumentFragment() in
  Dom.appendChild res (document##createTextNode(js name));
  let input = Html.createInput ~_type:(js"text") document in
  input##value <- js (string_of_int !value);
  Dom.appendChild res input;
  res

let button name callback =
  let res = document##createDocumentFragment() in
  let input = Html.createInput ~_type:(js "submit") document in
  input##value <- js name;
  input##onclick <- Html.handler callback;
  Dom.appendChild res input;
  res

let onload _ = 
  let d = Html.document in
  let main = Js.Opt.get (d##getElementById(js "main")) (fun () -> assert false) in
  let textbox = Html.createTextArea d in
  textbox##rows <- 30; textbox##cols <- 80; 
  Dom.appendChild main textbox;
  Dom.appendChild main (button "Carregar dados"
                               fun _ -> 
                               base := Proplog.parseProgram textbox##value;
                               Js._false);
  Dom.appendChild main (text_input "Consulta" "");
  Js._false

let _ = 
  Html.window##onload <- Html.handler onload
