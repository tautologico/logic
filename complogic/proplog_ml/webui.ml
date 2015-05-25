(* 
  
  Interface web para interpretador Proplog usando Js_of_ocaml

  Andrei de A. Formiga - 2015-05-17

 *)

module Html = Dom_html
let js = Js.string
let doc = Html.window##document

let base = ref []
let q_text = ref ""

let text_input name value =
  let res = doc##createDocumentFragment() in
  Dom.appendChild res (doc##createTextNode(js name));
  let input = Html.createInput ~_type:(js"text") doc in
  input##value <- js (!value);
  input##size <- 60;
  input##onchange <- Html.handler
                       (fun _ -> 
                        value := Js.to_string input##value;
                        input##value <- js !value;
                        Js._false);
  Dom.appendChild res input;
  res

let button name callback =
  let res = doc##createDocumentFragment() in
  let input = Html.createInput ~_type:(js "submit") doc in
  input##value <- js name;
  input##onclick <- Html.handler callback;
  Dom.appendChild res input;
  res

let show_msg msg fb = 
  fb##value <- (js msg)

let show_error msg fb = 
  fb##value <- (js @@ "Erro:\n" ^ msg)

let query_results query resp = 
  let res = if Proplog.establish_goal !base query then "Sim" else "Nao" in
  let rtx = "?- " ^ query ^ "\n" ^ res in
  resp##value <- js rtx

let create_feedback_box d = 
  let feedback = Js.Opt.get (d##getElementById(js "feedback")) (fun () -> assert false) in
  let fbtb = Html.createTextarea d in
  fbtb##rows <- 30; fbtb##cols <- 50;
  Dom.appendChild feedback (d##createTextNode(js "Resultado:"));
  Dom.appendChild feedback (Html.createBr d);
  Dom.appendChild feedback fbtb;
  Dom.appendChild feedback (Html.createBr d);
  fbtb

let create_response_box d = 
  let response = Js.Opt.get (d##getElementById(js "response")) (fun () -> assert false) in
  let rta = Html.createTextarea d in
  rta##rows <- 8; rta##cols <- 50;
  Dom.appendChild response (d##createTextNode(js "Resposta:"));
  Dom.appendChild response (Html.createBr d);
  Dom.appendChild response rta;
  Dom.appendChild response (Html.createBr d);
  rta
  
let onload _ = 
  let d = Html.document in
  let main = Js.Opt.get (d##getElementById(js "main")) (fun () -> assert false) in
  let feedback = create_feedback_box d in
  let textbox = Html.createTextarea d in
  textbox##rows <- 30; textbox##cols <- 80; 
  Dom.appendChild main (d##createTextNode(js "Banco de dados:"));
  Dom.appendChild main (Html.createBr d);
  Dom.appendChild main textbox;
  Dom.appendChild main (Html.createBr d);
  let query = Js.Opt.get (d##getElementById(js "query")) (fun () -> assert false) in
  Dom.appendChild query (button "Carregar dados"
                                (fun _ -> 
                                 try 
                                   base := Proplog.parse_program (Js.to_string textbox##value);
                                   show_msg "Dados carregados com sucesso." feedback;
                                   Js._false
                                 with Proplog.Parse_error msg -> show_error msg feedback;
                                                                 base := [];
                                                                 Js._false));
  Dom.appendChild query (Html.createBr d);
  Dom.appendChild query (Html.createBr d);
  let resp = create_response_box d in
  Dom.appendChild query (text_input "Consulta   ?-" q_text);
  Dom.appendChild query (button "Realizar consulta"
                                (fun _ ->
                                 query_results !q_text resp;
                                 Js._false));
  Dom.appendChild query (Html.createBr d);
  Dom.appendChild query (Html.createBr d);
  Js._false

let _ = 
  Html.window##onload <- Html.handler onload
