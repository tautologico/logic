all: proplog_web.js

proplog_web.js: proplog_web.byte
	js_of_ocaml proplog_web.byte

proplog_web.byte: proplog.ml webui.ml
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml.syntax -syntax camlp4o -linkpkg -o proplog_web.byte proplog.ml webui.ml

clean:
	rm -f *.cm*
	rm -f proplog_web.*

