TYPECONV=`ocamlfind query type_conv`
SEXPLIB=`ocamlfind query sexplib`
all:
	ocamlfind ocamlopt -annot -pp "camlp4o -I $(TYPECONV)\
		-I $(SEXPLIB) pa_type_conv.cma pa_sexp_conv.cma" \
		-o jcl -syntax batteries.syntax \
		-linkpkg -package batteries -package camlzip -package javalib \
		-package sexplib jcl.ml
	javac mypackage/*.java
clean:
	-rm mypackage/*.class
	-rm -r _build *.cmi *.cmx *.o *.cmo *.annot *.exe *.class

