TYPECONV=`ocamlfind query type_conv`
SEXPLIB=`ocamlfind query sexplib`
all:
	ocamlfind ocamlopt -g -annot -pp "camlp4o -I $(TYPECONV)\
		-I $(SEXPLIB) pa_type_conv.cma pa_sexp_conv.cma" \
		-o jcl -syntax batteries.syntax \
		-linkpkg -package batteries -package camlzip -package javalib \
		-package sexplib jcl.ml
	javac mypackage/*.java
clean:
	@rm -f mypackage/*.class mypackage/*.jar
	@rm -rf _build *.cmi *.cmx *.o *.cmo *.annot *.exe *.class *.json *.out

