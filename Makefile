TYPECONV=`ocamlfind query type_conv`
SEXPLIB=`ocamlfind query sexplib`
all:
	ocamlfind ocamlopt -g -annot -pp "camlp4o -I $(TYPECONV)\
		-I $(SEXPLIB) pa_type_conv.cma pa_sexp_conv.cma" -o jcl  \
		-linkpkg -package batteries -package camlzip -package javalib \
		-package sexplib jcl.ml
	javac mypackage/*.java
clean:
	@rm -f mypackage/*.class mypackage/*.jar JavaInstrument/*.class \
		agent.jar
	@rm -rf _build *.cmi *.cmx *.o *.cmo *.annot *.exe *.class *.json *.out *.MF

buildtest:
	javac JavaInstrument/*.java
	echo Premain-Class: JavaInstrument.ObjectSize > MANIFEST.MF
	jar cvmf MANIFEST.MF agent.jar JavaInstrument/ObjectSize.class

test:
	java -javaagent:agent.jar $(TARGET)
