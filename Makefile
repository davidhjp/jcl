TYPECONV=`ocamlfind query type_conv`
SEXPLIB=`ocamlfind query sexplib`
ARRAYGEN = arraygen.cmxa

all: buildtest $(ARRAYGEN)
	ocamlfind ocamlopt -g -annot -pp "camlp4o -I $(TYPECONV)\
		-I $(SEXPLIB) pa_type_conv.cma pa_sexp_conv.cma" -o jcl  \
		-linkpkg -package batteries -package camlzip -package javalib \
		-package sexplib -package sawja $(ARRAYGEN) jcl.ml
	javac mypackage/*.java

$(ARRAYGEN):
	ocamlfind ocamlopt -a -g -annot -package batteries \
		-package camlzip -package sawja -package javalib \
		jarrays.ml -o $@

clean:
	@rm -f mypackage/*.class mypackage/*.jar JavaInstrument/*.class \
		agent.jar
	@rm -rf _build *.cmi *.cmx *.o *.cmo *.annot *.exe *.class *.json *.out *.MF *.cmxa\
		*.a

buildtest:
	javac JavaInstrument/*.java
	echo Premain-Class: JavaInstrument.ObjectSize > MANIFEST.MF
	jar cvmf MANIFEST.MF agent.jar JavaInstrument/ObjectSize.class

test:
	java -javaagent:agent.jar JavaInstrument.Loader $(TARGET)
