LOG = log.cmx
LIB = $(LOG)

all: buildtest $(LIB)
	ocamlfind ocamlopt -g -annot -o jcl  \
		-linkpkg -package batteries -package camlzip -package javalib \
		-package sawja $(LIB) jcl.ml
	javac mypackage/*.java

$(LOG): 
	ocamlfind ocamlopt -c -package batteries log.ml

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
