# To compile jsmain.ml to javascript, run
# 	make all
#
# To clean the make files, run
# 	make clean
#
# To rebuild the dependency graph, run
#	make depend

# Flags 
YACC = menhir --explain --strict --reference-graph --infer
COMPILEFLAGS = -g

# File names
INCLUDE = 
DEPEND += lexer.ml parser.ml
OBJECTS = err.cmo cc.cmo parser.cmo lexer.cmo 

all: $(DEPEND) $(OBJECTS)
	ocamlfind ocamlc -package js_of_ocaml -package js_of_ocaml-ppx -linkpkg -o jsmain.byte $(OBJECTS) jsmain.ml
	js_of_ocaml jsmain.byte

# Include an automatically generated list of dependencies between source files
include .depend

# Compile an ML module interface
%.cmi : %.mli
	ocamlc $(COMPILEFLAGS) -c $<

# Compile an ML module implementation
%.cmo : %.ml
	ocamlc $(COMPILEFLAGS) -c $<

# Generate ML files from a lexer definition file
%.ml %.mli: %.mll
	@rm -f $@
	ocamllex $<
	@chmod -w $@

# Generate ML files from a parser definition file
parser.ml parser.mli: parser.mly
	@rm -f parser.ml parser.mli
	$(YACC) -v parser.mly
	@chmod -w parser.ml parser.mli
	
# Rebuild intermodule dependencies
depend:: $(DEPEND)
	ocamldep $(INCLUDE) *.mli *.ml > .depend

clean:: 
	rm -rf jsmain.js jsmain.byte *.cmi *.cmo lexer.ml parser.ml parser.mli \
	   parser.output parser.automaton parser.conflicts parser.dot

