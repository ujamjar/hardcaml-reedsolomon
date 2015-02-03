########################################
# hardcaml - hardware design in OCaml
#
#   (c) 2014 MicroJamJar Ltd
#
# Author(s): andy.ray@ujamjar.com
# Description: 
#
########################################

.PHONY: clean all install uninstall 

BUILD_OPTS=

all: setup.data
	ocaml setup.ml -build

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure

install: all
	ocaml setup.ml -install

uninstall: 
	ocamlfind remove hardcaml-reedsolomon

clean:
	ocaml setup.ml -clean
	- rm -f *.vcd *.vvp
	- find . -name "*~" | xargs rm

distclean: clean
	- rm -f setup.data setup.log
