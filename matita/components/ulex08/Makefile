VERSION=0.8

all: ulexing.cma pa_ulex.cma
opt: ulexing.cma ulexing.cmxa pa_ulex.cma
depend:
depend.opt:

DESTDIR=
install: all
	ocamlfind install -destdir $(DESTDIR) ulex08 META $(wildcard *.mli) $(wildcard *.cmi) $(wildcard *.a) $(wildcard *.cma) $(wildcard *.cmxa)

uninstall:
	ocamlfind remove ulex

ULEXING = utf8.mli utf8.ml ulexing.mli ulexing.ml utf16.mli utf16.ml
ULEX = cset.ml ulex.mli ulex.ml pa_ulex.ml

ulexing.cma: $(ULEXING)
	ocamlc -a -o ulexing.cma $(ULEXING)
ulexing.cmxa: $(ULEXING)
	ocamlopt -a -o ulexing.cmxa $(ULEXING)

pa_ulex.cma: $(ULEX)
	ocamlc -a -o pa_ulex.cma -pp 'camlp5o pa_macro.cmo pa_extend.cmo q_MLast.cmo' -I +camlp5 -I `ocamlfind -query camlp5` $(ULEX)

pa_ulex.ml: pa_ulex.ml.src
	ocaml mk_pa_ulex.ml

clean:
	rm -f *.cm* *~ test custom_ulexing *.o *.a *.html *.css pa_ulex.ml

view_test: pa_ulex.cma
	camlp5o ./pa_ulex.cma pr_o.cmo -sep "\n" test.ml

run_test: ulexing.cma pa_ulex.cma
	ocamlc -o test -pp 'camlp5o ./pa_ulex.cma' ulexing.cma test.ml
	./test

custom_ulexing: ulexing.cma pa_ulex.cma
	ocamlc -o custom_ulexing -pp 'camlp5o ./pa_ulex.cma' ulexing.cma custom_ulexing.ml


doc:
	ocamldoc -html ulexing.mli

PACKAGE = ulex-$(VERSION)
DISTRIB = CHANGES LICENSE META README Makefile *.ml *.mli *.ml.src
.PHONY: package
package: clean
	rm -Rf $(PACKAGE)
	mkdir $(PACKAGE)
	cp -R $(DISTRIB) $(PACKAGE)/
	tar czf $(PACKAGE).tar.gz $(PACKAGE)
	rm -Rf $(PACKAGE)
