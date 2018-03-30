all: matita/matita/matita

.PHONY: matita/matita/matita

matita/matita/matita: elpi/findlib/elpi/elpi.cmxa matita/Makefile.defs
	$(MAKE) -C matita

matita/Makefile.defs:
	cd matita && autoconf && ./configure

elpi/findlib/elpi/elpi.cmxa:
	git submodule update --init
	$(MAKE) -C elpi

clean: cleandk
	$(MAKE) -C elpi clean
	$(MAKE) -C matita clean
	rm -f time.*

run: matita/matita/matita
	TJPATH=`pwd`/elpi ./matita/matita/matita -elpi FG1 matita/matita/lib/arithmetics/nat.ma

print:
	cd refiner-CSC;\
	../elpi/elpi ../elpi/utils/elpi2mathjx.elpi -exec \
		main -- all_refiner.elpi ../refiner-CSC.html \
		pervasives test_refiner test_kernel list logic

BOUND=0.01
timing: time.logic time.relations time.nat
	@echo OK: `grep OK time.* | wc -l`
	@echo KO: `grep KO time.* | wc -l`
	@echo TIMEOUT: `grep TIME time.* | wc -l`
	@echo Matita: `cut -f 1 time.* | paste -sd+ - | bc`
	@echo ELPI.OK: `cut -f 2 time.* | grep OK | cut -d ' ' -f 2  | paste -sd+ - | bc`
	@echo ELPI.OK.BOUND: `cut -f 2 time.* | grep OK | cut -d ' ' -f 2 | awk '{ if (\$$0 < $(BOUND)) print \$$0}' | paste -sd+ - | bc` on `cut -f 2 time.* | grep OK | cut -d ' ' -f 2 | awk '{ if (\$$0 < $(BOUND)) print \$$0}' | wc -l` of `cat time.* | grep OK | wc -l` samples

time.%:
	rm -rf ~/.elpi/
	./matita/matita/matitac -elpi CSC ./matita/matita/lib/*/$*.ma -elpi-quiet -elpi-maxsteps 200000 > log 2>&1
	grep 'Matita refinement time' log | cut -d : -f 2- > log.matita.$*
	grep 'ELPI refinement time' log | cut -d : -f 2- > log.elpi.$*
	paste log.matita.$* log.elpi.$* > time.$*



# Targets related to Dedukti exports

# Exportable libraries folders
LIBS=basics arithmetics arithmetics/chebyshev

cleandk:
	ls export/*.dk | grep -v cic.dk | xargs rm
	rm export/*.dko

.PHONY : alldks test dkcheck

# Paths to exportable .ma
MAS = $(foreach dir,$(LIBS),$(wildcard matita/matita/lib/$(dir)/*.ma))

# Corresponding targets
TARGETS = $(subst .ma,, $(subst matita/matita/lib/,,$(MAS)))

alldks: $(TARGETS) | dkcheck

test: basics/bool | dkcheck

dkcheck:
	make -C export

define make_targets
$1: ./matita/matita/lib/$1.ma matita/matita/matita export
	echo ./matita/matita/lib/$1.ma
	matita/matita/matitac -elpi-quiet -extract_dedukti ./matita/matita/lib/$1.ma | grep .ma
	mv *.dk export/

$1.ma: $1

$1.dk: $1

endef

$(foreach dir,$(TARGETS),$(eval $(call make_targets,$(dir))))
