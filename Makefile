MALFOYC=./dist/build/malfoyc/malfoyc

SAMPLES=$(wildcard samples/*.mf)

GRAPHS=$(patsubst samples/%.mf, build/%.pdf, $(SAMPLES))
ASSEMBLIES=$(patsubst samples/%.mf, build/%.ds, $(SAMPLES))

.PHONY : all clean samples graphs

all : $(MALFOYC) samples

clean :
	rm -r build

samples : $(ASSEMBLIES)

graphs : $(GRAPHS)

$(MALFOYC) :
	cabal configure && cabal build

build/%.ds : samples/%.mf build $(MALFOYC)
	$(MALFOYC) $< > $@

build/%.pdf : samples/%.mf build $(MALFOYC)
	$(MALFOYC) --graphviz $< | dot -Tpdf -o $@

build :
	mkdir build

