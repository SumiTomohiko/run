# Unix Makefile stub for separate compilation with Moscow ML.

MOSMLHOME=/usr/local/libexec/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)
MOSMLC=mosmlc -c
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac

# this variable should list each unit in the program,
# in the correct order, with an indication of the mode (-structure or -toplevel)
# of each unit

UNITS= -toplevel run -toplevel run_test

.SUFFIXES:
.SUFFIXES: .sig .sml .ui .uo

all: run run_test

LINK = mosmlc -standalone -o

run: run.uo Makefile
	$(LINK) $@ run.uo

run_test: run_test.uo Makefile
	$(LINK) $@ run_test.uo

clean:
	rm -f *.ui
	rm -f *.uo
	rm -f Makefile.bak

# these rules are only needed if UNITS is undefined or empty
.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend:
	rm -f Makefile.bak
	mv Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep $(UNITS) >> Makefile

### DO NOT DELETE THIS LINE
run.uo: run.sml 
	$(MOSMLC) -toplevel run.sml 
run_test.uo: run_test.sml run.uo 
	$(MOSMLC) -toplevel run.ui run_test.sml 
