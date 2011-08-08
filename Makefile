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

UNITS= -toplevel run

.SUFFIXES :
.SUFFIXES : .sig .sml .ui .uo

all: run

run: run.uo
	mosmlc -o run run.uo

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
