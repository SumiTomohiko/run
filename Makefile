
COMPILE = mosmlc -orthodox -c -o
LINK = mosmlc -g -standalone -o
PKG = run run_test

all: $(PKG)

run: run.uo Makefile
	$(LINK) $@ run.uo

run_test: run_test.uo Makefile
	$(LINK) $@ run_test.uo

.SUFFIXES: .sml .uo .sig .ui

.sml.uo:
	$(COMPILE) $@ $<

clean:
	rm -rf *.uo *.ui $(PKG)

test:
	@./run_tests

# vim: tabstop=8 shiftwidth=8 noexpandtab
