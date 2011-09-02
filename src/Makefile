
PKG = run

all: $(PKG)

$(PKG): Main.uo
	$(LINK) $@ Main.uo

Main.uo: Try.ui Main.sml
	$(COMPILE) $@ Try.ui Main.sml

Try.ui Try.uo: Try.sml
	$(COMPILE) Try.uo Try.sml

clean:
	make clean-common
	$(RM) $(PKG)

include ../Makefile.common

# vim: tabstop=8 shiftwidth=8 noexpandtab
