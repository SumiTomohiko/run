
PKG = run
TEST_DIR = tests

all:
	make $(PKG)
	cd $(TEST_DIR) && make

$(PKG): run.uo Makefile
	$(LINK) $@ run.uo

run.uo: run.sml
	$(COMPILE) $@ run.sml

clean:
	make clean-common
	$(RM) $(PKG)
	cd $(TEST_DIR) && make $@

test:
	cd $(TEST_DIR) && ./run_tests

include Makefile.common

# vim: tabstop=8 shiftwidth=8 noexpandtab
