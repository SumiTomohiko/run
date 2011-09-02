
SRC_DIR = src
TEST_DIR = tests

all clean:
	for d in $(SRC_DIR) $(TEST_DIR); do (cd $$d && make $@); done

test:
	cd $(TEST_DIR) && ./run_tests

# vim: tabstop=8 shiftwidth=8 noexpandtab
