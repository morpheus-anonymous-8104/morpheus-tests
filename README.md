# Morpheus paper submission - anonymous repository for test code

## Tests code

All tests are in `*_test/test/`, for example, `locks_test/test/locks_1.erl`.

## Runing the tests

Currently the tests do not run since Morpheus is not anonymized yet.
We will update the repository once that is done.

Assume working dir is the repository root.
The command for each test is:

- `locks-1`: `make -C locks_test tests t=locks_1`
- `locks-2`: `make -C locks_test tests t=locks_2`
- `gproc-1`: `TESTCASE="t_simple_reg" make -C gproc_test tests`
- `gproc-2`: `TESTCASE="t_simple_ensure_other" make -C gproc_test tests`
- `gproc-3`: `TESTCASE="t_master_dies" make -C gproc_test tests`
- `mnesia-1`: `TESTCASE="add_copy_and_restart" make -C mnesia_test tests`
- `mnesia-2`: `TESTCASE="del_copy_and_restart" make -C mnesia_test tests`
- `ms-1`: `make -C rabbit_test tests`
- `ra-1`: `TESTCASE="badkey_previous_cluster" make -C ra_test tests`
- `ra-1`: `TESTCASE="inconsistent_state" make -C ra_test tests`
- `ra-1`: `TESTCASE="inconsistent_state_2" make -C ra_test tests`
