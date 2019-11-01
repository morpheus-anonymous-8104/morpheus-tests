PROJECT = locks_test

include ../dep_morpheus.mk

DEPS += locks morpheus_test_lib
dep_locks = git https://github.com/uwiger/locks
dep_locks_commit = 8e9b2e33deec6ed375a38fc88f67eff792b91504
dep_morpheus_test_lib = cp ../morpheus_test_lib

include ../erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
