PROJECT = gproc_test

include ../dep_morpheus.mk

DEPS += gproc gen_leader morpheus_test_lib

dep_gproc = git https://github.com/uwiger/gproc
dep_gproc_commit = ed37980aa895cd3643e101f36cc2c3bdf1daa7fc
dep_gen_leader = git https://github.com/garret-smith/gen_leader_revival.git
dep_gen_leader_commit = d9689e6e80cd8a437bc207d37cb53290ecd64b35
dep_morpheus_test_lib = cp ../morpheus_test_lib

include ../erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
