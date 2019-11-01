PROJECT = mnesia_test

include ../dep_morpheus.mk

DEPS += morpheus_test_lib

dep_morpheus_test_lib = cp ../morpheus_test_lib

include ../erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
