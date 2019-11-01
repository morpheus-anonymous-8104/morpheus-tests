PROJECT = rabbit_test

include ../dep_morpheus.mk

DEPS += rabbit morpheus_test_lib
dep_rabbit = git https://github.com/rabbitmq/rabbitmq-common.git
dep_rabbit_commit = v3.7.8
dep_morpheus_test_lib = cp ../morpheus_test_lib

include ../erlang.mk

ERLC_OPTS := $(filter-out -Werror,$(ERLC_OPTS))
