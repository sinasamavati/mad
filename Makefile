.PHONY: all build test

PROJECT = mad
CT_SUITES = mad_utils mad_deps mad_compile

DEPS = getopt
dep_getopt = git://github.com/jcomellas/getopt.git v0.8.2

all: deps app build

build:
	escript build

test: tests

include erlang.mk
