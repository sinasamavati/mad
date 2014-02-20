.PHONY: all build test

PROJECT = mad
CT_SUITES = mad_utils mad_deps mad_compile

all: app build

build:
	escript build

test: tests

include erlang.mk
