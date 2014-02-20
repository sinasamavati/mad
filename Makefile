.PHONY: all build test

PROJECT = mad
CT_SUITES = mad_utils mad_deps mad_compile

DEPS = getopt uri
dep_getopt = git://github.com/jcomellas/getopt.git v0.8.2
dep_uri = git://github.com/heroku/uri.git master

all: deps app build

build:
	escript build

test: tests

include erlang.mk
