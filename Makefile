.PHONY: all build test

PROJECT = mad
CT_SUITES = mad_utils mad_deps mad_compile

DEPS = getopt uri
dep_getopt = git://github.com/jcomellas/getopt.git v0.8.2
dep_uri = git://github.com/heroku/uri.git master

all: deps app-file build

app-file:
	@mkdir -p ebin
	$(eval MODULES := $(shell find ebin -type f -name \*.beam \
		| sed 's/ebin\///;s/\.beam/,/' | sed '$$s/.$$//'))
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed 's/{modules,[[:space:]]*\[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app

build:
	@mkdir -p ebin
	escript build

rm-beams:
	rm ebin/*.beam

test: tests rm-beams build
	./mad --version

include erlang.mk
