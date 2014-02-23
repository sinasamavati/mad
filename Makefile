.PHONY: all build test

PROJECT = mad

DEPS = getopt uri
dep_getopt = git://github.com/jcomellas/getopt.git v0.8.2
dep_uri = git://github.com/heroku/uri.git master

all: deps build

app-file:
	@mkdir -p ebin
	$(eval MODULES := $(shell find ebin -type f -name \*.beam \
		| sed 's/ebin\///;s/\.beam/,/' | sed '$$s/.$$//'))
	$(appsrc_verbose) cat src/$(PROJECT).app.src \
		| sed 's/{modules,[[:space:]]*\[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app

build: app-file
	escript build

define run_eunit
	$(gen_verbose) erl -noshell -pa ebin $(DEPS_DIR)/*/ebin test -eval ' \
		case eunit:test({dir, "test"}, [verbose]) of \
			error -> halt(1); \
			_ -> halt(0) \
		end'
endef

eunit:
	$(call run_eunit)

build-tests: ERLC_OPTS += -DTEST_DIR='"$(CURDIR)/test"' -Itest

test: clean deps build build-tests eunit
	./mad --version
	$(gen_verbose) rm -rf ebin test/*.beam mad

clean: $(shell rm -f mad)

include erlang.mk
