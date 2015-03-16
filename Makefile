ERL_LIB_DIR = $(shell erl -eval 'io:format(code:lib_dir()), init:stop().' -noshell)
PLT_APPS = $(shell ls $(ERL_LIB_DIR) | grep -v interface | grep -v tsung | sed -e 's/-[0-9.]*//')
DIALYZER_OPTS = -Wno_undefined_callbacks --fullpath
ERL_FLAGS = "-config test.config"
REBAR = ./rebar
.PHONY: all build_plt compile compile-fast doc clean depclean distclean dialyze nuke test build-travis dialyzer-travis

all: compile

build_plt:
	@dialyzer --build_plt --apps $(PLT_APPS) ; \
		if [ $$? -eq 2 ] ; then \
		echo "Ignoring build_plt warnings" ; \
		elif [ $$? -ne 0 ] ; then \
		echo "Error during build_plt" ; exit 1 ; \
		fi

compile: deps
	@$(REBAR) compile

compile-fast:
	@$(REBAR) skip_deps=true compile

deps:
	@$(REBAR) get-deps update-deps

doc:
	@$(REBAR) skip_deps=true doc

clean:
	@$(REBAR) skip_deps=true clean

depclean:
	@$(REBAR) clean

distclean:
	@$(REBAR) clean delete-deps
	@rm -rf logs

dialyze:
	@dialyzer $(DIALYZER_OPTS) -r ebin

nuke:
	@rm -rf deps
	@git clean -f -d

test:
	@ERL_FLAGS=$(ERL_FLAGS) $(REBAR) skip_deps=true ct verbose=1

build-travis:
	${MAKE} nuke
	${MAKE} deps
	${MAKE} compile
	${MAKE} test
	${MAKE} dialyzer-travis
	${MAKE} dialyze

dialyzer-travis:
	@curl -L "https://github.com/esl/erlang-plts/blob/master/plts/travis-erlang-r16b02.plt?raw=true" -o /home/travis/.dialyzer_plt
