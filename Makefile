REPO        ?= rpg_battlemap

.PHONY: rel deps

all: deps compile

compile:
	./rebar compile

deps:
	./rebar get-deps

clean: testclean
	./rebar clean

distclean: clean devclean relclean ballclean
	./rebar delete-deps


TEST_LOG_FILE := eunit.log
testclean:
	@rm -f $(TEST_LOG_FILE)

eunit: clean deps compile
	./rebar eunit skip_deps=true

# Test each dependency individually in its own VM
test: deps compile testclean
	./rebar eunit skip_deps=true

script: compile
	./rebar escriptize skip_deps=true

rel: deps 
	./rebar compile generate

relclean:
	rm -rf rel/$(REPO)

stage: rel
	rm -rf rel/$(REPO)/lib/$(REPO)*; ln -sf $(abspath app/$(REPO)) rel/$(REPO)/lib/rpg_battlemap-0.1.0;
