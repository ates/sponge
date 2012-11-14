REBAR = ./rebar
DEBUG = true
PROFILE = false
LIBS = ERL_LIBS=apps:deps

.PHONY: deps

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean

test:
	@$(REBAR) xref ct skip_deps=true

release: test
	@$(REBAR) generate

run:
	@$(REBAR) compile skip_deps=true
	test -d log/sasl || mkdir -p log/sasl
	$(LIBS) erl -boot start_sasl \
		-config rel/files/sys.config \
		-args_file rel/files/vm.args \
		-sponge profile $(PROFILE) \
		-sponge debug $(DEBUG) \
		-s sponge
