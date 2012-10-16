REBAR = ./rebar
DEBUG = true
PROFILE = false
LIBS = ERL_LIBS=apps:deps

compile:
	@$(REBAR) compile

clean:
	@$(REBAR) clean

test:
	@$(REBAR) eunit xref

release: test
	@$(REBAR) generate

run:
	@$(REBAR) compile skip_deps=true
	test -d log/sasl || mkdir -p log/sasl
	$(LIBS) erl -boot start_sasl \
		-config rel/files/sys.config \
		-args_file rel/files/vm.args \
		-mnesia dir '"/tmp/sponge"' \
		-sponge profile $(PROFILE) \
		-sponge debug $(DEBUG) \
		-s sponge
