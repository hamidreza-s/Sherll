.PHONY: run compile deps

run: compile
	@echo "====================>>> run:"
	erl -pa ../sherll/ebin ../sherll/deps/*/ebin -s sherll

compile: deps
	@echo "====================>> compile:"
	./rebar compile

deps:
	@echo "====================>> get deps:"
	./rebar get-deps
