.PHONY: run compile

run: compile
	@echo "====================>>> run:"
	erl -pa ../sherll/ebin ../sherll/deps/*/ebin -s sherll

compile:
	@echo "====================>> compile:"
	rebar compile
