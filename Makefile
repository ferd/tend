PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
.PHONY: all test clean

REBAR=./rebar

all:
	@$(REBAR) get-deps compile

get-deps:
	@$(REBAR) get-deps

test:
	@$(REBAR) compile ct skip_deps=true 

clean:
	@rm -rf deps/ ebin/ logs/ apps/*

dialyzer:
	@$(REBAR) dialyze
