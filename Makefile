PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)
SHELL:=/bin/sh
CWD:=$(shell pwd)
EBIN:=$(CWD)/ebin
DEPS:=$(CWD)/deps
CONF:=$(CWD)/tend
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

script:
	@sed 's|LIB_DIR|$(CWD)/apps|' etc/tend.config > tend.config
	@sed 's|BASE_DIR|$(CWD)|' etc/tend.sh > tenderl.sh
	@chmod +x tenderl.sh
	@echo "Script tenderl.sh created. You can also add:"
	@echo "  alias erl=\"erl -pa $(EBIN) -env ERL_LIBS $(DEPS) -config $(CONF) -s tend\""
	@echo "as an alias to 'erl' to always have The Erl Next Door ready."
