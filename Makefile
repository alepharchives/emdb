# See LICENSE for licensing information.

MODULE = emdb

DIALYZER = dialyzer
REBAR = rebar

.PHONY: build clean

all: ebin priv build

ebin:
	@mkdir -p $@

priv:
	@mkdir -p $@

build:
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -f *~ */*~ erl_crash.dump
	@rm -rf ebin priv
