all: compile doc

.PHONY: doc

compile doc clean eunit:
	rebar $@
