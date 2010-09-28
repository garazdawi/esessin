all: compile doc

compile doc clean eunit:
	rebar $@
