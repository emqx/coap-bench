##

REBAR = rebar3
all: compile

compile:
	$(REBAR) compile

ct: compile
	$(REBAR) as test ct -v

eunit: compile
	$(REBAR) as test eunit

rel:
	$(REBAR) release

xref:
	$(REBAR) xref

clean:
	$(REBAR) clean

distclean:
	@rm -rf _build
	@rm -f data/app.*.config data/vm.*.args rebar.lock
