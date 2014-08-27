.PHONY: all deps compile clean distclean

REBAR = rebar

all: deps compile

deps:
	$(REBAR) get-deps

compile: deps
	$(REBAR) compile

clean:
	$(REBAR) clean
	rm -rf ebin

distclean: clean
	rm -rf deps
