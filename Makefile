.PHONY: all clean compile deps doc

REBAR=rebar

clean:
	$(REBAR) clean

clean-deps:
	@rm -rf deps

compile: clean
	$(REBAR) compile

deps:
	$(REBAR) get-deps

run:
	#@erl -pa ebin deps/*/ebin -s eppi -config eppi -s reloader -boot start_sasl +P 2000000
	@erl -pa ebin deps/*/ebin -s eppi -config eppi

compile-and-run: compile run

recompile-and-run: clean-deps deps compile run

run-node: compile
	@erl -sname $(node) -pa ebin deps/*/ebin -s eppi -config eppi -s reloader -boot start_sasl +P 2000000

doc:
	$(REBAR) doc skip_deps=true

all: deps compile
