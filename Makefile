.PHONY: all clean compile deps doc

REBAR=rebar
config_file=eppi
node_name=eppi

clean:
	$(REBAR) clean

clean-deps:
	@rm -rf deps

compile: clean
	$(REBAR) compile

deps:
	$(REBAR) get-deps

# make run
# make run config_file=tests/eppi
run:
	@erl -pa ebin deps/*/ebin -s eppi -config ${config_file}
	#@erl -pa ebin deps/*/ebin -s eppi -config eppi -s reloader -boot start_sasl +P 2000000

compile-and-run: compile run

recompile-and-run: clean-deps deps compile run

# make run-node node_name=eppi01
# make run-node node_name=eppi02 config_file=tests/eppi
run-node: compile
	@erl -sname ${node_name} 	\
		-pa ebin deps/*/ebin 	\
		-s eppi 				\
		-setcookie eppi_cookie	\
		-config ${config_file}

doc:
	$(REBAR) doc skip_deps=true

tests:
	$(REBAR) eunit skip_deps=true

all: deps compile