.PHONY: install clean compile deps doc

REBAR=rebar
config_file=eppi
node_name=eppi@`hostname`
eppi_cookie="96482c6a3670ee336828c1b0d51cdee6"

clean:
	$(REBAR) clean

clean-deps:
	@rm -rf deps

compile:
	$(REBAR) compile

recompile: clean
	$(REBAR) compile

deps:
	$(REBAR) get-deps

doc:
	$(REBAR) doc skip_deps=true

tests:
	$(REBAR) eunit skip_deps=true

install: deps compile

push:
	@git tag `grep vsn src/eppi.app.src  | grep -o -P '\d{1,2}.\d{1,2}.\d{1,3}'`
	@git push --tags origin master

#
# Start a node
#

# make run
# make run config_file=tests/eppi
run:
	@erl -pa ebin deps/*/ebin -s eppi -config ${config_file}

# make run-node node_name=eppi01
# make run-node node_name=eppi02 config_file=tests/eppi
run-node: compile
	@erl -name ${node_name} 		\
		-pa ebin deps/*/ebin 		\
		-s eppi 					\
		-setcookie ${eppi_cookie} 	\
		-config ${config_file}

#
# For testing
#

run-test-cluster-node-1: compile
	@erl -sname eppi1 			\
		-pa ebin deps/*/ebin 	\
		-s eppi 				\
		-setcookie eppi_cookie	\
		-config eppi

run-test-cluster-node-2:
	@rm -rf packages-2
	@erl -sname eppi2 			\
		-pa ebin deps/*/ebin 	\
		-s eppi 				\
		-setcookie eppi_cookie	\
		-eval "eppi:connect('eppi1@kev-nb900')." \
		-config tests/eppi
