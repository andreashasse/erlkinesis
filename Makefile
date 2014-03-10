compile:
	./rebar skip_deps=true compile xref

eunit:
	./rebar skip_deps=true compile verbose=1 eunit

test: eunit

xref:
	./rebar xref skip_deps=true

init_dialyzer:
	dialyzer --apps stdlib kernel erts inets crypto ssl sasl xmerl tools public_key compiler syntax_tools -r deps --build_plt --output_plt .dialyzer.plt

check: compile
	dialyzer --no_native -Wno_undefined_callbacks -Wno_return -r ebin --plt .dialyzer.plt

all: get_deps update_deps compile_deps compile

update_deps:
	./rebar update-deps

get_deps:
	./rebar get-deps -j 1

compile_deps:
	./rebar compile

clean:
	rm -rf .eunit
	./rebar clean delete-deps
