
all:
	./rebar compile

deps:
	./rebar get-deps
	./rebar compile

clean:
	./rebar clean

run:
	ERL_LIBS=apps:deps erl +K true -sname serials -s serials_app
