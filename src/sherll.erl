-module(sherll).

-export([start/0]).
-export([stop/0]).

start() ->
	ensure_deps_started(),
	ensure_started(sherll),
	io:format("Server started at 8080~n").

stop() ->
	io:format("Server stoped!~n"),
	cowboy:stop_listener(sherll_listener).

ensure_deps_started() ->
	ensure_started(crypto),
	ensure_started(ranch),
	ensure_started(cowlib),
	ensure_started(cowboy).

ensure_started(App) ->
	case application:start(App) of
		ok -> ok;
		{error, {already_started, App}} -> ok
	end.
