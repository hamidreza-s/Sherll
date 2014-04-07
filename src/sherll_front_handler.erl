-module(sherll_front_handler).
-behaviour(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Opts) ->
	{ok, Req, undefined_state}.

handle(Req, State) ->
	io:format("~nRequest:~p ~n", [Req]),
	{ok, Rep} = cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain">>}
	], <<"Front handler!">>, Req),
	{ok, Rep, State}.

terminate(_Reason, _Req, _State) ->
	ok.
