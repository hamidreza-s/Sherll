-module(sherll_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
		application:start(crypto),
		application:start(ranch),
		application:start(cowlib),
		application:start(cowboy),
		Dispatch = cowboy_router:compile([
			%% {URIHost, [{URIPath, Handler, Opts}]}
			%% '_' is wildcard, means all URLs 
			%% maps to this handler
			{'_', 
				[
					{"/front", sherll_front_handler, []},
					{"/about", sherll_about_handler, []}
				]
			}
		]),
		%% Name, NumOFAcceptors, TransOpts, ProtoOpts
		cowboy:start_http(sherll_http_listener, 100,
			[{port, 8080}],
			[{env, [{dispatch, Dispatch}]}]
		),
    sherll_sup:start_link().

stop(_State) ->
    ok.
