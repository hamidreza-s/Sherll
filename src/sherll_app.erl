-module(sherll_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
		Dispatch = cowboy_router:compile([
			%% {URIHost, [{URIPath, Handler, Opts}]}
			%% '_' is wildcard, means all URLs 
			%% maps to this handler
			{'_', 
				[
               {"/", cowboy_static, {priv_file, sherll, "static/index.html"}},
               {"/assets/[...]", cowboy_static, {
                  priv_dir, sherll, "static/assets", [{mimetypes, cow_mimetypes, all}]
               }},
					{"/front", sherll_front_handler, []},
					{"/ws", sherll_ws_handler, []}
				]
			}
		]),
		%% Name, NumOFAcceptors, TransOpts, ProtoOpts
		cowboy:start_http(sherll_listener, 100,
			[{port, 8080}],
			[{env, [{dispatch, Dispatch}]}]
		),
    sherll_sup:start_link().

stop(_State) ->
    ok.
