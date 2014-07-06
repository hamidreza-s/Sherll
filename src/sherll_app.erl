-module(sherll_app).

-behaviour(application).

-include("../include/records.hrl").

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
   %% initialize an ets table
   ets:new(actor_list, [
      set, 
      public, 
      named_table, 
      {keypos, #actor_list.name}
   ]),

   Dispatch = cowboy_router:compile([
      %% {URIHost, [{URIPath, Handler, Opts}]}
      %% '_' is wildcard, means all URLs 
      %% maps to this handler
      {'_', 
         [
            {"/", cowboy_static, {priv_file, sherll, "static/index.html"}},
            {"/front", sherll_front_handler, []},
            {"/ws", sherll_ws_handler, []},
            {"/[...]", cowboy_static, {
               priv_dir, sherll, "static", [{mimetypes, cow_mimetypes, all}]
            }}
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
