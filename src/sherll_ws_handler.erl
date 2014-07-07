-module(sherll_ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, undefined}.

websocket_handle({text, Msg}, Req, State) ->
   %% pass to ws dispatcher
   gen_server:cast(sherll_ws_dispatcher, 
      {inbound_frame, self(), Msg}
   ),
   {ok, Req, State};
websocket_handle(_Any, Req, State) ->
	{ok, Req, State}.

websocket_info({outbound_frame, Msg}, Req, State) ->
   {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
