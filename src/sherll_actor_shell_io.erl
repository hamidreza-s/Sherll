-module(sherll_actor_shell_io).
-behaviour(gen_server).


-export([start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
   {ok, undefined}.

handle_call(_Msg, _From, State) ->
   {reply, ok, State}.

handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info({io_request, From, ReplyAs, Request}, State) ->

   {put_chars, Encoding, Module, Function, Args} = Request,
   
   %% @todo: how to get WebSocketPid? [done]
   %%        with registering it

   %% @todo: why does it wait after sendig? [done]
   %%        becase io_client is waiting for it!

   %% @todo: parse io operation [done]
   Response = apply(Module, Function, Args),
   web_socket_pid ! {outbound_frame, Response},
   From ! {io_reply, ReplyAs, ok},

   {noreply, State};
handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
