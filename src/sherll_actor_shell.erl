-module(sherll_actor_shell).
-behaviour(gen_server).

-include("../include/records.hrl").

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
   %% register in actor list table
   This = #actor_list{
      name = shell,
      module = sherll_actor_shell,
      command = <<"shell:parse">>,
      arguments = "2 + 2 ."
   },
   ets:insert(actor_list, This),
   Binding = erl_eval:new_bindings(),
   {ok, Binding}.

handle_call(_Msg, _From, State) ->
   {reply, ok, State}.

handle_cast({do, TupleMsg, WebSocketPid}, State) ->

   %% @todo: check if TupleMsg has arguments

   Binding = State,
   Key = <<"arguments">>,
   {Key, ArgumentsBin} = lists:keyfind(Key, 1, TupleMsg),
   ArgumentsStr = binary_to_list(ArgumentsBin),
   io:format("arguments: ~p~n", [ArgumentsStr]),
   {ok, Tokens, _} = erl_scan:string(ArgumentsStr, 0),
   {ok, Exprs} = erl_parse:parse_exprs(Tokens),
   {value, Result, NewBinding} = erl_eval:exprs(Exprs, Binding),
   Response = lists:flatten(io_lib:format("~p", [Result])),

   %% @todo: also send shell io to WebSocket

   io:format("request: ~p~n", [TupleMsg]),
   io:format("result: ~p~n", [Result]),
   io:format("response: ~p~n", [Response]),

   WebSocketPid ! {outbound_frame, Response},
   {noreply, NewBinding};
handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
