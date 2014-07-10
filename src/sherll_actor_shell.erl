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
   %% @todo: also send shell io to WebSocket
   %% @todo: make an env to hold app's root path

   %SelfRef = lists:subtract(pid_to_list(self()), "<..>"),
   %{ok, IoDevice} = file:open("./tmp/" ++ SelfRef, [write]),
   %group_leader(IoDevice, self()),
   %io:format(IoDevice, Response, []),

   Binding = State,
   Key = <<"arguments">>,
   {Key, ArgumentsBin} = lists:keyfind(Key, 1, TupleMsg),
   ArgumentsStr = binary_to_list(ArgumentsBin),
   {ok, Tokens, _} = erl_scan:string(ArgumentsStr, 0),

      {ResponseF, NewBindingF} = case erl_parse:parse_exprs(Tokens) of
         {ok, Exprs} ->
            {value, Result, NewBinding} = erl_eval:exprs(Exprs, Binding),
            Response = term_to_list(Result),
            {Response, NewBinding};
         {error, ErrInfo} ->
            Response = term_to_list(ErrInfo),
            NewBinding = Binding,
            {Response, NewBinding}
      end,

   WebSocketPid ! {outbound_frame, ResponseF},
   {noreply, NewBindingF};
handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

term_to_list(Term) ->
   lists:flatten(io_lib:format("~p", [Term])).
