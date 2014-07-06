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
      command = "shell:parse",
      arguments = {content, string}
   },
   ets:insert(actor_list, This),
   {ok, undefined}.

handle_call(_Msg, _From, State) ->
   {reply, ok, State}.

handle_cast({do, Msg, WsPid}, State) ->
   io:format("Do ===> ~nMsg: ~p~nWsPid: ~p~n", [Msg, WsPid]),
   {noreply, State};
handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
