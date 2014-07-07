-module(sherll_ws_dispatcher).
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
   {ok, undefined}.

handle_call(_Msg, _From, State) ->
   {reply, ok, State}.

handle_cast({inbound_frame, WsPid, Msg}, State) ->
   MalformedRes = <<"[error: malformed json request]">>,
   UnknownCommand = <<"[error: unknown command]">>,
   %% check if Msg is json
   case jsx:is_json(Msg) of
      false -> 
         WsPid ! {outbound_frame, MalformedRes};
      true ->
         %% find recipient actor
         TupleMsg = jsx:decode(Msg),
         Key = <<"command">>,
         ActorCommand = lists:keyfind(Key, 1, TupleMsg),
         case ActorCommand of
            false -> 
               WsPid ! {outbound_frame, MalformedRes};
            {Key, Command} ->
               ActorRecord = ets:lookup(actor_list, Command),
               case length(ActorRecord) > 0 of
                  false ->
                     WsPid ! {outbound_frame, UnknownCommand};
                  true ->
                     Record = hd(ActorRecord),
                     %% pass Msg and From to recipient actor
                     gen_server:cast(
                        Record#actor_list.module, 
                        {do, TupleMsg, WsPid}
                     ) 
               end;
            _ ->
               WsPid ! {outbound_frame, MalformedRes}
         end
   end,
   {noreply, State};
handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.
