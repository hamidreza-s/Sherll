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
   %% State structure:
   %% [{sessions, [{WsPid, SessionPid, Binding}]}]
   State = [{sessions,[]}],
   {ok, State}.

handle_call({state_change, {WsPid, SesPid, SesBnd}}, _From, State) ->
   NewState = update_state(WsPid, SesPid, SesBnd, State),
   {reply, ok, NewState};
handle_call(_Msg, _From, State) ->
   {reply, ok, State}.

handle_cast({do, TupleMsg, WsPid}, State) ->
   [{sessions, Sessions}] = State,
   Result = case lists:keyfind(WsPid, 1, Sessions) of
      {OldWsPid, OldSesPid, OldSesBnd} ->
         {OldWsPid, OldSesPid, OldSesBnd};
      false -> 
         session_create(WsPid)
   end,

   {WsPid, SesPid, SesBnd} = Result,
   SesPid ! {self(), WsPid, SesBnd, TupleMsg},

   NewState = update_state(WsPid, SesPid, SesBnd, State),
   
   {noreply, NewState};
handle_cast(_Msg, State) ->
   {noreply, State}.

handle_info(_Msg, State) ->
   {noreply, State}.

terminate(_Reason, _State) ->
   ok.

code_change(_OldVsn, State, _Extra) ->
   {ok, State}.

%% =================
%% === internals ===
%% =================

session_create(WsPid) ->
   SesIO = spawn(fun() -> session_io() end),
   SesPid = spawn(fun() -> session_loop(WsPid, SesIO) end),
   SesBnd = erl_eval:new_bindings(),
   {WsPid, SesPid, SesBnd}.

session_io() ->
   receive
      {io_request, From, ReplyAs, Request} ->
         {put_chars, _Encoding, Module, Function, Args} = Request,
         Response = apply(Module, Function, Args),
         From ! {io_reply, ReplyAs, Response}
   end,
   session_io().

session_loop(WsPid, SesIO) ->
   %% @todo: terminate when wsPid closes
   %% @todo: store gen_server's state in ets
   group_leader(SesIO, self()),
   receive
      {ShellPid, WsPid, SesBnd, TupleMsg} ->
         {Res, NewSesBnd} = session_eval(SesBnd, TupleMsg),
         MsgPack = {WsPid, self(), NewSesBnd},
         ok = gen_server:call(ShellPid, {state_change, MsgPack}),
         WsPid ! {outbound_frame, Res}
   end,
   session_loop(WsPid, SesIO).

session_eval(SesBnd, TupleMsg) ->
   Binding = SesBnd,
   Key = <<"arguments">>,
   {Key, ArgumentsBin} = lists:keyfind(Key, 1, TupleMsg),
   ArgumentsStr = binary_to_list(ArgumentsBin),
   {ok, Tokens, _} = erl_scan:string(ArgumentsStr, 0),

   %% handle logic error
   {ResponseFinal, NewBindingFinal} = try
      
      %% handle syntax error
      case erl_parse:parse_exprs(Tokens) of
         {ok, Exprs} ->
            {value, Result, NewBinding} = erl_eval:exprs(Exprs, Binding),
            Response = term_to_list(Result),
            {Response, NewBinding};
         {error, ErrInfo} ->
            {_Line, _Module, Description} = ErrInfo,
            Response = format_err_description(Description),
            NewBinding = Binding,
            {Response, NewBinding}
      end

   catch

      Type:Exception ->
         {term_to_list({Type, Exception}), Binding}

   end,
   {ResponseFinal, NewBindingFinal}. 

update_state(WsPid, SesPid, SesBnd, State) ->
   [{sessions, Sessions}] = State,
   ChangedData = {WsPid, SesPid, SesBnd},
   NewSessions = lists:keystore(WsPid, 1, Sessions, ChangedData),
   NewState = lists:keystore(sessions, 1, State, {sessions, NewSessions}),
   NewState.

term_to_list(Term) ->
   lists:flatten(io_lib:format("~p", [Term])).

format_err_description([String, Argument]) ->
   lists:flatten(io_lib:format("~s~s", [String, Argument])).
