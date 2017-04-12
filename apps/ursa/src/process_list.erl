-module(process_list).
-export([start_link/0, register/1, unregister/1, broadcast/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behavior(gen_server).
-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, #{ws_list=>[]}}.

register(Pid) ->
    gen_server:cast(?SERVER, {register, Pid}).

unregister(Pid) ->
    gen_server:cast(?SERVER, {unregister, Pid}).

broadcast(Msg) ->
    gen_server:cast(?SERVER, {broadcast, Msg}).

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({register, Pid}, #{ws_list:=WS} = State) ->
    {noreply, State#{ws_list:=[Pid|WS]}};
handle_cast({unregister, Pid}, #{ws_list:=WS} = State) ->
    {noreply, State#{ws_list:=lists:delete(Pid, WS)}};
handle_cast({broadcast, Msg}, #{ws_list:=WS} = State) ->  
    lists:map(fun(Pid) -> Pid ! {broadcasted, Msg} end, WS),
    {noreply, State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
