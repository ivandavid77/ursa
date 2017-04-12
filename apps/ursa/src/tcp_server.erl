-module(tcp_server).
-export([start_link/0]).
-export([attend_connection/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behavior(gen_server).

-include_lib("ursa_defs.hrl").
-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    Port = get_port(),
    process_flag(trap_exit, true),
    Opts = [{active, false},
            {keepalive, true}],
    case gen_tcp:listen(Port, Opts) of
        {ok, LSock} -> {ok, #{lsock=>LSock}, 0};
        {error, Reason} -> {stop, Reason}
    end.

get_port() ->
    case application:get_env(tcp_port) of
        {ok, TCPPort} -> TCPPort;
        undefined -> 55056
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State, 0}.

handle_cast(_Msg, State) ->
    {noreply, State, 0}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {stop, normal, State};
handle_info(timeout, #{lsock:=LSock} = State) ->
    {ok, Socket} = gen_tcp:accept(LSock),
    Pid = spawn(?MODULE, attend_connection, [Socket]),
    gen_tcp:controlling_process(Socket, Pid),
    {noreply, State, 0}.
  
attend_connection(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Packet} ->
            handle_packet(Packet),
            attend_connection(Socket);
        {tcp_error, Socket, _Reason} -> ok;
        {tcp_closed, Socket} -> ok
    end.

get_network(Ip) ->
    [Oct1,Oct2,Oct3,_Oct4] = string:tokens(Ip, "."),
    string:join([Oct1,Oct2,Oct3],".").

handle_packet(Packet) ->
    {struct, Urbo} = mochijson:decode(Packet),
    {value, {"ip", Ip}} = lists:keysearch("ip", 1, Urbo),
    {value, {"ok", Ok}} = lists:keysearch("ok", 1, Urbo),
    {value, {"gps", {array,[Lat, Lng]}}} = lists:keysearch("gps",1, Urbo),
    {value, {"dt", Dt}} = lists:keysearch("dt", 1, Urbo),
    [Record] = storage:get_record(get_network(Ip)),
    Msg = lists:flatten(io_lib:format(
        "{\"unit\":\"~s\",\"route\":\"~s\",\"plate\":\"~s\",\"url\":\"~s\",\"ok\":~B,\"lat\":~f,\"lng\":~f,\"dt\":\"~s\"}",[
            Record#bus_record.unit,
            Record#bus_record.route,
            Record#bus_record.plate,
            Record#bus_record.url,
            Ok,
            Lat,
            Lng,
            Dt
        ]
    )),
    process_list:broadcast(Msg).

terminate(_Reason, #{lsock:=LSock}) ->
    gen_tcp:close(LSock).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
