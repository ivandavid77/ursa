%%%-------------------------------------------------------------------
%% @doc ursa top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ursa_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    case supervisor:start_link({local, ?SERVER}, ?MODULE, []) of
        {ok, Pid} ->
            start_http_server(),
            add_child(tcp_server),
            {ok, Pid};
        Error ->
            Error
    end.


%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 10
    },
    ChildSpecList = [
        child(storage),
        child(process_list),
        child(ranch_sup),
        child(cowboy_sup)
    ],
    {ok, {RestartFlags, ChildSpecList} }.

%%====================================================================
%% Internal functions
%%====================================================================
start_http_server() ->
    Acceptors = 10,
    Port = get_port(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, ursa, "static/index.html"}},
            {"/websocket", ws_handler, []},
            {"/[...]", cowboy_static, {priv_dir, ursa,  "static", [{mimetypes, cow_mimetypes, web}]}}
        ]}
    ]),
    TransportOpts = [{port, Port}],
    ProtocolOpts = #{env => #{dispatch => Dispatch}},
    cowboy:start_clear(http, Acceptors, TransportOpts, ProtocolOpts).

get_port() ->
    case application:get_env(web_port) of
        {ok, WebPort} -> WebPort;
        undefined -> 8099
    end.

child(Module) ->
    #{id => Module,
        start => {Module, start_link, []},
        restart => permanent,
        shutdown => 2000}.

add_child(Module) ->
    supervisor:start_child(?MODULE, child(Module)).
