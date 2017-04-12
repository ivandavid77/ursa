-module(storage).
-export([start_link/0, update_record/1, get_record/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-behavior(gen_server).

-include_lib("ursa_defs.hrl").
-define(SERVER, ?MODULE).
-define(DBNAME, storagedb).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

update_record(Record) when is_record(Record, bus_record) ->
    gen_server:cast(?SERVER, {update_record, Record}).

get_record(Id) ->
    gen_server:call(?SERVER, {get_record, Id}).

init(_Args) ->
    process_flag(trap_exit, true),
    Opts = [{auto_save, infinity},
            {repair, true},
            {type, set},
            {ram_file, true},
            {keypos, #bus_record.id}],
    case dets:open_file(?DBNAME, Opts) of
        {ok, ?DBNAME} ->
            case dets:lookup(?DBNAME, "10.0.1") of
                [] -> dets:insert(?DBNAME, #bus_record{id="10.0.1",unit="070",route="ROJO-1",plate="380-996",url="https://n180.meraki.com/Mexibus-CamDemo/n/p6fkvc0c/manage/video/video_wall/664280945037148260"});
                _ -> ok
            end,
            case dets:lookup(?DBNAME, "10.0.2") of
                [] -> dets:insert(?DBNAME, #bus_record{id="10.0.2",unit="367",route="VERDE-2",plate="490-100",url="google.com"});
                _ -> ok
            end,
            case dets:lookup(?DBNAME, "10.0.3") of
                [] -> dets:insert(?DBNAME, #bus_record{id="10.0.3",unit="1033-B",route="MORADO-1",plate="",url="facebook.com"});
                _ -> ok
            end,
            dets:sync(?DBNAME),
            {ok, {}};
        {error, Reason} -> {stop, Reason}
    end.

handle_cast({update_record, Record}, State) ->
    dets:insert(?DBNAME, Record),
    dets:sync(?DBNAME),
    {noreply, State}.

handle_call({get_record, Id}, _From, State) ->
    {reply, dets:lookup(?DBNAME, Id), State}.

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {stop, normal, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    dets:close(?DBNAME).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
