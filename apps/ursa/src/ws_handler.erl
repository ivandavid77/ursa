-module(ws_handler).
-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, websocket_terminate/2]).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_init(State) ->
    process_list:register(self()),
    {ok, State}.

websocket_handle({text, _}, State) ->
    {reply, {text, <<"pong!">>}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({broadcasted, Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.

websocket_terminate(_Reason, _State) ->
    process_list:unregister(self()).
