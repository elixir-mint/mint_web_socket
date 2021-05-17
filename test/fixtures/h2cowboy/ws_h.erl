-module(ws_h).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

init(Req, Opts) ->
  {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
  {[], State}.

websocket_handle({text, Msg}, State) ->
  {[{text, Msg}], State};
websocket_handle(_Data, State) ->
  {[], State}.

websocket_info(_Info, State) ->
  {[], State}.
