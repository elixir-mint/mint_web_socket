%% @private
-module(websocket_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", ws_h, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 7070}], #{
    env => #{dispatch => Dispatch},
    enable_connect_protocol => true
  }),
  websocket_sup:start_link().

stop(_State) ->
  ok = cowboy:stop_listener(http).
