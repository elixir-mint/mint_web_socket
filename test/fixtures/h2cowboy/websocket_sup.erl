%% @private
-module(websocket_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", ws_h, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 7070}], #{
    env => #{dispatch => Dispatch},
    enable_connect_protocol => true
  }),
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  Procs = [],
  {ok, {{one_for_one, 10, 10}, Procs}}.
