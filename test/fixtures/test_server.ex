defmodule TestServer do
  @moduledoc """
  A supervisor for the WebsocketHandler
  """

  def start() do
    dispatch =
      :cowboy_router.compile([
        {:_,
         [
           {~c"/", WebsocketHandler, []},
           {~c"/http_get", HttpHandler, []},
           {~c"/forbidden", ForbiddenHandler, []}
         ]}
      ])

    {:ok, _} =
      :cowboy.start_clear(:http, [port: 7070], %{
        env: %{dispatch: dispatch},
        enable_connect_protocol: true
      })
  end
end
