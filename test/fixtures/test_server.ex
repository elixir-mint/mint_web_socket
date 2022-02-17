defmodule TestServer do
  @moduledoc """
  A supervisor for the WebsocketHandler
  """

  use Supervisor

  def start_link(_opts) do
    dispatch =
      :cowboy_router.compile([
        {:_,
         [
           {'/', WebsocketHandler, []},
           {'/http_get', HttpHandler, []}
         ]}
      ])

    {:ok, _} =
      :cowboy.start_clear(:http, [port: 7070], %{
        env: %{dispatch: dispatch},
        enable_connect_protocol: true
      })

    Supervisor.start_link([], strategy: :one_for_one)
  end

  def init(_opts), do: {:ok, nil}
end
