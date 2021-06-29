defmodule WebsocketHandler do
  @moduledoc """
  An example websocket handler for cowboy with compression enabled
  """

  @behaviour :cowboy_websocket

  @impl :cowboy_websocket
  def init(req, state) do
    {:cowboy_websocket, req, state, %{compress: true}}
  end

  @impl :cowboy_websocket
  def websocket_init(state), do: {[], state}

  @impl :cowboy_websocket
  def websocket_handle({:text, msg}, state), do: {[text: msg], state}

  def websocket_handle(_data, state), do: {[], state}

  @impl :cowboy_websocket
  def websocket_info(_info, state), do: {[], state}
end
