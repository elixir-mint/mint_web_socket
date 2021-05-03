defmodule Mint.WebSocket do
  @moduledoc """
  WebSocket
  """

  alias __MODULE__.Utils

  @type t :: %__MODULE__{}
  defstruct []

  def build_request_headers(_opts \\ []) do
    Utils.random_nonce()
    |> Utils.headers()
  end
end
