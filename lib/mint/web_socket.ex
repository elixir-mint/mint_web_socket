defmodule Mint.WebSocket do
  @moduledoc """
  WebSocket
  """

  alias __MODULE__.Utils

  @type t :: %__MODULE__{}
  defstruct []

  @spec build_request_headers(Keyword.t()) :: Mint.Types.headers()
  def build_request_headers(_opts \\ []) do
    Utils.random_nonce()
    |> Utils.headers()
  end

  @spec new(%Mint.HTTP1{} | %Mint.HTTP2{}, pos_integer(), Mint.Types.headers()) ::
          {:ok, %__MODULE__{}} | {:error, any()}
  def new(_conn, status, _headers) when status != 101 do
    {:error, :connection_not_upgrading}
  end

  def new(conn, status, headers) do
  end
end
