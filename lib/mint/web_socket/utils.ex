defmodule Mint.WebSocket.Utils do
  @moduledoc false

  @websocket_guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  def random_nonce do
    :crypto.strong_rand_bytes(16) |> Base.encode64()
  end

  def headers(nonce) when is_binary(nonce) do
    [
      {"upgrade", "websocket"},
      {"connection", "upgrade"},
      {"sec-websocket-version", "13"},
      {"sec-websocket-key", nonce}
      # {"sec-websocket-extensions", "permessage-deflate"}
    ]
  end

  @spec check_accept_nonce(Mint.Types.headers(), Mint.Types.headers()) ::
          :ok | {:error, :invalid_nonce}
  def check_accept_nonce(request_headers, response_headers) do
    with {:ok, request_nonce} <- fetch_header(request_headers, "sec-websocket-key"),
         {:ok, response_nonce} <- fetch_header(response_headers, "sec-websocket-accept"),
         true <- valid_accept_nonce?(request_nonce, response_nonce) do
      :ok
    else
      _header_not_found_or_not_valid_nonce ->
        {:error, :invalid_nonce}
    end
  end

  defp fetch_header(headers, key) do
    Enum.find_value(headers, :error, fn
      {^key, value} -> {:ok, value}
      _ -> false
    end)
  end

  def valid_accept_nonce?(request_nonce, response_nonce) do
    expected_nonce = :crypto.hash(:sha, request_nonce <> @websocket_guid) |> Base.encode64()

    # note that this is not a security measure so we do not need to make this
    # a constant-time equality check
    response_nonce == expected_nonce
  end
end
