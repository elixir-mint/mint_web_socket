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

  def valid_accept_nonce?(request_nonce, response_nonce) do
    expected_nonce = :crypto.hash(:sha, request_nonce <> @websocket_guid) |> Base.encode64()

    # note that this is not a security measure so we do not need to make this
    # a constant-time equality check
    response_nonce == expected_nonce
  end

  def create_mask, do: :crypto.strong_rand_bytes(4)

  def mask_payload(<<a, b, c, d>>, payload) do
    [a, b, c, d]
    |> Stream.cycle()
    |> Enum.reduce_while({payload, _acc = <<>>}, fn
      _mask_key, {<<>>, acc} ->
        {:halt, acc}

      mask_key, {<<part_key::integer, payload_rest::binary>>, acc} ->
        {:cont, {payload_rest, <<acc::binary, Bitwise.bxor(mask_key, part_key)::integer>>}}
    end)
  end
end
