defmodule Mint.WebSocket.Utils do
  @moduledoc false

  alias Mint.WebSocket.Extension

  @websocket_guid "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

  def random_nonce do
    :crypto.strong_rand_bytes(16) |> Base.encode64()
  end

  def headers({:http1, nonce}, extensions) when is_binary(nonce) do
    [
      {"upgrade", "websocket"},
      {"connection", "upgrade"},
      {"sec-websocket-version", "13"},
      {"sec-websocket-key", nonce},
      {"sec-websocket-extensions", extension_string(extensions)}
    ]
    |> Enum.reject(fn {_k, v} -> v == "" end)
  end

  def headers(:http2, extensions) do
    [
      {"sec-websocket-version", "13"},
      {"sec-websocket-extensions", extension_string(extensions)}
    ]
    |> Enum.reject(fn {_k, v} -> v == "" end)
  end

  @spec check_accept_nonce(binary() | nil, Mint.Types.headers()) ::
          :ok | {:error, :invalid_nonce}
  def check_accept_nonce(nil, _response_headers) do
    {:error, :invalid_nonce}
  end

  def check_accept_nonce(request_nonce, response_headers) do
    with {:ok, response_nonce} <- fetch_header(response_headers, "sec-websocket-accept"),
         true <- valid_accept_nonce?(request_nonce, response_nonce) do
      :ok
    else
      _header_not_found_or_not_valid_nonce ->
        {:error, :invalid_nonce}
    end
  end

  def valid_accept_nonce?(request_nonce, response_nonce) do
    expected_nonce = :crypto.hash(:sha, request_nonce <> @websocket_guid) |> Base.encode64()

    # note that this is not a security measure so we do not need to make this
    # a constant-time equality check
    response_nonce == expected_nonce
  end

  defp fetch_header(headers, key) do
    Enum.find_value(headers, :error, fn
      {^key, value} -> {:ok, value}
      _ -> false
    end)
  end

  def maybe_concat(<<>>, data), do: data
  def maybe_concat(a, b), do: a <> b

  defp extension_string(extensions) when is_list(extensions) do
    Enum.map_join(extensions, ", ", &extension_string/1)
  end

  defp extension_string(%Extension{name: name, params: []}), do: name

  defp extension_string(%Extension{name: name, params: params}) do
    params =
      params
      |> Enum.map(fn {key, value} ->
        if value == "true", do: key, else: "#{key}=#{value}"
      end)

    Enum.join([name | params], "; ")
  end
end
