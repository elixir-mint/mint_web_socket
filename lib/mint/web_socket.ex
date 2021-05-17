defmodule Mint.WebSocket do
  @moduledoc """
  WebSocket
  """

  require __MODULE__.Frame, as: Frame
  alias __MODULE__.Utils
  alias Mint.WebSocketError

  @type t :: %__MODULE__{}
  defstruct extensions: MapSet.new(),
            fragments: [],
            private: %{},
            buffer: <<>>

  @type error :: Mint.Types.error() | WebSocketError.t()

  defguardp is_frame(frame)
            when frame in [:ping, :pong, :close] or
                   (is_tuple(frame) and elem(frame, 0) in [:text, :binary, :ping, :pong] and
                      is_binary(elem(frame, 1))) or
                   (is_tuple(frame) and elem(frame, 0) == :close and is_integer(elem(frame, 1)) and
                      is_binary(elem(frame, 2)))

  @type frame ::
          {:text, binary()}
          | {:binary, binary()}
          | :ping
          | {:ping, binary()}
          | :pong
          | {:pong, binary()}
          | :close
          | {:close, code :: non_neg_integer(), reason :: binary()}

  @doc """
  Requests that a connection be upgraded to the WebSocket protocol

  This function wraps `Mint.HTTP.request/5` to provide a single interface
  for bootstrapping an upgrade for HTTP/1 and HTTP/2 connections.

  For HTTP/1 connections, this function performs a GET request with
  WebSocket-specific headers. For HTTP/2 connections, this function performs
  an extended CONNECT request which opens a stream to be used for the WebSocket
  connection.
  """
  @spec upgrade(
          conn :: Mint.HTTP.t(),
          path :: String.t(),
          headers :: Mint.Types.headers(),
          # maybe t:Keyword.t/0, will hold extensions in the future
          opts :: list()
        ) :: {:ok, Mint.HTTP.t(), Mint.Types.request_ref()} | {:error, Mint.HTTP.t(), error()}
  def upgrade(conn, path, headers, opts \\ [])

  def upgrade(%Mint.HTTP1{} = conn, path, headers, _opts) do
    nonce = Utils.random_nonce()
    headers = Utils.headers({:http1, nonce}) ++ headers
    conn = put_in(conn.private[:sec_websocket_key], nonce)

    Mint.HTTP.request(conn, "GET", path, headers, nil)
  end

  def upgrade(
        %Mint.HTTP2{server_settings: %{enable_connect_protocol: true}} = conn,
        path,
        headers,
        _opts
      ) do
    headers = [
      {":scheme", conn.scheme},
      {":path", path},
      {":protocol", "websocket"}
      | headers
    ] ++ Utils.headers(:http2)

    Mint.HTTP.request(conn, "CONNECT", path, headers, :stream)
  end

  def upgrade(%Mint.HTTP2{} = conn, _path, _headers, _opts) do
    {:error, conn, %WebSocketError{reason: :extended_connect_disabled}}
  end

  @spec new(Mint.HTTP.t(), reference(), pos_integer(), Mint.Types.headers()) ::
          {:ok, Mint.HTTP.t(), t(), [Mint.Types.response()]} | {:error, Mint.HTTP.t(), error()}
  def new(%Mint.HTTP1{} = conn, _request_ref, status, _response_headers)
      when status != 101 do
    {:error, conn, %WebSocketError{reason: :connection_not_upgraded}}
  end

  def new(%Mint.HTTP1{} = conn, request_ref, _status, response_headers) do
    with :ok <- Utils.check_accept_nonce(conn.private[:sec_websocket_key], response_headers) do
      conn = re_open_request(conn, request_ref)

      {:ok, conn, %__MODULE__{}}
    else
      {:error, reason} -> {:error, conn, reason}
    end
  end

  def new(%Mint.HTTP2{} = conn, _request_ref, status, _response_headers)
      when status in 200..299 do
    {:ok, conn, %__MODULE__{}}
  end

  def new(%Mint.HTTP2{} = conn, _request_ref, _status, _response_headers) do
    {:error, conn, %WebSocketError{reason: :connection_not_upgraded}}
  end

  @spec encode(t(), frame()) :: {:ok, t(), binary()} | {:error, t(), any()}
  def encode(%__MODULE__{} = websocket, frame) when is_frame(frame) do
    case frame |> Frame.translate() |> Frame.encode() do
      {:ok, encoded} -> {:ok, websocket, encoded}
      {:error, reason} -> {:error, websocket, reason}
    end
  end

  @spec decode(t(), data :: binary()) :: {:ok, t(), [frame()]} | {:error, t(), any()}
  defdelegate decode(websocket, data), to: Frame

  # we re-open the request in the conn for HTTP1 connections because a :done
  # will complete the request
  defp re_open_request(%Mint.HTTP1{} = conn, request_ref) do
    request = new_request(request_ref, nil, :stream, :identity)
    %{conn | request: %{request | state: :body}, streaming_request: request}
  end

  defp new_request(ref, method, body, encoding) do
    state =
      if body == :stream do
        {:stream_request, encoding}
      else
        :status
      end

    %{
      ref: ref,
      state: state,
      method: method,
      version: nil,
      status: nil,
      headers_buffer: [],
      data_buffer: [],
      content_length: nil,
      connection: [],
      transfer_encoding: [],
      body: nil
    }
  end
end
