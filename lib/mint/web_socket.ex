defmodule Mint.WebSocket do
  @moduledoc """
  WebSocket
  """

  require __MODULE__.Frame, as: Frame
  alias __MODULE__.Utils

  @type t :: %__MODULE__{}
  defstruct extensions: MapSet.new(),
            fragments: [],
            private: %{},
            buffer: <<>>

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

  @spec build_request_headers(Keyword.t()) :: Mint.Types.headers()
  def build_request_headers(_opts \\ []) do
    Utils.random_nonce()
    |> Utils.headers()
  end

  @spec new(Mint.HTTP.t(), reference(), pos_integer(), Mint.Types.headers(), Mint.Types.headers()) ::
          {:ok, Mint.HTTP.t(), t(), [Mint.Types.response()]} | {:error, Mint.HTTP.t(), any()}
  def new(conn, _request_ref, status, _request_headers, _response_headers) when status != 101 do
    {:error, conn, :connection_not_upgraded}
  end

  def new(conn, request_ref, _status, request_headers, response_headers) do
    with :ok <- Utils.check_accept_nonce(request_headers, response_headers) do
      conn = re_open_request(conn, request_ref)

      websocket = %__MODULE__{
        extensions: Utils.common_extensions(request_headers, response_headers)
      }

      {conn, websocket, messages} = messages(conn, websocket)

      {:ok, conn, websocket, messages}
    else
      {:error, reason} -> {:error, conn, reason}
    end
  end

  # translate frame to something more user-friendly
  @spec encode(t(), frame :: tuple()) :: {:ok, t(), binary()} | {:error, t(), any()}
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

  defp messages(%{buffer: <<>>} = conn, websocket), do: {conn, websocket, []}

  defp messages(%{buffer: buffer} = conn, websocket) when is_binary(buffer) do
    case decode(websocket, buffer) do
      {:ok, websocket, messages} ->
        {put_in(conn.buffer, <<>>), websocket, messages}

      error ->
        IO.inspect(error, label: "error in new/5 parse")
        {conn.buffer, websocket, []}
    end
  end
end
