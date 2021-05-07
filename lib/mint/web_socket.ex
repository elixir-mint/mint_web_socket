defmodule Mint.WebSocket do
  @moduledoc """
  WebSocket
  """

  alias __MODULE__.{Utils, Frame}

  @type t :: %__MODULE__{}
  defstruct extensions: [],
            private: %{}

  defguardp is_frame(frame)
            when frame in [:ping, :pong, :close] or
                   (is_tuple(frame) and elem(frame, 0) in [:text, :binary, :ping, :pong] and is_binary(elem(frame, 1))) or
                   (is_tuple(frame) and elem(frame, 0) == :close and is_integer(elem(frame, 1)) and is_binary(elem(frame, 2)))

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
          {:ok, Mint.HTTP.t(), t()} | {:error, any()}
  def new(_conn, _request_ref, status, _request_headers, _response_headers) when status != 101 do
    {:error, :connection_not_upgraded}
  end

  def new(conn, request_ref, _status, request_headers, response_headers) do
    with :ok <- Utils.check_accept_nonce(request_headers, response_headers) do
      {:ok, re_open_request(conn, request_ref), %__MODULE__{}}
    end
  end

  # translate frame to something more user-friendly
  @spec encode(t(), frame :: tuple()) :: {:ok, t(), binary()} | {:error, t(), any()}
  def encode(%__MODULE__{} = websocket, frame) when is_frame(frame) do
    # pass websocket to the frame module, translate extensions
    case frame |> Frame.translate() |> Frame.encode() do
      {:ok, encoded} -> {:ok, websocket, encoded}
      {:error, reason} -> {:error, websocket, reason}
    end
  end

  @spec decode(t(), data :: binary()) :: {:ok, t(), [tuple()]} | {:error, t(), any()}
  def decode(%__MODULE__{} = websocket, data) do
    # pass websocket to the frame module, translate extensions
    case Frame.decode(data) do
      {:ok, decoded} -> {:ok, websocket, Enum.map(decoded, &Frame.translate/1)}
      {:error, reason} -> {:error, websocket, reason}
    end
  end

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
