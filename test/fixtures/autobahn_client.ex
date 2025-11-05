defmodule AutobahnClient do
  @moduledoc """
  A client that uses Mint.WebSocket to test against the Autobahn|Testsuite
  WebSocket testing suite
  """

  import Kernel, except: [send: 2]
  require Logger

  defstruct [:conn, :websocket, :ref, messages: [], next: :cont, sent_close?: false, buffer: <<>>]

  defguardp is_close_frame(frame)
            when is_tuple(frame) and elem(frame, 0) == :close

  def get_case_count do
    %{messages: [{:text, count} | _]} = connect("/getCaseCount") |> decode_buffer()

    String.to_integer(count)
  end

  def run_case(case_number, extensions \\ []) do
    _state = connect("/runCase?case=#{case_number}&agent=Mint", extensions) |> loop()

    :ok
  end

  def get_case_status(case_number) do
    %{messages: [{:text, status} | _]} =
      connect("/getCaseStatus?case=#{case_number}&agent=Mint") |> decode_buffer()

    Jason.decode!(status)["behavior"]
  end

  def get_case_info(case_number) do
    %{messages: [{:text, status} | _]} =
      connect("/getCaseInfo?case=#{case_number}&agent=Mint") |> decode_buffer()

    Jason.decode!(status, keys: :atoms)
  end

  def update_reports do
    _state = connect("/updateReports?agent=Mint") |> loop()

    :ok
  end

  def flush do
    receive do
      _message -> flush()
    after
      0 -> :ok
    end
  end

  def connect(resource, extensions \\ []) do
    :ok = flush()
    host = System.get_env("FUZZINGSERVER_HOST") || "localhost"
    {:ok, conn} = Mint.HTTP.connect(:http, host, 9001)

    {:ok, conn, ref} = Mint.WebSocket.upgrade(:ws, conn, resource, [], extensions: extensions)

    http_get_message = receive(do: (message -> message))

    {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers} | rest]} =
      Mint.WebSocket.stream(conn, http_get_message)

    buffer =
      case rest do
        [{:data, ^ref, data}, {:done, ^ref}] -> data
        [{:done, ^ref}] -> <<>>
      end

    {:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, resp_headers)

    %__MODULE__{
      next: :cont,
      conn: conn,
      ref: ref,
      websocket: websocket,
      buffer: buffer
    }
  end

  def recv(%__MODULE__{ref: ref} = state) do
    {:ok, conn, messages} = Mint.WebSocket.stream(state.conn, receive(do: (message -> message)))

    %{
      state
      | conn: conn,
        buffer: join_data_frames(messages, ref),
        next: stop_if_done(messages, ref)
    }
  end

  def decode_buffer(%__MODULE__{} = state) do
    {:ok, websocket, messages} = Mint.WebSocket.decode(state.websocket, state.buffer)

    %{state | messages: messages, buffer: <<>>, websocket: websocket}
  end

  def loop(%__MODULE__{} = state) do
    case state |> decode_buffer |> handle_messages do
      %{next: :cont} = state ->
        loop(recv(state))

      state ->
        state
    end
  end

  def handle_messages(%__MODULE__{} = state) do
    Enum.reduce(state.messages, state, fn message, state ->
      Logger.debug("Handling #{inspect(message, printable_limit: 30)}")
      handle_message(message, state)
    end)
    |> Map.put(:messages, [])
  end

  defp handle_message({:close, _code, _reason}, %__MODULE__{} = state) do
    close(state, 1000, "")
  end

  defp handle_message({:ping, data}, %__MODULE__{} = state) do
    send(state, {:pong, data})
  end

  # no-op on unsolicited pongs
  defp handle_message({:pong, _body}, %__MODULE__{} = state), do: state

  defp handle_message({:error, reason}, %__MODULE__{} = state) do
    Logger.debug("Closing the connection because of a protocol error: #{inspect(reason)}")

    code =
      case reason do
        {:invalid_utf8, _data} -> 1_007
        _ -> 1_002
      end

    close(state, code, "")
  end

  defp handle_message(frame, %__MODULE__{} = state), do: send(state, frame)

  def send(%__MODULE__{sent_close?: true} = state, frame) when is_close_frame(frame) do
    Logger.debug("Ignoring send of close")
    state
  end

  def send(state, frame) do
    Logger.debug("Sending #{inspect(frame, printable_limit: 30)}")

    case Mint.WebSocket.encode(state.websocket, frame) do
      {:ok, websocket, data} ->
        do_send(put_in(state.websocket, websocket), frame, data)

      {:error, websocket, reason} ->
        Logger.debug(
          "Could not send frame #{inspect(frame, printable_limit: 30)} because #{inspect(reason)}, sending close..."
        )

        send(put_in(state.websocket, websocket), {:close, 1002, ""})
    end
  end

  defp do_send(%__MODULE__{} = state, frame, data) do
    case Mint.WebSocket.stream_request_body(state.conn, state.ref, data) do
      {:ok, conn} ->
        Logger.debug("Sent.")
        %{state | conn: conn, sent_close?: is_close_frame(frame)}

      {:error, conn, %Mint.TransportError{reason: :closed}} ->
        Logger.debug(
          "Could not send frame #{inspect(frame, printable_limit: 30)} because the connection is closed"
        )

        {:ok, conn} = Mint.HTTP.close(conn)
        %{state | conn: conn, next: :stop}
    end
  end

  defp close(%__MODULE__{} = state, code, reason) do
    state = send(state, {:close, code, reason})
    {:ok, conn} = Mint.HTTP.close(state.conn)
    %{state | conn: conn, next: :stop}
  end

  defp join_data_frames(messages, ref) do
    messages
    |> Enum.filter(fn
      {:data, ^ref, _data} -> true
      _ -> false
    end)
    |> Enum.map_join(<<>>, fn {:data, ^ref, data} -> data end)
  end

  defp stop_if_done(messages, ref) do
    if Enum.any?(messages, &match?({:done, ^ref}, &1)), do: :stop, else: :cont
  end
end
