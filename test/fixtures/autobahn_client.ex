defmodule AutobahnClient do
  @moduledoc """
  A client that uses Mint.WebSocket to test against the Autobahn|Testsuite
  WebSocket testing suite
  """

  import Kernel, except: [send: 2]

  require Logger

  defstruct [:conn, :websocket, :ref, messages: [], next: :cont, sent_close?: false, buffer: <<>>]

  defguardp is_close_frame(frame)
            when frame == :close or (is_tuple(frame) and elem(frame, 0) == :close)

  def get_case_count do
    %{messages: [{:text, count} | _]} = connect("/getCaseCount") |> loop()

    String.to_integer(count)
  end

  def run_case(case_number) do
    _state = connect("/runCase?case=#{case_number}&agent=Mint") |> loop()

    :ok
  end

  def get_case_status(case_number) do
    %{messages: [{:text, status} | _]} =
      connect("/getCaseStatus?case=#{case_number}&agent=Mint") |> decode_buffer()

    Jason.decode!(status)["behavior"]
  end

  def update_reports do
    _state = connect("/updateReports?agent=Mint") |> loop()

    :ok
  end

  def connect(resource) do
    {:ok, conn} = Mint.HTTP.connect(:http, "fuzzingserver", 9001)
    req_headers = Mint.WebSocket.build_request_headers()
    {:ok, conn, ref} = Mint.HTTP.request(conn, "GET", resource, req_headers, nil)
    http_get_message = receive(do: (message -> message))

    {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
      Mint.HTTP.stream(conn, http_get_message)

    {:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, req_headers, resp_headers)

    %__MODULE__{
      next: :cont,
      conn: %{conn | buffer: <<>>},
      ref: ref,
      websocket: websocket,
      buffer: conn.buffer
    }
  end

  def recv(%{ref: ref} = state) do
    case Mint.HTTP.stream(state.conn, receive(do: (message -> message))) do
      {:ok, conn, [{:done, ^ref}]} ->
        %__MODULE__{state | conn: conn, buffer: <<>>, next: :stop}

      {:ok, conn, messages} ->
        %__MODULE__{state | conn: conn, buffer: join_data_frames(messages, ref), next: :cont}
    end
  end

  def decode_buffer(state) do
    case Mint.WebSocket.decode(state.websocket, state.buffer) do
      {:ok, websocket, messages} ->
        %__MODULE__{state | messages: messages, buffer: <<>>, websocket: websocket}

      {:error, websocket, reason} ->
        Logger.debug(
          "Could not parse buffer #{inspect(state.buffer, printable_limit: 30)}" <>
            " because #{inspect(reason)}, sending close"
        )

        %__MODULE__{state | websocket: websocket}
        |> close(close_code_for_reason(reason), "Malformed payload")
    end
  end

  def loop(state) do
    case state |> decode_buffer |> handle_messages do
      %{next: :cont} = state ->
        loop(recv(state))

      state ->
        state
    end
  end

  def handle_messages(state) do
    Enum.reduce(state.messages, state, fn message, state ->
      Logger.debug("Handling #{inspect(message, printable_limit: 30)}")
      handle_message(message, state)
    end)
    |> Map.put(:messages, [])
  end

  defp handle_message(:close, state) do
    handle_message({:close, 1000, ""}, state)
  end

  defp handle_message({:close, _code, _reason}, state) do
    close(state, 1000, "")
  end

  defp handle_message(:ping, state), do: handle_message({:ping, ""}, state)

  defp handle_message({:ping, data}, state) do
    send(state, {:pong, data})
  end

  # no-op on unsolicited pongs
  defp handle_message(:pong, state), do: handle_message({:pong, ""}, state)

  defp handle_message({:pong, _body}, state), do: state

  defp handle_message(frame, state), do: send(state, frame)

  def send(%__MODULE__{sent_close?: true} = state, frame) when is_close_frame(frame) do
    Logger.debug("Ignoring send of close")
    state
  end

  def send(state, frame) do
    Logger.debug("Sending #{inspect(frame, printable_limit: 30)}")

    case Mint.WebSocket.encode(state.websocket, frame) do
      {:ok, websocket, data} ->
        {:ok, conn} = Mint.HTTP.stream_request_body(state.conn, state.ref, data)
        Logger.debug("Sent.")
        %__MODULE__{state | conn: conn, websocket: websocket, sent_close?: is_close_frame(frame)}

      {:error, websocket, reason} ->
        Logger.debug(
          "Could not send frame #{inspect(frame, printable_limit: 30)} because #{inspect(reason)}, sending close..."
        )

        send(put_in(state.websocket, websocket), {:close, 1002, ""})
    end
  end

  defp close(state, code, reason) do
    state = send(state, {:close, code, reason})
    {:ok, conn} = Mint.HTTP.close(state.conn)
    %__MODULE__{state | conn: conn, next: :stop}
  end

  defp close_code_for_reason({:invalid_utf8, _data}), do: 1007
  defp close_code_for_reason(_), do: 1002

  defp join_data_frames(messages, ref) do
    messages
    |> Enum.filter(fn
      {:data, ^ref, _data} -> true
      _ -> false
    end)
    |> Enum.map(fn {:data, ^ref, data} -> data end)
    |> Enum.join(<<>>)
  end
end
