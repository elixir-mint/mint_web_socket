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
    %{messages: [{:text, count} | _]} = connect("/getCaseCount") |> get_once()

    String.to_integer(count)
  end

  def run_case(case_number, extensions \\ []) do
    _state = connect("/runCase?case=#{case_number}&agent=Mint", extensions) |> loop()

    :ok
  end

  def get_case_status(case_number) do
    %{messages: [{:text, status} | _]} =
      connect("/getCaseStatus?case=#{case_number}&agent=Mint") |> get_once()

    Jason.decode!(status)["behavior"]
  end

  def get_case_info(case_number) do
    %{messages: [{:text, status} | _]} =
      connect("/getCaseInfo?case=#{case_number}&agent=Mint") |> get_once()

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

  defp get_once(state) do
    case decode_buffer(state) do
      %{messages: []} = state -> state |> recv() |> get_once()
      state -> state
    end
  end

  def connect(resource, extensions \\ []) do
    :ok = flush()
    host = System.get_env("FUZZINGSERVER_HOST") || "localhost"
    {:ok, conn} = Mint.HTTP.connect(:http, host, 9001)

    {:ok, conn, ref} = Mint.WebSocket.upgrade(conn, resource, [], extensions: extensions)

    http_get_message = receive(do: (message -> message))

    {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
      Mint.HTTP.stream(conn, http_get_message)

    {:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, resp_headers)

    %__MODULE__{
      next: :cont,
      conn: %{conn | buffer: <<>>},
      ref: ref,
      websocket: websocket,
      buffer: conn.buffer
    }
  end

  def recv(%{ref: ref} = state) do
    {:ok, conn, messages} = Mint.HTTP.stream(state.conn, receive(do: (message -> message)))

    %__MODULE__{
      state
      | conn: conn,
        buffer: join_data_frames(messages, ref),
        next: stop_if_done(messages, ref)
    }
  end

  def decode_buffer(state) do
    IO.inspect(state.buffer)
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

  defp handle_message({:close, _code, _reason}, state) do
    close(state, 1000, "")
  end

  defp handle_message({:ping, data}, state) do
    send(state, {:pong, data})
  end

  # no-op on unsolicited pongs
  defp handle_message({:pong, _body}, state), do: state

  defp handle_message({:error, reason}, state) do
    Logger.debug("Closing the connection because of a protocol error: #{inspect(reason)}")

    code=
      case reason do
        {:invalid_utf8, _data} -> 1_007
        _ -> 1_002
      end

    close(state, code, "")
  end

  defp handle_message(frame, state), do: send(state, frame)

  def send(%__MODULE__{sent_close?: true} = state, frame) when is_close_frame(frame) do
    Logger.debug("Ignoring send of close")
    state
  end

  def send(state, frame) do
    Logger.debug("Sending #{inspect(frame, printable_limit: 30)}")

    with {:ok, %Mint.WebSocket{} = websocket, data} <-
           Mint.WebSocket.encode(state.websocket, frame),
         {:ok, conn} <- Mint.HTTP.stream_request_body(state.conn, state.ref, data) do
      Logger.debug("Sent.")
      %__MODULE__{state | conn: conn, websocket: websocket, sent_close?: is_close_frame(frame)}
    else
      {:error, %Mint.WebSocket{} = websocket, reason} ->
        Logger.debug(
          "Could not send frame #{inspect(frame, printable_limit: 30)} because #{inspect(reason)}, sending close..."
        )

        send(put_in(state.websocket, websocket), {:close, 1002, ""})

      {:error, conn, %Mint.TransportError{reason: :closed}} ->
        Logger.debug(
          "Could not send frame #{inspect(frame, printable_limit: 30)} because the connection is closed"
        )

        {:ok, conn} = Mint.HTTP.close(conn)
        %__MODULE__{state | conn: conn, next: :stop}
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

  defp stop_if_done(messages, ref) do
    if Enum.any?(messages, &match?({:done, ^ref}, &1)), do: :stop, else: :cont
  end
end
