defmodule AutobahnClient do
  @moduledoc """
  A client that uses Mint.WebSocket to test against the Autobahn|Testsuite
  WebSocket testing suite
  """

  import Kernel, except: [send: 2]

  require Logger

  defstruct [:conn, :websocket, :ref, messages: [], next: :cont]

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
      connect("/getCaseStatus?case=#{case_number}&agent=Mint") |> loop()

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

    {:ok, conn, websocket, messages} =
      Mint.WebSocket.new(conn, ref, status, req_headers, resp_headers)

    %__MODULE__{
      next: :cont,
      conn: conn,
      ref: ref,
      websocket: websocket,
      messages: messages
    }
  end

  def recv(%{ref: ref} = state) do
    case Mint.HTTP.stream(state.conn, receive(do: (message -> message))) do
      {:ok, conn, [{:done, ^ref}]} ->
        %__MODULE__{state | conn: conn, messages: [], next: :stop}

      {:ok, conn, messages} ->
        {:ok, websocket, messages} =
          Mint.WebSocket.decode(state.websocket, join_data_frames(messages, ref))

        %__MODULE__{state | conn: conn, messages: messages, next: :cont, websocket: websocket}
    end
  end

  def loop(state) do
    case handle_messages(state) do
      %{next: :cont} ->
        loop(recv(state))
      _ ->
        state
    end
  end

  def handle_messages(state) do
    Enum.reduce(state.messages, state, fn message, state ->
      Logger.debug("Handling #{inspect(message, printable_limit: 30)}")
      handle_message(message, state)
    end)
  end

  defp handle_message(:close, state) do
    handle_message({:close, 1000, ""}, state)
  end

  defp handle_message({:close, _code, _reason}, state) do
    state = send(state, :close)
    {:ok, conn} = Mint.HTTP.close(state.conn)
    %__MODULE__{state | conn: conn, next: :stop}
  end

  defp handle_message(:ping, state), do: handle_message({:ping, ""}, state)

  defp handle_message({:ping, data}, state) do
    send(state, {:pong, data})
  end

  # no-op on unsolicited pongs
  defp handle_message(:pong, state), do: handle_message({:pong, ""}, state)

  defp handle_message({:pong, _body}, state), do: state

  defp handle_message(frame, state), do: send(state, frame)

  def send(state, frame) do
    Logger.debug("Sending #{inspect(frame, printable_limit: 30)}")

    case Mint.WebSocket.encode(state.websocket, frame) do
      {:ok, websocket, data} ->
        {:ok, conn} = Mint.HTTP.stream_request_body(state.conn, state.ref, data)
        Logger.debug("Sent.")
        %__MODULE__{state | conn: conn, websocket: websocket}

      {:error, websocket, reason} ->
        Logger.debug("Could not send frame #{inspect(frame, printable_limit: 30)} because #{inspect(reason)}, sending close...")
        send(put_in(state.websocket, websocket), {:close, 1002, ""})
    end
  end

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
