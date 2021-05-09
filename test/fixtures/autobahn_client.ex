defmodule AutobahnClient do
  @moduledoc """
  A client that uses Mint.WebSocket to test against the Autobahn|Testsuite
  WebSocket testing suite
  """

  import Kernel, except: [send: 2]

  require Logger

  def connect(resource) do
    {:ok, conn} = Mint.HTTP.connect(:http, "fuzzingserver", 9001)
    req_headers = Mint.WebSocket.build_request_headers()
    {:ok, conn, ref} = Mint.HTTP.request(conn, "GET", resource, req_headers, nil)
    http_get_message = receive(do: (message -> message))

    {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
      Mint.HTTP.stream(conn, http_get_message)

    {:ok, conn, websocket, messages} =
      Mint.WebSocket.new(conn, ref, status, req_headers, resp_headers)

    {:cont, {conn, ref, websocket}, messages}
  end

  def recv({conn, ref, websocket}) do
    case Mint.HTTP.stream(conn, receive(do: (message -> message))) do
      {:ok, conn, [{:done, ^ref}]} ->
        {:stop, {conn, ref, websocket}, []}

      {:ok, conn, messages} ->
        data =
          messages
          |> Enum.filter(fn
            {:data, ^ref, _data} -> true
            _ -> false
          end)
          |> Enum.map(fn {:data, ^ref, data} -> data end)
          |> Enum.join(<<>>)

        {:ok, websocket, messages} = Mint.WebSocket.decode(websocket, data)

        {:cont, {conn, ref, websocket}, messages}

      {:error, %{buffer: buffer} = conn, %Mint.TransportError{reason: :closed}, []} ->
        {:ok, websocket, messages} = Mint.WebSocket.decode(websocket, buffer)

        {:closed, {conn, ref, websocket}, messages}
    end
  end

  def loop({next, state, messages}) do
    {next, state} = handle_messages(state, next, messages)

    if next == :cont do
      loop(recv(state))
    else
      :ok
    end
  end

  def handle_messages(state, next, messages) do
    Enum.reduce(messages, {next, state}, fn message, acc ->
      Logger.debug("Handling #{inspect(message, printable_limit: 30)}")
      handle_message(message, acc)
    end)
  end

  defp handle_message(:close, acc) do
    handle_message({:close, 1000, ""}, acc)
  end

  defp handle_message({:close, _code, _reason}, {_next, {conn, ref, websocket} = state}) do
    send(state, :close)
    {:ok, conn} = Mint.HTTP.close(conn)
    {:stop, {conn, ref, websocket}}
  end

  defp handle_message(frame, {next, state}), do: {next, send(state, frame)}

  def send({conn, ref, websocket}, frame) do
    Logger.debug("Sending #{inspect(frame, printable_limit: 30)}")
    {:ok, websocket, data} = Mint.WebSocket.encode(websocket, frame)
    {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)
    Logger.debug("Sent.")

    {conn, ref, websocket}
  end
end
