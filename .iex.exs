# TODO move into test file, delete .iex.exs
do_it = fn ->
    host = System.get_env("H2SERVER_HOST") || "h2server"

  {:ok, conn} =
    Mint.HTTP.connect(
      :http,
      host,
      7070,
      protocols: [:http2]
    )

  {:ok, conn, ref} = Mint.WebSocket.upgrade(conn, "/", [])

  {:ok, conn, []} = Mint.HTTP.stream(conn, receive(do: (message -> message)))

  http_get_message = receive(do: (message -> message))

  {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}]} =
    Mint.HTTP.stream(conn, http_get_message)

  {:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, resp_headers)

  {:ok, websocket, data} = Mint.WebSocket.encode(websocket, {:text, "hello world"})
  {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

  hello_world_echo_message = receive(do: (message -> message))
  {:ok, conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, hello_world_echo_message)
  Mint.WebSocket.decode(websocket, data)
end
