do_it = fn ->
  {:ok, conn} = Mint.HTTP.connect(:http, "echo", 8080)
  headers = Mint.WebSocket.build_request_headers()

  {:ok, conn, request_ref} =
    Mint.HTTP.request(conn, "GET", "/", headers, nil)

  message = receive(do: (message -> message))

  Mint.HTTP.stream(conn, message)

  # {:ok, websocket, []} = Mint.WebSocket.upgrade(conn, message)
end
