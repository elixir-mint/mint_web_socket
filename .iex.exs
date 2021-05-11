do_it = fn ->
  host = System.get_env("H2SERVER_HOST") || "h2server"

  {:ok, conn} =
    Mint.HTTP.connect(
      :http,
      host,
      7070,
      protocols: [:http2],
      client_settings: [enable_connect_protocol: true]
    )

  conn

  #req_headers = Mint.WebSocket.build_request_headers()
  #{:ok, conn, ref} = Mint.HTTP.request(conn, "GET", "/", req_headers, nil)
  #http_get_message = receive(do: (message -> message))

  #{:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
    #Mint.HTTP.stream(conn, http_get_message)

  #Mint.WebSocket.new(conn, ref, status, req_headers, resp_headers)
end
