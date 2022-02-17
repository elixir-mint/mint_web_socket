# this is a phoenix v1.3 server that sends pings periodically
# see https://phoenixchat.herokuapp.com for the in-browser version
{:ok, conn} = Mint.HTTP.connect(:https, "phoenixchat.herokuapp.com", 443)

{:ok, conn, ref} = Mint.WebSocket.upgrade(:wss, conn, "/ws", [])

http_get_message = receive(do: (message -> message))
{:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
  Mint.WebSocket.stream(conn, http_get_message)
{:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, resp_headers)

{:ok, websocket, data} = Mint.WebSocket.encode(websocket, {:text, ~s[{"topic":"rooms:lobby","event":"phx_join","payload":{},"ref":1}]})
{:ok, conn} = Mint.WebSocket.stream_request_body(conn, ref, data)

message = receive(do: (message -> message))
{:ok, conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, message)
{:ok, websocket, messages} = Mint.WebSocket.decode(websocket, data)
IO.inspect(messages)

message = receive(do: (message -> message))
{:ok, conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, message)
{:ok, websocket, messages} = Mint.WebSocket.decode(websocket, data)
IO.inspect(messages)

message = receive(do: (message -> message))
{:ok, conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, message)
{:ok, websocket, messages} = Mint.WebSocket.decode(websocket, data)
IO.inspect(messages)

message = receive(do: (message -> message))
{:ok, _conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, message)
{:ok, _websocket, messages} = Mint.WebSocket.decode(websocket, data)
IO.inspect(messages)
