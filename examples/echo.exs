# see https://websocket.org/echo.html
{:ok, conn} = Mint.HTTP.connect(:https, "echo.websocket.org", 443)

{:ok, conn, ref} = Mint.WebSocket.upgrade(conn, "/", [])

message = receive(do: (message -> message))
{:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
  Mint.HTTP.stream(conn, message)
{:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, resp_headers)

frame = {:text, "Rock it with Mint.WebSocket"}

{:ok, websocket, data} = Mint.WebSocket.encode(websocket, frame)
{:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

message = receive(do: (message -> message))
{:ok, conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, message)
{:ok, websocket, [{:text, "Rock it with Mint.WebSocket"}]} =
  Mint.WebSocket.decode(websocket, data)

{:ok, websocket, data} = Mint.WebSocket.encode(websocket, :close)
{:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

message = receive(do: (message -> message))
{:ok, conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, message)
{:ok, websocket, [{:close, 1_000, ""}]} =
  Mint.WebSocket.decode(websocket, data)

Mint.HTTP.close(conn)
