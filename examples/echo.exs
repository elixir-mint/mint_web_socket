Mix.install([:mint_web_socket, :castore])
require Logger

# see https://websocket.org/echo.html
{:ok, conn} = Mint.HTTP.connect(:https, "echo.websocket.org", 443)
Logger.debug("Connected to https://echo.websocket.org:443")

Logger.debug("Upgrading to WebSocket protocol on /")
{:ok, conn, ref} = Mint.WebSocket.upgrade(conn, "/", [])

message = receive(do: (message -> message))
{:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
  Mint.HTTP.stream(conn, message)
{:ok, conn, websocket} = Mint.WebSocket.new(:wss, conn, ref, status, resp_headers)
Logger.debug("WebSocket established")

frame = {:text, "Rock it with Mint.WebSocket"}
Logger.debug("Sending frame #{inspect(frame)}")
{:ok, websocket, data} = Mint.WebSocket.encode(websocket, frame)
{:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

message = receive(do: (message -> message))
{:ok, conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, message)
{:ok, websocket, frames} = Mint.WebSocket.decode(websocket, data)
Logger.debug("Received frames #{inspect(frames)}")

frame = :close
Logger.debug("Sending frame #{inspect(frame)}")
{:ok, websocket, data} = Mint.WebSocket.encode(websocket, frame)
{:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

message = receive(do: (message -> message))
{:ok, conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, message)
{:ok, websocket, frames} = Mint.WebSocket.decode(websocket, data)
Logger.debug("Received frames #{inspect(frames)}")

Mint.HTTP.close(conn)
