# Mint.WebSocket

WebSocket support for Mint 🌱

> This repo is not complete: it only tests WebSockets over HTTP/1.
> Issues and PRs are welcome :slightly_smiling_face:

## Usage

`Mint.WebSocket` piggybacks much of the existing `Mint.HTTP` API. For example,
here's sending and receiving a text frame of "hello world" to a WebSocket
server which echos our frames:

```elixir
# bootstrap
{:ok, conn} = Mint.HTTP.connect(:http, "echo", 9000)
req_headers = Mint.WebSocket.build_request_headers()
{:ok, conn, ref} = Mint.HTTP.request(conn, "GET", "/", req_headers, nil)
http_get_message = receive(do: (message -> message))

{:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
  Mint.HTTP.stream(conn, http_get_message)

{:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, req_headers, resp_headers)

# send the hello world frame
{:ok, websocket, data} = Mint.WebSocket.encode(websocket, {:text, "hello world"})
{:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

# receive the hello world reply frame
hello_world_echo_message = receive(do: (message -> message))
{:ok, _conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, hello_world_echo_message)
{:ok, _websocket, [{:text, "hello world"}]} = Mint.WebSocket.decode(websocket, data)
```

## Development workflow

Interesting in developing `Mint.WebSocket`? The `docker-compose.yml` sets up
an Elixir container, a simple websocket echo server, and the Autobahn|Testsuite
fuzzing server.

```
(host)$ docker-compose up -d
(host)$ docker-compose exec app /bin/bash
(app)$ mix deps.get
(app)$ mix test
(app)$ iex -S mix
```
