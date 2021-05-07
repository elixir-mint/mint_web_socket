# Mint.WebSocket

WebSocket support for Mint 🌱

> This repo is not complete: it only supports the basics for WebSocket over
> HTTP/1 at the moment. Issues and PRs are welcome :slightly_smiling_face:

## Usage

`Mint.WebSocket` piggybacks much of the existing `Mint.HTTP` API. For example,
here's sending and receiving a text frame of "hello world":

```elixir
{:ok, conn} = Mint.HTTP.connect(:http, "echo", 8080)
req_headers = Mint.WebSocket.build_request_headers()
{:ok, conn, ref} = Mint.HTTP.request(conn, "GET", "/", req_headers, nil)
http_get_message = receive(do: (message -> message))

{:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
  Mint.HTTP.stream(conn, http_get_message)

{:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, req_headers, resp_headers)

# receive one message about the request being served, pushed by the server
request_served_by_message = receive(do: (message -> message))
{:ok, conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, request_served_by_message)

{:ok, websocket, [{:text, "Request served by " <> _}]} =
  Mint.WebSocket.decode(websocket, data)

# send the hello world frame
{:ok, websocket, data} = Mint.WebSocket.encode(websocket, {:text, "hello world"})
{:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

# receive another message which is the echo reply to our hello world
hello_world_echo_message = receive(do: (message -> message))
{:ok, _conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, hello_world_echo_message)
{:ok, _websocket, [{:text, "hello world"}]} = Mint.WebSocket.decode(websocket, data)
```

## Development workflow

Interesting in developing `Mint.WebSocket`? The `docker-compose.yml` sets up
an Elixir container and a simple websocket echo server.

```
(host)$ docker-compose up -d
(host)$ docker-compose exec app /bin/bash
(app)$ mix deps.get
(app)$ mix test
(app)$ iex -S mix
```
