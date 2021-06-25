# Mint.WebSocket

(Unofficial) HTTP/1 and HTTP/2 WebSocket support for Mint 🌱

## What is Mint?

Mint is a _functional_ HTTP/1 and HTTP/2 client library written in Elixir.

Why does it matter that it's functional? Isn't Elixir functional?

Well, Existing WebSocket implementations like
[`:gun`](https://github.com/ninenines/gun),
[`:websocket_client`](https://github.com/jeremyong/websocket_client),
or [`WebSockex`](https://github.com/Azolo/websockex) work by spawning and
passing messages among processes. This is a very convenient interface in
Elixir and Erlang, but it does not allow the author much control over
the WebSocket connection.

Instead `Mint.WebSocket` is process-less: the entire HTTP and WebSocket
states are kept in immutable datastructures. This enables authors of
WebSocket clients a more fine-grained control over the connections:
`Mint.WebSocket` does not prescribe a process archicture.

For more information, check out
[Mint#Usage](https://github.com/elixir-mint/mint#usage).

## Spec conformance

This library aims to follow
[rfc6455](https://datatracker.ietf.org/doc/html/rfc6455) and
[rfc8441](https://datatracker.ietf.org/doc/html/rfc8441) as closely as possible
and uses [Autobahn|Testsuite](https://github.com/crossbario/autobahn-testsuite)
to check conformance with every run of tests/CI. The auto-generated report
produced by the Autobahn|Testsuite is uploaded on each push to main.

See the report here:
https://mint-websocket.nyc3.digitaloceanspaces.com/autobahn/index.html

## A Quick Note About HTTP/2

HTTP/2 WebSockets are not a built-in feature of HTTP/2. rfc8441 is an extension
to the HTTP/2 protocol and server libraries are not obligated to implement it.
In the current landscape, very few server libraries support the HTTP/2
extended CONNECT method which bootstraps WebSockets.

If `Mint.WebSocket.upgrade/4` returns

```elixir
{:error, conn, %Mint.WebSocketError{reason: :extended_connect_disabled}}
```

Then the server does not support HTTP/2 WebSockets or does not have them
enabled.

## Usage

`Mint.WebSocket` piggybacks much of the existing `Mint.HTTP` API. For example,
this snippet shows sending and receiving a text frame of "hello world" to a
WebSocket server which echos our frames:

```elixir
# bootstrap
{:ok, conn} = Mint.HTTP.connect(:http, "echo", 9000)

{:ok, conn, ref} = Mint.WebSocket.upgrade(conn, "/", [])

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

Interested in developing `Mint.WebSocket`? The `docker-compose.yml` sets up
an Elixir container, a simple websocket echo server, and the Autobahn|Testsuite
fuzzing server.

```
(host)$ docker-compose up -d
(host)$ docker-compose exec app /bin/bash
(app)$ mix deps.get
(app)$ mix test
(app)$ iex -S mix
```
