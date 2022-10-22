# Mint.WebSocket

[![CI][ci-badge]][actions]
[![Coverage Status][coverage-badge]][coverage]
[![hex.pm version][hex-version-badge]][hex-package]
[![hex.pm license][hex-licence-badge]][licence]
[![Last Updated][last-updated-badge]][commits]

HTTP/1 and HTTP/2 WebSocket support for Mint 🌱

## Usage

`Mint.WebSocket` works together with `Mint.HTTP` API. For example,
this snippet shows sending and receiving a text frame of "hello world" to a
WebSocket server which echos our frames:

```elixir
# bootstrap
{:ok, conn} = Mint.HTTP.connect(:http, "echo", 9000)

{:ok, conn, ref} = Mint.WebSocket.upgrade(:ws, conn, "/", [])

http_get_message = receive(do: (message -> message))
{:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
  Mint.WebSocket.stream(conn, http_get_message)

{:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, resp_headers)

# send the hello world frame
{:ok, websocket, data} = Mint.WebSocket.encode(websocket, {:text, "hello world"})
{:ok, conn} = Mint.WebSocket.stream_request_body(conn, ref, data)

# receive the hello world reply frame
hello_world_echo_message = receive(do: (message -> message))
{:ok, conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, hello_world_echo_message)
{:ok, websocket, [{:text, "hello world"}]} = Mint.WebSocket.decode(websocket, data)
```

Check out some [examples](./examples) and the online [documentation][hex-docs].

## Functional WebSockets

Mint.WebSocket (like Mint) takes a _functional_ approach.
Other WebSocket implementations like
[`:gun`][gun] / [`:websocket_client`][websocket-client] /
[`Socket`][socket] / [`WebSockex`][websockex] work by spawning and
passing messages among processes. This is a very convenient interface in
Erlang and Elixir but it does not allow the author much control over
the WebSocket connection.

Instead `Mint.WebSocket` is process-less: the entire HTTP and WebSocket
states are kept in immutable data structures. When you implement a WebSocket
client with `Mint.WebSocket`, runtime behavior and process architecture
are up to you: you decide how to handle things like reconnection and failures.

For a practical introduction, check out Mint's [usage documentation][mint-usage].

## Spec conformance

This library aims to follow [RFC6455][rfc6455] and [RFC8441][rfc8441] as
closely as possible and uses the [Autobahn|Testsuite][autobahn] to check
conformance with every run of tests/CI. The auto-generated report produced
by the Autobahn|Testsuite is uploaded on each push to main.

See the report here: https://elixir-mint.github.io/mint_web_socket/

## HTTP/2 Support

HTTP/2 WebSockets are not a built-in feature of HTTP/2. In the current
landscape, very few server libraries support the RFC8441's extended CONNECT
method which bootstraps WebSockets.

If `Mint.WebSocket.upgrade/4` returns

```elixir
{:error, conn, %Mint.WebSocketError{reason: :extended_connect_disabled}}
```

Then the server does not support HTTP/2 WebSockets or does not have them
enabled.

## Development workflow

Contributions are very welcome!

If you're interested in developing `Mint.WebSocket`, you'll need docker-compose
to run the fuzzing test suite. The `docker-compose.yml` sets up an Elixir
container, a simple websocket echo server, and the Autobahn|Testsuite fuzzing
server.

In host:

```sh
docker-compose up -d
docker-compose exec app bash
```

In app:

```sh
mix deps.get
mix test
iex -S mix
```

[ci-badge]: https://github.com/elixir-mint/mint_web_socket/workflows/CI/badge.svg
[actions]: https://github.com/elixir-mint/mint_web_socket/actions/workflows/ci.yml
[coverage]: https://coveralls.io/github/elixir-mint/mint_web_socket
[coverage-badge]: https://coveralls.io/repos/github/elixir-mint/mint_web_socket/badge.svg
[hex-version-badge]: https://img.shields.io/hexpm/v/mint_web_socket.svg
[hex-licence-badge]: https://img.shields.io/hexpm/l/mint_web_socket.svg
[hex-package]: https://hex.pm/packages/mint_web_socket
[licence]: https://github.com/elixir-mint/mint_web_socket/blob/main/LICENSE
[last-updated-badge]: https://img.shields.io/github/last-commit/elixir-mint/mint_web_socket.svg
[commits]: https://github.com/elixir-mint/mint_web_socket/commits/main

[hex-docs]: https://hexdocs.pm/mint_web_socket/Mint.WebSocket.html

[gun]: https://github.com/ninenines/gun
[websocket-client]: https://github.com/jeremyong/websocket_client
[socket]: https://github.com/meh/elixir-socket
[websockex]: https://github.com/Azolo/websockex
[mint-usage]: https://github.com/elixir-mint/mint#usage

[rfc6455]: https://datatracker.ietf.org/doc/html/rfc6455
[rfc8441]: https://datatracker.ietf.org/doc/html/rfc8441
[autobahn]: https://github.com/crossbario/autobahn-testsuite
