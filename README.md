# Mint.WebSocket

WebSocket support for Mint 🌱

> This repo is not complete: it only supports the basics for WebSocket over
> HTTP/1 at the moment. Issues and PRs are welcome :slightly_smiling_face:

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
