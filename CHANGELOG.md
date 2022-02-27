# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a
Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.3.0 - 2022-02-27

### Changed

- Failure to upgrade now gives a `Mint.WebSocket.UpgradeFailureError`
  as the error when a server returns a status code other than 101 for
  HTTP/1 or a status code in the range 200..299 range for HTTP/2.

## 0.2.0 - 2022-02-17

This release is a breaking change from the 0.1.0 series. This update removes
all instances where Mint.WebSocket would access opaque `t:Mint.HTTP.t/0` fields
or call private functions within `Mint.HTTP1`, so now Mint.WebSocket should be
more compatible with future changes to Mint.

#### Upgrade guide

First, add the `scheme` argument to calls to `Mint.WebSocket.upgrade/5`.
For connections formed with `Mint.HTTP.connect(:http, ..)`, use the `:ws`
scheme. For `Mint.HTTP.connect(:https, ..)`, use `:wss`.


```diff
- Mint.WebSocket.upgrade(conn, path, headers)
+ Mint.WebSocket.upgrade(scheme, conn, path, headers)
```

Then replace calls to `Mint.HTTP.stream/2` and/or `Mint.HTTP.recv/3` and
`Mint.HTTP.stream_request_body/3` with the new `Mint.WebSocket` wrappers.
This is safe to do even when these functions are being used to send and
receive data in normal HTTP requests: the functionality only changes when
the connection is an established HTTP/1 WebSocket.

### Added

- Added `Mint.WebSocket.stream/2` which wraps `Mint.HTTP.stream/2`
- Added `Mint.WebSocket.recv/3` which wraps `Mint.HTTP.recv/3`
- Added `Mint.WebSocket.stream_request_body/3` which wraps `Mint.HTTP.stream_request_body/3`

### Changed

- Changed function signature of `Mint.Websocket.upgrade/5` to accept the
  WebSocket's scheme (`:ws` or `:wss`) as the first argument
- Added an optional `opts` argument to `Mint.WebSocket.new/5` to control
  active vs. passive mode on the socket
- Restricted compatible Mint versions to `~> 1.4`
    - `Mint.WebSocket` now uses `Mint.HTTP.get_protocol/1` which was
      introduced in `1.4.0`.

## 0.1.4 - 2021-07-06

### Fixed

- Fixed typespec for `Mint.WebSocket.new/4`

## 0.1.3 - 2021-07-02

### Fixed

- Switch from using `Bitwise.bor/2` to `:erlang.bor/2` for compatibility
  with Elixir < 1.10

## 0.1.2 - 2021-07-02

### Fixed

- Switch from using `Bitwise.bxor/2` to `:erlang.bxor/2` for compatibility
  with Elixir < 1.10

## 0.1.1 - 2021-07-01

### Fixed

- Close frame codes and reasons are now nillable instead of defaulted
  - The WebSocket spec does not require that a code and reason be included
    for all close frames

## 0.1.0 - 2021-06-30

### Added

- Initial implementation
    - includes HTTP/1.1 and HTTP/2 support and extensions
