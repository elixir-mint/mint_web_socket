defmodule Mint.WebSocketTest do
  use ExUnit.Case, async: true

  describe "given a 'hello world' text frame" do
    test "we can send it and receive an echo reply" do
      # bootstrap
      host = System.get_env("ECHO_HOST") || "localhost"
      {:ok, conn} = Mint.HTTP.connect(:http, host, 9000)

      {:ok, conn, ref} = Mint.WebSocket.upgrade(conn, "/", [])
      assert_receive http_get_message

      {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
        Mint.HTTP.stream(conn, http_get_message)

      {:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, resp_headers)

      # send the hello world frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, {:text, "hello world"})
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive the hello world reply frame
      assert_receive hello_world_echo_message
      {:ok, conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, hello_world_echo_message)
      assert {:ok, websocket, [{:text, "hello world"}]} = Mint.WebSocket.decode(websocket, data)

      # send a ping frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, :ping)
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive a pong frame
      assert_receive pong_message
      {:ok, conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, pong_message)
      assert {:ok, websocket, [:pong]} = Mint.WebSocket.decode(websocket, data)

      # send a close frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, :close)
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive a close frame
      assert_receive close_message
      {:ok, conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, close_message)
      assert {:ok, _websocket, [:close]} = Mint.WebSocket.decode(websocket, data)

      {:ok, _conn} = Mint.HTTP.close(conn)
    end
  end
end
