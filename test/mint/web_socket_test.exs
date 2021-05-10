defmodule Mint.WebSocketTest do
  use ExUnit.Case, async: true

  describe "given a 'hello world' text frame" do
    test "we can send it and receive an echo reply" do
      # bootstrap
      {:ok, conn} = Mint.HTTP.connect(:http, "echo", 9000)
      req_headers = Mint.WebSocket.build_request_headers()
      {:ok, conn, ref} = Mint.HTTP.request(conn, "GET", "/", req_headers, nil)
      assert_receive http_get_message

      {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
        Mint.HTTP.stream(conn, http_get_message)

      {:ok, conn, websocket} = Mint.WebSocket.new(conn, ref, status, req_headers, resp_headers)

      # send the hello world frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, {:text, "hello world"})
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive another message which is the echo reply to our hello world
      assert_receive hello_world_echo_message
      {:ok, _conn, [{:data, ^ref, data}]} = Mint.HTTP.stream(conn, hello_world_echo_message)
      assert {:ok, _websocket, [{:text, "hello world"}]} = Mint.WebSocket.decode(websocket, data)
    end
  end
end
