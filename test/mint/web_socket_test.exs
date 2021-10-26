defmodule Mint.WebSocketTest do
  use ExUnit.Case, async: true

  describe "given an HTTP/1 connection to an echo server" do
    setup do
      host = System.get_env("ECHO_HOST") || "localhost"
      {:ok, conn} = Mint.HTTP.connect(:http, host, 9000)

      [conn: conn]
    end

    test "we can send and hello-world frame and receive an echo reply", %{conn: conn} do
      {:ok, conn, ref} = Mint.WebSocket.upgrade(conn, "/", [])
      assert_receive http_get_message

      {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
        Mint.WebSocket.stream(conn, http_get_message)

      {:ok, conn, websocket} = Mint.WebSocket.new(:ws, conn, ref, status, resp_headers)

      # send the hello world frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, {:text, "hello world"})
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive the hello world reply frame
      assert_receive hello_world_echo_message
      {:ok, conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, hello_world_echo_message)
      assert {:ok, websocket, [{:text, "hello world"}]} = Mint.WebSocket.decode(websocket, data)

      # send a ping frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, :ping)
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive a pong frame
      assert_receive pong_message
      {:ok, conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, pong_message)
      assert {:ok, websocket, [{:pong, ""}]} = Mint.WebSocket.decode(websocket, data)

      # send a close frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, :close)
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive a close frame
      assert_receive close_message
      {:ok, conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, close_message)
      assert {:ok, _websocket, [{:close, 1_000, ""}]} = Mint.WebSocket.decode(websocket, data)

      {:ok, _conn} = Mint.HTTP.close(conn)
    end
  end

  describe "given an HTTP/2 connection to an echo server" do
    setup do
      start_supervised!(WebsocketSupervisor)

      {:ok, conn} = Mint.HTTP.connect(:http, "localhost", 7070, protocols: [:http2])

      [conn: conn]
    end

    @tag :http2
    test "we can send and hello-world frame and receive an echo reply", %{conn: conn} do
      {:ok, conn, ref} =
        Mint.WebSocket.upgrade(conn, "/", [], extensions: [Mint.WebSocket.PerMessageDeflate])

      assert_receive http_connect_message

      {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}]} =
        case Mint.WebSocket.stream(conn, http_connect_message) do
          {:ok, conn, []} ->
            assert_receive http_connect_message
            Mint.WebSocket.stream(conn, http_connect_message)

          other ->
            other
        end

      {:ok, conn, websocket} = Mint.WebSocket.new(:ws, conn, ref, status, resp_headers)

      # send the hello world frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, {:text, "hello world"})
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive the hello world reply frame
      assert_receive hello_world_echo_message
      {:ok, conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, hello_world_echo_message)
      assert {:ok, websocket, [{:text, "hello world"}]} = Mint.WebSocket.decode(websocket, data)

      # send a ping frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, :ping)
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive a pong frame
      assert_receive pong_message
      {:ok, conn, [{:data, ^ref, data}]} = Mint.WebSocket.stream(conn, pong_message)
      assert {:ok, websocket, [{:pong, ""}]} = Mint.WebSocket.decode(websocket, data)

      # send a close frame
      {:ok, websocket, data} = Mint.WebSocket.encode(websocket, :close)
      {:ok, conn} = Mint.HTTP.stream_request_body(conn, ref, data)

      # receive a close frame
      assert_receive close_message

      {:ok, conn, [{:data, ^ref, data}, {:done, ^ref}]} =
        Mint.WebSocket.stream(conn, close_message)

      assert {:ok, _websocket, [{:close, 1_000, ""}]} = Mint.WebSocket.decode(websocket, data)

      {:ok, _conn} = Mint.HTTP.close(conn)
    end
  end
end
