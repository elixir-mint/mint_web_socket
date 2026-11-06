defmodule Mint.WebSocketTest do
  use ExUnit.Case, async: true

  alias Mint.{HTTP1, HTTP2, WebSocket, WebSocket.UpgradeFailureError}

  setup_all do
    TestServer.start()
    :ok
  end

  describe "given an active HTTP/1 connection to an echo server" do
    setup do
      host = System.get_env("ECHO_HOST") || "localhost"
      {:ok, conn} = HTTP1.connect(:http, host, 9000)

      [conn: conn]
    end

    test "we can send and hello-world frame and receive an echo reply", %{conn: conn} do
      {:ok, conn, ref} = WebSocket.upgrade(:ws, conn, "/", [])
      assert_receive http_get_message

      {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
        WebSocket.stream(conn, http_get_message)

      {:ok, conn, websocket} = WebSocket.new(conn, ref, status, resp_headers)

      # send the hello world frame
      {:ok, websocket, data} = WebSocket.encode(websocket, {:text, "hello world"})
      {:ok, conn} = WebSocket.stream_request_body(conn, ref, data)

      # receive the hello world reply frame
      assert_receive hello_world_echo_message
      {:ok, conn, [{:data, ^ref, data}]} = WebSocket.stream(conn, hello_world_echo_message)
      assert {:ok, websocket, [{:text, "hello world"}]} = WebSocket.decode(websocket, data)

      # send a ping frame
      {:ok, websocket, data} = WebSocket.encode(websocket, :ping)
      {:ok, conn} = WebSocket.stream_request_body(conn, ref, data)

      # receive a pong frame
      assert_receive pong_message
      {:ok, conn, [{:data, ^ref, data}]} = WebSocket.stream(conn, pong_message)
      assert {:ok, websocket, [{:pong, ""}]} = WebSocket.decode(websocket, data)

      # send a close frame
      {:ok, websocket, data} = WebSocket.encode(websocket, :close)
      {:ok, conn} = WebSocket.stream_request_body(conn, ref, data)

      # receive a close frame
      assert_receive close_message
      {:ok, conn, [{:data, ^ref, data}]} = WebSocket.stream(conn, close_message)
      assert {:ok, _websocket, [{:close, 1_000, ""}]} = WebSocket.decode(websocket, data)

      {:ok, _conn} = HTTP1.close(conn)
    end
  end

  describe "given an HTTP/1 connection with :max_message_size" do
    setup do
      host = System.get_env("ECHO_HOST") || "localhost"
      {:ok, conn} = HTTP1.connect(:http, host, 9000)
      {:ok, conn, ref} = WebSocket.upgrade(:ws, conn, "/", [])
      assert_receive http_get_message

      {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
        WebSocket.stream(conn, http_get_message)

      {:ok, conn, websocket} =
        WebSocket.new(conn, ref, status, resp_headers, max_message_size: 200)

      on_exit(fn -> HTTP1.close(conn) end)
      [conn: conn, ref: ref, status: status, resp_headers: resp_headers, websocket: websocket]
    end

    test "a limit below the maximum control frame payload is rejected", c do
      for size <- [124, 0, "big"] do
        assert_raise ArgumentError, fn ->
          WebSocket.new(c.conn, c.ref, c.status, c.resp_headers, max_message_size: size)
        end
      end

      assert {:ok, _conn, _websocket} =
               WebSocket.new(c.conn, c.ref, c.status, c.resp_headers, max_message_size: 125)
    end

    test "a frame declaring a length over the limit is rejected from its header",
         %{websocket: websocket} do
      # text frame header declaring a 4 GB payload, followed by a few bytes
      header = <<1::1, 0::3, 1::4, 0::1, 127::7, 4_294_967_296::64>>

      assert {:error, _websocket, %Mint.WebSocketError{reason: :message_too_large}} =
               WebSocket.decode(websocket, header <> "abc")
    end

    test "a fragmented message over the limit is rejected", %{websocket: websocket} do
      # 120 bytes each: first text frame (FIN = 0), then the final continuation (FIN = 1)
      first = <<0::1, 0::3, 1::4, 0::1, 120::7>> <> :binary.copy("a", 120)
      last = <<1::1, 0::3, 0::4, 0::1, 120::7>> <> :binary.copy("a", 120)

      assert {:ok, websocket, []} = WebSocket.decode(websocket, first)

      assert {:error, _websocket, %Mint.WebSocketError{reason: :message_too_large}} =
               WebSocket.decode(websocket, last)
    end

    test "a fragmented message of exactly the limit is delivered", %{websocket: websocket} do
      first = <<0::1, 0::3, 1::4, 0::1, 120::7>> <> :binary.copy("a", 120)
      last = <<1::1, 0::3, 0::4, 0::1, 80::7>> <> :binary.copy("a", 80)

      assert {:ok, websocket, []} = WebSocket.decode(websocket, first)
      assert {:ok, _websocket, [{:text, message}]} = WebSocket.decode(websocket, last)
      assert byte_size(message) == 200
    end
  end

  describe "given an HTTP/1 connection with compression and :max_message_size" do
    setup do
      {:ok, conn} = HTTP1.connect(:http, "localhost", 7070)

      {:ok, conn, ref} =
        WebSocket.upgrade(:ws, conn, "/", [], extensions: [WebSocket.PerMessageDeflate])

      assert_receive http_get_message

      {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
        WebSocket.stream(conn, http_get_message)

      {:ok, conn, websocket} =
        WebSocket.new(conn, ref, status, resp_headers, max_message_size: 200)

      on_exit(fn -> HTTP1.close(conn) end)
      [conn: conn, ref: ref, websocket: websocket]
    end

    test "a compressed message inflating over the limit is rejected", c do
      # 1,000 identical bytes compress to a frame far below the limit
      {:ok, websocket, data} = WebSocket.encode(c.websocket, {:text, :binary.copy("a", 1_000)})
      {:ok, conn} = WebSocket.stream_request_body(c.conn, c.ref, data)

      assert_receive echo_message
      {:ok, _conn, [{:data, _ref, data}]} = WebSocket.stream(conn, echo_message)

      assert byte_size(data) < 200

      assert {:error, _websocket, %Mint.WebSocketError{reason: :message_too_large}} =
               WebSocket.decode(websocket, data)
    end

    test "a compressed message inflating exactly to the limit is delivered", c do
      message = :binary.copy("a", 200)
      {:ok, websocket, data} = WebSocket.encode(c.websocket, {:text, message})
      {:ok, conn} = WebSocket.stream_request_body(c.conn, c.ref, data)

      assert_receive echo_message
      {:ok, _conn, [{:data, _ref, data}]} = WebSocket.stream(conn, echo_message)

      assert {:ok, _websocket, [{:text, ^message}]} = WebSocket.decode(websocket, data)
    end
  end

  describe "given a passive HTTP/1 connection to an echo server" do
    setup do
      host = System.get_env("ECHO_HOST") || "localhost"
      {:ok, conn} = HTTP1.connect(:http, host, 9000, mode: :passive)

      [conn: conn]
    end

    test "we can send and receive frames (with recv/3)", %{conn: conn} do
      {:ok, conn, ref} = WebSocket.upgrade(:ws, conn, "/", [])

      {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}, {:done, ^ref}]} =
        WebSocket.recv(conn, 0, 5_000)

      {:ok, conn, websocket} = WebSocket.new(conn, ref, status, resp_headers, mode: :passive)

      # send the hello world frame
      {:ok, websocket, data} = WebSocket.encode(websocket, {:text, "hello world"})
      {:ok, conn} = WebSocket.stream_request_body(conn, ref, data)

      # receive the hello world reply frame
      {:ok, conn, [{:data, ^ref, data}]} = WebSocket.recv(conn, 0, 5_000)
      assert {:ok, websocket, [{:text, "hello world"}]} = WebSocket.decode(websocket, data)

      # send a close frame
      {:ok, websocket, data} = WebSocket.encode(websocket, :close)
      {:ok, conn} = WebSocket.stream_request_body(conn, ref, data)

      # receive a close frame
      {:ok, conn, [{:data, ^ref, data}]} = WebSocket.recv(conn, 0, 5_000)
      assert {:ok, _websocket, [{:close, 1_000, ""}]} = WebSocket.decode(websocket, data)

      {:ok, _conn} = HTTP1.close(conn)
    end
  end

  describe "given a passive HTTP/1 connection to the local cowboy server" do
    setup do
      {:ok, conn} = HTTP1.connect(:http, "localhost", 7070, mode: :passive)
      [conn: conn]
    end

    test "a response code other than 101 gives a UpgradeFailureError", %{conn: conn} do
      {:ok, conn, ref} = WebSocket.upgrade(:ws, conn, "/forbidden", [])

      {:ok, conn,
       [
         {:status, ^ref, status},
         {:headers, ^ref, resp_headers},
         {:data, ^ref, data},
         {:done, ^ref}
       ]} = WebSocket.recv(conn, 0, 5_000)

      assert status == 403
      assert data == "Forbidden."

      assert {:error, _conn, %UpgradeFailureError{} = reason} =
               WebSocket.new(conn, ref, status, resp_headers, mode: :passive)

      assert UpgradeFailureError.message(reason) =~ "status code 403"
    end
  end

  describe "given an HTTP/1 connection to a socket which captures the request" do
    setup do
      {:ok, listen_socket} = :gen_tcp.listen(0, [:binary, active: false, reuseaddr: true])
      on_exit(fn -> :gen_tcp.close(listen_socket) end)
      {:ok, port} = :inet.port(listen_socket)

      [listen_socket: listen_socket, port: port]
    end

    test "the upgrade headers are lowercased by default", c do
      {:ok, conn} = HTTP1.connect(:http, "localhost", c.port)
      {:ok, _conn, _ref} = WebSocket.upgrade(:ws, conn, "/", [])

      request = capture_request(c.listen_socket)
      assert request =~ "upgrade: websocket"
      assert request =~ "sec-websocket-key: "
    end

    test "the upgrade headers keep their casing with case_sensitive_headers: true", c do
      {:ok, conn} = HTTP1.connect(:http, "localhost", c.port, case_sensitive_headers: true)
      {:ok, _conn, _ref} = WebSocket.upgrade(:ws, conn, "/", [])

      request = capture_request(c.listen_socket)
      assert request =~ "Upgrade: websocket"
      assert request =~ "Connection: upgrade"
      assert request =~ "Sec-WebSocket-Version: 13"
      assert request =~ "Sec-WebSocket-Key: "
    end
  end

  defp capture_request(listen_socket) do
    {:ok, socket} = :gen_tcp.accept(listen_socket, 5_000)
    {:ok, request} = :gen_tcp.recv(socket, 0, 5_000)
    :gen_tcp.close(socket)

    request
  end

  @doc !"""
       In Mint 1.5.0+, Mint handles the SETTINGS frame from the server asynchronously
       and returns default values for server settings until it is received. So we must
       wait for the server to send the SETTINGS frame enabling the connect protocol.
       """
  defp wait_for_connect_protocol(conn) do
    if HTTP2.get_server_setting(conn, :enable_connect_protocol) do
      conn
    else
      receive do
        message ->
          {:ok, conn, []} = HTTP2.stream(conn, message)
          wait_for_connect_protocol(conn)
      end
    end
  end

  describe "given an HTTP/2 WebSocket connection to an echo server" do
    setup do
      {:ok, conn} = HTTP2.connect(:http, "localhost", 7070)

      conn = wait_for_connect_protocol(conn)

      {:ok, conn, ref} =
        WebSocket.upgrade(:ws, conn, "/", [], extensions: [WebSocket.PerMessageDeflate])

      {:ok, conn, [{:status, ^ref, status}, {:headers, ^ref, resp_headers}]} =
        stream_until_responses(conn)

      {:ok, conn, websocket} = WebSocket.new(conn, ref, status, resp_headers)

      [conn: conn, ref: ref, websocket: websocket]
    end

    @tag :http2
    test "we can send and hello-world frame and receive an echo reply", c do
      ref = c.ref

      # send the hello world frame
      {:ok, websocket, data} = WebSocket.encode(c.websocket, {:text, "hello world"})
      {:ok, conn} = WebSocket.stream_request_body(c.conn, ref, data)

      # receive the hello world reply frame
      assert_receive hello_world_echo_message
      {:ok, conn, [{:data, ^ref, data}]} = WebSocket.stream(conn, hello_world_echo_message)
      assert {:ok, websocket, [{:text, "hello world"}]} = WebSocket.decode(websocket, data)

      # send a ping frame
      {:ok, websocket, data} = WebSocket.encode(websocket, :ping)
      {:ok, conn} = WebSocket.stream_request_body(conn, ref, data)

      # receive a pong frame
      assert_receive pong_message
      {:ok, conn, [{:data, ^ref, data}]} = WebSocket.stream(conn, pong_message)
      assert {:ok, websocket, [{:pong, ""}]} = WebSocket.decode(websocket, data)

      # send a close frame
      {:ok, websocket, data} = WebSocket.encode(websocket, :close)
      {:ok, conn} = WebSocket.stream_request_body(conn, ref, data)

      # receive a close frame
      assert_receive close_message

      {:ok, conn, [{:data, ^ref, data}, {:done, ^ref}]} = WebSocket.stream(conn, close_message)

      assert {:ok, _websocket, [{:close, 1_000, ""}]} = WebSocket.decode(websocket, data)

      {:ok, _conn} = HTTP2.close(conn)
    end

    @tag :http2
    test "we can multiplex WebSocket and HTTP traffic", c do
      websocket_ref = c.ref

      {:ok, conn, http_ref} = HTTP2.request(c.conn, "GET", "/http_get", [], nil)

      assert_receive http_get_response

      assert {:ok, conn,
              [
                {:status, ^http_ref, 200},
                {:headers, ^http_ref, _headers},
                {:data, ^http_ref, "hi!"},
                {:done, ^http_ref}
              ]} = WebSocket.stream(conn, http_get_response)

      # send the hello world frame
      {:ok, websocket, data} = WebSocket.encode(c.websocket, {:text, "hello world"})
      {:ok, conn} = WebSocket.stream_request_body(conn, websocket_ref, data)

      # receive the hello world reply frame
      assert_receive hello_world_echo_message

      {:ok, conn, [{:data, ^websocket_ref, data}]} =
        WebSocket.stream(conn, hello_world_echo_message)

      assert {:ok, _websocket, [{:text, "hello world"}]} = WebSocket.decode(websocket, data)

      {:ok, _conn} = HTTP2.close(conn)
    end

    @tag :http2
    test "a response code outside the 200..299 range gives a UpgradeFailureError", %{conn: conn} do
      {:ok, conn, ref} = WebSocket.upgrade(:ws, conn, "/forbidden", [])

      assert_receive message

      {:ok, conn,
       [
         {:status, ^ref, status},
         {:headers, ^ref, resp_headers},
         {:data, ^ref, data},
         {:done, ^ref}
       ]} = WebSocket.stream(conn, message)

      assert status == 403
      assert data == "Forbidden."

      assert {:error, _conn, %UpgradeFailureError{} = reason} =
               WebSocket.new(conn, ref, status, resp_headers, mode: :passive)

      assert UpgradeFailureError.message(reason) =~ "status code 403"
    end
  end

  # cowboy's WebSocket is a little weird here, is it sending SETTINGS frames and then
  # frames with content? will need to crack open WireShark to tell
  defp stream_until_responses(conn) do
    with {:ok, conn, []} <- WebSocket.stream(conn, receive(do: (message -> message))) do
      stream_until_responses(conn)
    end
  end
end
