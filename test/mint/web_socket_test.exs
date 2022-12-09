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

  describe "given an HTTP/2 WebSocket connection to an echo server" do
    setup do
      {:ok, conn} = HTTP2.connect(:http, "localhost", 7070)

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
