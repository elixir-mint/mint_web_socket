:websocket_sup.start_link()
ExUnit.configure(assert_receive_timeout: 500, exclude: [compression: :stress])
ExUnit.start()
