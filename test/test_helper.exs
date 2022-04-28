ExUnit.configure(assert_receive_timeout: 500, exclude: [compression: :stress, flaky: true])
ExUnit.start()
