defmodule MintWebSocketTest do
  use ExUnit.Case
  doctest MintWebSocket

  test "greets the world" do
    assert MintWebSocket.hello() == :world
  end
end
