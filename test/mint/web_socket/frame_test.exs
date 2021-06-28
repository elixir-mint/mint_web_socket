defmodule Mint.WebSocket.FrameTest do
  use ExUnit.Case, async: true

  import Mint.WebSocket.Frame

  test "the fin? guard correctly detects the fin bit in frames" do
    assert text(fin?: true, data: "hello") |> is_fin()
    refute text(fin?: false, data: "hello") |> is_fin()

    assert ping(fin?: true) |> is_fin()
  end
end
