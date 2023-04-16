defmodule Mint.WebSocket.FrameTest do
  use ExUnit.Case, async: true

  import Mint.WebSocket.Frame

  test "the fin? guard correctly detects the fin bit in frames" do
    assert text(fin?: true, data: "hello") |> is_fin()
    refute text(fin?: false, data: "hello") |> is_fin()

    assert ping(fin?: true) |> is_fin()
  end

  test "incomplete frames should return error" do
    assert {:error, :unexpected_continuation} =
             translate({:continuation, <<0x0::size(3)>>, nil, "hello", true})
  end
end
