defmodule Mint.WebSocket.FrameTest do
  use ExUnit.Case, async: true

  import Mint.WebSocket.Frame

  describe "given real-life text-frames" do
    test "masked text frames can be decoded and encoded" do
      masked = masked_frame()

      assert {:ok, [decoded]} =
               {:ok,
                [
                  text(
                    fin?: true,
                    reserved: <<0::size(3)>>,
                    mask: <<33, 29, 36, 227>>,
                    data: ~s(["1","1","rooms:lobby","phx_join",{}])
                  )
                ]} = decode(masked)

      assert {:ok, ^masked} = encode(decoded)
    end

    test "unmasked text frames can be decoded and encoded" do
      masked = unmasked_frame()

      assert {:ok, [decoded]} =
               {:ok,
                [
                  text(
                    fin?: true,
                    reserved: <<0::size(3)>>,
                    mask: nil,
                    data: ~s(["1","1","rooms:lobby","phx_reply",{"response":{},"status":"ok"}])
                  )
                ]} = decode(masked)

      assert {:ok, ^masked} = encode(decoded)
    end
  end

  defp masked_frame do
    # client->server text frame
    "81a5211d24e37a3f15c10d3f15c10d3f568c4e7057d94d724681583f08c151755cbc4b724d8d03315f9e7c"
    |> hex_to_binary()
  end

  defp unmasked_frame do
    # server->client text frame in response
    "81415b2231222c2231222c22726f6f6d733a6c6f626279222c227068785f7265706c79222c7b22726573706f6e7365223a7b7d2c22737461747573223a226f6b227d5d"
    |> hex_to_binary()
  end

  defp hex_to_binary(hex_string) do
    for <<char::8*2 <- hex_string>>, into: <<>> do
      <<char::8*2>>
      |> Integer.parse(16)
      |> then(fn {parsed, <<>>} -> <<parsed>> end)
    end
  end
end
