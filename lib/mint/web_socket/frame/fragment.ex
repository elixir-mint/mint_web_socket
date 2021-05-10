defmodule Mint.WebSocket.Frame.Fragment do
  @moduledoc false

  require Mint.WebSocket.Frame, as: Frame

  # helper functions for collecting and combining fragments

  @doc """
  Emits frames for any finalized fragments and stores any unfinalized fragments
  in the `:fragments` key in the websocket
  """
  def resolve(websocket, frames, acc \\ [])

  def resolve(websocket, [], acc) do
    {websocket, :lists.reverse(acc)}
  end

  def resolve(websocket, [frame | rest], acc) when Frame.is_control(frame) do
    resolve(websocket, rest, [Frame.translate(frame) | acc])
  end

  def resolve(websocket, [frame | rest], acc) when Frame.is_fin(frame) do
    frame = combine_frames([frame | websocket.fragments])

    put_in(websocket.fragments, [])
    |> resolve(rest, [frame | acc])
  end

  def resolve(websocket, [frame | rest], acc) do
    update_in(websocket.fragments, &[frame | &1])
    |> resolve(rest, acc)
  end

  defp combine_frames([Frame.continuation()]) do
    throw({:mint, :uninitiated_continuation})
  end

  defp combine_frames([full_frame]) do
    Frame.translate(full_frame)
  end

  defp combine_frames([Frame.continuation() = continuation, prior_fragment | rest]) do
    combine_frames([Frame.combine(prior_fragment, continuation) | rest])
  end

  defp combine_frames(_out_of_order_fragments) do
    throw({:mint, :out_of_order_fragments})
  end
end
