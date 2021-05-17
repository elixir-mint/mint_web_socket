defmodule Mint.WebSocketError do
  @moduledoc """
  Represents an error in the WebSocket protocol

  The `Mint.WebSocketError` struct is an exception, so it can be raised as
  any other exception.
  """

  reason_type =
    quote do
      :extended_connect_disabled
      | :connection_not_upgraded
    end

  @type t :: %__MODULE__{reason: unquote(reason_type) | term()}

  defexception [:reason]

  @impl Exception
  def message(%__MODULE__{reason: reason}) do
    format_reason(reason)
  end

  defp format_reason(:extended_connect_disabled) do
    "extended CONNECT method not enabled"
  end

  defp format_reason(:connection_not_upgraded) do
    "connection not upgraded by remote"
  end
end
