defmodule Mint.WebSocketError do
  @moduledoc """
  Represents an error in the WebSocket protocol

  The `Mint.WebSocketError` struct is an exception, so it can be raised as
  any other exception.
  """

  reason_type =
    quote do
      :extended_connect_disabled
      | :payload_too_large
      | {:extension_not_negotiated, Mint.WebSocket.Extension.t()}
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

  defp format_reason(:payload_too_large) do
    "frame payload cannot exceed 9,223,372,036,854,775,807 bytes"
  end

  defp format_reason({:extension_not_negotiated, extension}) do
    "the remote server accepted an extension the client did not offer: #{inspect(extension)}"
  end
end
