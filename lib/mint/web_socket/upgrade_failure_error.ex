defmodule Mint.WebSocket.UpgradeFailureError do
  @moduledoc """
  An error representing a failure to upgrade protocols from HTTP to WebSocket
  """

  @type t() :: %__MODULE__{
          status_code: Mint.Types.status(),
          headers: [Mint.Types.headers()]
        }
  defexception [:status_code, :headers]

  def message(%__MODULE__{} = error) do
    """
    Could not upgrade from HTTP to WebSocket. The server returned status code #{error.status_code} with headers:
    #{inspect(error.headers)}
    """
  end
end
