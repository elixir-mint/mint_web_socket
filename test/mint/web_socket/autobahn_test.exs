defmodule Mint.WebSocket.AutobahnTest do
  @moduledoc """
  A suite of tests against the Autobahn|Testsuite

  See https://github.com/crossbario/autobahn-testsuite
  """
  use ExUnit.Case, async: true

  @moduletag :autobahn
  @moduletag :capture_log

  # for case_number <- Range.new(1, AutobahnClient.get_case_count()) do
  for case_number <- 1..209 do
    test "Autobahn|Testsuite case number #{case_number}" do
      assert AutobahnClient.run_case(unquote(case_number)) == :ok
      :ok = flush()
      assert AutobahnClient.get_case_status(unquote(case_number)) in ["OK", "NON-STRICT"]
    end
  end

  defp flush() do
    receive do
      _message -> flush()
    after
      0 -> :ok
    end
  end
end
