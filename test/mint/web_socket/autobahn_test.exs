defmodule Mint.WebSocket.AutobahnTest do
  @moduledoc """
  A suite of tests against the Autobahn|Testsuite

  See https://github.com/crossbario/autobahn-testsuite
  """
  use ExUnit.Case, async: true

  @moduletag :autobahn
  @moduletag :capture_log

  describe "Autobahn|Testsuite" do
    for case_number <- Range.new(1, AutobahnClient.get_case_count()) do
      info = AutobahnClient.get_case_info(case_number)

      test inspect("case #{info.id} (##{case_number}): #{info.description}", printable_limit: 200) do
        assert AutobahnClient.run_case(unquote(case_number)) == :ok
        :ok = flush()

        assert AutobahnClient.get_case_status(unquote(case_number)) in ~w[OK NON-STRICT INFORMATIONAL]
      end
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
