defmodule Mint.WebSocket.AutobahnTest do
  @moduledoc """
  A suite of tests against the Autobahn|Testsuite

  See https://github.com/crossbario/autobahn-testsuite
  """
  use ExUnit.Case, async: true

  @moduletag :autobahn
  @moduletag :capture_log

  @extensions [
    {~r"^12\.", [Mint.WebSocket.PerMessageDeflate]},
    {~r"^13\.",
     [
       {Mint.WebSocket.PerMessageDeflate,
        [client_no_context_takeover: true, client_max_window_bits: true]}
     ]},
    {~r/.*/, []}
  ]

  @test_tags [
    {~r"^9\.", :performance},
    {~r"^1(2|3)\.\d\.(1|2|3|4)$", compression: :basic},
    {~r"^1(2|3)\.", compression: :stress},
    {~r"^7\.1\.6$", flaky: true}
  ]

  setup_all do
    on_exit(&AutobahnClient.update_reports/0)
  end

  describe "Autobahn|Testsuite" do
    for case_number <- Range.new(1, AutobahnClient.get_case_count()) do
      info = Task.await(Task.async(fn -> AutobahnClient.get_case_info(case_number) end))

      if tag =
           Enum.find_value(@test_tags, fn {regex, tag} -> Regex.match?(regex, info.id) && tag end) do
        @tag tag
      end

      test inspect("case #{info.id} (##{case_number}): #{info.description}", printable_limit: 200) do
        extensions = extensions_for_case(unquote(info.id))
        assert AutobahnClient.run_case(unquote(case_number), extensions) == :ok

        assert AutobahnClient.get_case_status(unquote(case_number)) in ~w[OK NON-STRICT INFORMATIONAL]
      end
    end
  end

  defp extensions_for_case(case_id) do
    Enum.find_value(@extensions, fn {id_regex, extensions} ->
      Regex.match?(id_regex, case_id) && extensions
    end)
  end
end
