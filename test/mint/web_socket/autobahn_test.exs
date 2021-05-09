defmodule Mint.WebSocket.AutobahnTest do
  @moduledoc """
  A suite of tests against the Autobahn|Testsuite

  See https://github.com/crossbario/autobahn-testsuite
  """
  use ExUnit.Case, async: true

  @moduletag :autobahn

  def do_it(case_number \\ 1) do
    AutobahnClient.connect("/runCase?case=#{case_number}&agent=Mint")
    |> AutobahnClient.loop()
  end

  def update_reports do
    AutobahnClient.connect("/updateReports?agent=Mint")
    |> AutobahnClient.loop()
  end
end
