defmodule HttpHandler do
  @moduledoc """
  A Cowboy HTTP handler that serves a GET request in the test suite
  """

  def init(req, state) do
    req = :cowboy_req.reply(200, %{"content_type" => "text/plain"}, "hi!", req)

    {:ok, req, state}
  end
end
