defmodule ForbiddenHandler do
  @moduledoc """
  A Cowboy HTTP handler that serves a GET request in the test suite
  and returns a 403 status code.

  See https://http.cat/403 :)
  """

  def init(req, state) do
    req = :cowboy_req.reply(403, %{"content_type" => "text/plain"}, "Forbidden.", req)

    {:ok, req, state}
  end
end
