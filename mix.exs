defmodule MintWebSocket.MixProject do
  use Mix.Project

  def project do
    [
      app: :mint_web_socket,
      version: "0.1.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:mint, "~> 1.0"}
    ]
  end
end
