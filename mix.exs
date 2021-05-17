defmodule MintWebSocket.MixProject do
  use Mix.Project

  def project do
    [
      app: :mint_web_socket,
      version: "0.1.0",
      elixir: "~> 1.8",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.html": :test,
        "coveralls.github": :test
      ]
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:mint,
       git: "https://github.com/the-mikedavis/mint.git",
       ref: "46a5d290e728277068c389456ec3c235f5ece31e"},
      {:castore, ">= 0.0.0", only: [:dev]},
      {:jason, ">= 0.0.0", only: [:dev, :test]},
      {:excoveralls, "~> 0.14", only: [:test]}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/fixtures"]
  defp elixirc_paths(_), do: ["lib"]
end
