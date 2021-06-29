defmodule MintWebSocket.MixProject do
  use Mix.Project

  def project do
    [
      app: :mint_web_socket,
      version: "0.1.0",
      elixir: "~> 1.8",
      elixirc_paths: elixirc_paths(Mix.env()),
      erlc_paths: erlc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.html": :test,
        "coveralls.github": :test,
        docs: :dev,
        bless: :test,
        credo: :test
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
      {:mint, "~> 1.0"},
      {:ex_doc, "~> 0.24", only: [:dev], runtime: false},
      {:castore, ">= 0.0.0", only: [:dev]},
      {:jason, ">= 0.0.0", only: [:dev, :test]},
      {:cowboy, "~> 2.9", only: [:test]},
      {:credo, "~> 1.0", only: [:test], runtime: false},
      {:excoveralls, "~> 0.14", only: [:test]},
      {:bless, "~> 1.0", only: [:test]}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/fixtures"]
  defp elixirc_paths(_), do: ["lib"]

  defp erlc_paths(:test), do: ["src", "test/fixtures"]
  defp erlc_paths(_), do: ["src"]
end
