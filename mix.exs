defmodule MintWebSocket.MixProject do
  use Mix.Project

  @source_url "https://github.com/elixir-mint/mint_web_socket"

  def project do
    [
      app: :mint_web_socket,
      version: "1.0.3",
      elixir: "~> 1.6",
      elixirc_paths: elixirc_paths(Mix.env()),
      erlc_paths: erlc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      test_coverage: [tool: ExCoveralls],
      preferred_cli_env: [
        coveralls: :test,
        "coveralls.html": :test,
        "coveralls.github": :test,
        docs: :dev
      ],
      dialyzer: [
        plt_local_path: "priv/plts",
        plt_add_apps: [:mix]
      ],
      package: package(),
      description: description(),
      source_url: @source_url,
      name: "MintWebSocket",
      docs: docs()
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:mint, "~> 1.4 and >= 1.4.1"},
      {:ex_doc, "~> 0.24", only: [:dev], runtime: false},
      {:castore, ">= 0.0.0", only: [:dev]},
      {:jason, ">= 0.0.0", only: [:dev, :test]},
      {:cowboy, "~> 2.9", only: [:test]},
      {:gun, "== 2.0.0-rc.2", only: [:test]},
      {:excoveralls, "~> 0.14", only: [:test]},
      {:dialyxir, "~> 1.2", only: [:dev, :test], runtime: false}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/fixtures"]
  defp elixirc_paths(_), do: ["lib"]

  defp erlc_paths(:test), do: ["src", "test/compare"]
  defp erlc_paths(_), do: ["src"]

  defp package do
    [
      name: "mint_web_socket",
      files: ~w(lib .formatter.exs mix.exs README.md),
      licenses: ["Apache-2.0"],
      links: %{
        "GitHub" => @source_url,
        "Changelog" => @source_url <> "/blob/main/CHANGELOG.md"
      }
    ]
  end

  defp description do
    "HTTP/1 and HTTP/2 WebSocket support for Mint"
  end

  defp docs do
    [
      deps: [],
      language: "en",
      formatters: ["html"],
      main: Mint.WebSocket,
      extras: [
        "CHANGELOG.md"
      ],
      skip_undefined_reference_warnings_on: [
        "CHANGELOG.md"
      ]
    ]
  end
end
