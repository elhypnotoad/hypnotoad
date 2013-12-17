defmodule Hypnotoad.Mixfile do
  use Mix.Project

  def project do
    [ app: :hypnotoad,
      version: "0.0.1",
      elixir: ">= 0.12.0",
      build_per_environment: true,
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [ applications: %w(ssh gproc exlogger cowboy exconfig)a ++ applications(Mix.env),
      env: [http_port: 15080],
      mod: { Hypnotoad, [] }]
  end

  defp applications(env) when env in %w(dev), do: %w(exreloader)a
  defp applications(_), do: []

  defp deps do
    [
     {:gproc,    github: "uwiger/gproc"},
     {:exlogger, github: "ElixirWerkz/exlogger"},
     {:cowboy,   github: "extend/cowboy"},
     {:jsex,     github: "talentdeficit/jsex"},
     {:socket,   github: "meh/elixir-socket"},
     {:exconfig, github: "yrashk/exconfig"},
    ] ++ deps(Mix.env)
  end

  defp deps(env) when env in %w(dev) do
    [
      {:exreloader, github: "yrashk/exreloader"}
    ]
  end
  defp deps(_), do: []
end
