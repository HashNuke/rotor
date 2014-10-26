defmodule Rotor.Mixfile do
  use Mix.Project

  def project do
    [app: :rotor,
     version: "0.3.0",
     elixir: ">= 1.0.0",
     description: description,
     package: package,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [:logger],
     mod: {Rotor, []}]
  end


  defp description do
    """
    Rotor is a build system for Elixir projects. Use it to compile things, run commands or do anything when files change.
    """
  end


  defp package do
    [
      contributors: ["Akash Manohar J"],
      licenses: ["MIT"],
      links: %{ "GitHub" => "https://github.com/HashNuke/rotor" }
    ]
  end


  defp deps do
    []
  end
end
