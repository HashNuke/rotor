defmodule Rotor.Mixfile do
  use Mix.Project

  def project do
    [app: :rotor,
     version: "0.1.0",
     elixir: "~> 0.13.3",
     description: description,
     package: package,
     deps: deps]
  end

  # Configuration for the OTP application
  #
  # Type `mix help compile.app` for more information
  def application do
    [applications: [],
     mod: {Rotor, []}]
  end


  defp description do
    """
    Rotor is a build system for Elixir projects. Use it to compile things, run commands or do anything that requires being run when files change.
    """
  end


  defp package do
    [
      contributors: ["Akash Manohar J"],
      links: %{ "GitHub" => "https://github.com/HashNuke/rotor" }
    ]
  end


  # Dependencies can be hex.pm packages:
  #
  #   {:mydep, "~> 0.3.0"}
  #
  # Or git/path repositories:
  #
  #   {:mydep, git: "https://github.com/elixir-lang/mydep.git", tag: "0.1"}
  #
  # Type `mix help deps` for more examples and options
  defp deps do
    []
  end
end
