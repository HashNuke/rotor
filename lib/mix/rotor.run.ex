defmodule Mix.Tasks.Rotor.Run do
  use Mix.Task

  @shortdoc "Run rotors"


  #TODO incomplete
  def run([]) do
    Application.start(:rotor)
    Rotor.Config.load_rotors
  end


  #TODO incomplete
  def run([path]) do
    Application.start(:rotor)
    Rotor.Config.load_rotors(path)
  end

end
