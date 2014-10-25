defmodule Rotor.Config do

  def __using__(_opts) do
    quote do
      import Rotor.BasicRotors
    end
  end

end
