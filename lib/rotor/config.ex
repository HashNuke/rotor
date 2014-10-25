defmodule Rotor.Config do

  defmacro __using__(_opts) do
    quote do
      import Rotor.BasicRotors
    end
  end

end
