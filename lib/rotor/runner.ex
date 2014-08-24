defmodule Rotor.Runner do
  require Logger

  def run(name, changed_files, all_files) do
    Logger.debug "Run rotor function for #{name}"
    group = Rotor.group_info name

    try do
      apply group.rotor_fn, [changed_files, all_files]
    rescue
      error ->
        Logger.debug "Error running rotor function for group: #{name}"
        IO.inspect error
    end
  end
end
