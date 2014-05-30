ExUnit.start

outputs_dir = "test/samples/outputs"
unless File.dir?(outputs_dir) do
  File.mkdir(outputs_dir)
end
