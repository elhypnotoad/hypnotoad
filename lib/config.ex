defmodule Hypnotoad.Config do
  use ExConfig.Object
  defproperty hosts, default: []
  defproperty http_port, default: 15080
end