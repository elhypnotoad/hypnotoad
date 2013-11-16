defmodule Hypnotoad.Common do
  defmacro __using__(_opts) do
  	quote do
  	  use ExLogger, as: L
  	end
  end
end