defmodule Hypnotoad.Utils do
  def timestamp do
    {mega, secs, _micro} = :os.timestamp
    mega * 1_000_000 + secs
  end
end