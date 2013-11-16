defmodule Hypnotoad.Test.KeyManager do
  use Hypnotoad.TestCase

  test "vagrant key should be loaded" do
  	path = Path.absname("test/fixtures/default/.keys/vagrant")
  	assert [{^path, _}] = Hypnotoad.KeyManager.keys
  end
end
