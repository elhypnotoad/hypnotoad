defmodule Hypnotoad.Test.Hosts do
  use Hypnotoad.TestCase

  test "vagrant hosts should be loaded" do
  	assert Hypnotoad.Host.SSH[host: "127.0.0.1", port: 10222, user: "vagrant", _: _] = Hypnotoad.Hosts.host(:vagrant_host)
  	assert Hypnotoad.Host.SSH[host: "127.0.0.1", port: 12222, user: "root", password: "screencast",  _: _] = Hypnotoad.Hosts.host(:vagrant_container)
  end
end
