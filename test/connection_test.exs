defmodule Hypnotoad.Test.Connection do
  use Hypnotoad.TestCase

  test "executes a command" do
  	assert Hypnotoad.Connection.execute(:vagrant_host, "uname") == "Linux"
  end

  test "executes a multi-line command" do
  	assert Hypnotoad.Connection.execute(:vagrant_host, """) == "Operating system: Linux"
  	echo -n "Operating system: "
  	uname
  	"""
  end

  test "executes a command under root" do
  	assert Hypnotoad.Connection.execute(:vagrant_host, "whoami") == "root"
  end

  test "passes the exit status correctly" do
  	assert {100, _} = Hypnotoad.Connection.execute(:vagrant_host, "test -e /does_not_exist || exit 100", status: true)
  end

  test "executes in a container" do
  	in_container do
  	  # The following assures that we're not talking to the host anymore as
  	  # /docker_installed should not be present there
  	  assert {1, _} = Hypnotoad.Connection.execute(:vagrant_container, "test -e /docker_installed", status: true)
  	end
  end

  test "uploads a file" do
    assert :ok = Hypnotoad.Connection.upload(:vagrant_host, "/home/vagrant/test_file", "content")
    assert Hypnotoad.Connection.download(:vagrant_host, "/home/vagrant/test_file") == "content"
  end

  test "uploads a file with sudo" do
    content = "#{:erlang.phash2(:erlang.now)}"
    assert :ok = Hypnotoad.Connection.upload(:vagrant_host, "/test_file1", content)
    assert Hypnotoad.Connection.download(:vagrant_host, "/test_file1") == content
  end

end
