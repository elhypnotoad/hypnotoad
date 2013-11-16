defmodule Hypnotoad.TestCase do
  use ExUnit.CaseTemplate


  using _options do
    quote do
      import Hypnotoad.TestCase
    end
  end

  setup_all _context do
    :application.load(:exlogger)
    :application.set_env(:exlogger, :backends, [])
    System.put_env("HYPNOTOAD_PATH", Path.join([__DIR__, "fixtures", "default"]))
    System.put_env("HYPNOTOAD_PORT", "15081")
    :ok = Application.Behaviour.start(:hypnotoad)
    case Hypnotoad.Connection.execute(:vagrant_host, "test -e /docker_installed", status: true) do
      {0, _} -> :ok
      {_, _} -> IO.puts "Docker is not (fully) provisioned on the test host, this might take a few minutes..."
    end
    Hypnotoad.Connection.execute(:vagrant_host, """)
    	if ! [ -e /docker_installed ]; then
    	  wget -q -O - https://get.docker.io/gpg | apt-key add -
    	  echo deb http://get.docker.io/ubuntu docker main > /etc/apt/sources.list.d/docker.list
          apt-get update -qq
          apt-get install -q -y --force-yes lxc-docker-0.6.6
          docker pull dhrp/sshd
          touch /docker_installed
      fi
    """
    :ok
  end

  teardown_all _context do
  	Hypnotoad.Connection.execute(:vagrant_host, "docker stop sshd && docker rm sshd")
    Enum.each(Hypnotoad.Modules.modules, fn({module, _}) ->
      :code.delete(module)
      :code.purge(module)
    end)
    :application.stop(:hypnotoad)
    :application.stop(:cowboy)
    :application.stop(:ranch)
    :ok
  end

  defmacro in_container(do: body) do
  	quote do
  	  Hypnotoad.Connection.execute(:vagrant_host, "docker stop sshd && docker rm sshd")
  	  Hypnotoad.Connection.execute(:vagrant_host, "docker run -d -name sshd -p 2222:22 dhrp/sshd")
  	  :timer.sleep 500
  	  unquote(body)
  	  Hypnotoad.Connection.execute(:vagrant_host, "docker stop sshd && docker rm sshd")
  	end
  end

end

ExUnit.start
