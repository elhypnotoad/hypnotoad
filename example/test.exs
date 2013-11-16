defmodule Package do
  use Hypnotoad
  @shortdoc "Manages operating system packages (install, uninstall)"
  requires SystemInfo

  def test(opts) do
    lock :apt do
      {status, _} = shell("dpkg -l #{opts[:name]} >/dev/null 1>/dev/null 2>/dev/null", status: true)
    end
    if status == 0 do
      log "Package installed, nothing else to do\n"
    else
      log "Package is not installed\n"
    end
    status == 0
  end

  def run(opts) do
    lock :apt do
    	shell("DEBIAN_FRONTEND=noninteractive apt-get install -y #{opts[:name]}")
    end
  end
end


defmodule SystemInfo do
  use Hypnotoad

  def test(opts) do
    get(:collected?) || false
  end

  def run(opts) do
    os = shell("uname")
    host_assert {:os, os}
    lsb = shell(""")
    . /etc/lsb-release
    echo DISTRIB_ID=$DISTRIB_ID
    echo DISTRIB_RELEASE=$DISTRIB_RELEASE
    echo DISTRIB_CODENAME=$DISTRIB_CODENAME
    echo DISTRIB_DESCRIPTION=$DISTRIB_DESCRIPTION
    """
    Enum.each(String.split(lsb, "\n"), fn(line) ->
      [name, value] = String.split(line, "=")
      host_assert {name, value}
    end)
    put(:collected?, true)
  end
end

## Actual system

defmodule TargetSystem do
  use Hypnotoad, plan: true
  @shortdoc "This is my deployment"

  requires Package, name: "zzuf"
  requires Package, name: "ansible"

end