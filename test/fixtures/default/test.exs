defmodule Package do
  use Hypnotoad
  @shortdoc "Manages operating system packages (install, uninstall)"

  def test(opts) do
    lock :apt do
      {status, _} = shell("dpkg -l #{opts[:name]}", status: true)
    end
    status == 0
  end

  def run(opts) do
    lock :apt do
      shell("apt-get install -y #{opts[:name]}")
    end
  end
end

defmodule TargetSystem do
  use Hypnotoad, plan: true
  @shortdoc "This is my deployment"

  requires Package, name: "lxc-docker-0.6.6"

end