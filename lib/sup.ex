defmodule Hypnotoad.Sup do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link(__MODULE__, [])
  end

  def init([]) do
    children = [
      worker(Hypnotoad.KeyManager, []),
      worker(Hypnotoad.Hosts, []),
      worker(Hypnotoad.Modules, []),
      supervisor(Hypnotoad.Host.SSH.Channel.Sup, []),
      supervisor(Hypnotoad.Connection.Sup, []),
      supervisor(Hypnotoad.Job.Sup, []),
      supervisor(Hypnotoad.Plan.Sup, []),
    ]

    supervise(children, strategy: :one_for_one)
  end
end