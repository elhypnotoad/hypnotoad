defmodule Hypnotoad.Semaphore do

  use GenServer.Behaviour
  use Hypnotoad.Common

  def start(id, max) do
  	case :supervisor.start_child(Hypnotoad.Semaphore.Sup, Supervisor.Behaviour.worker(__MODULE__, [id, max], id: id)) do
  	  {:ok, pid} -> {:ok, pid}
  	  {:error, {:already_started, pid}} -> {:ok, pid}
  	end
  end

  def lock(id, timeout // :infinity) do
    pid = :gproc.lookup_local_name(id)
    try do
	  :gen_server.call(pid, :lock, timeout)
    catch :exit, {:timeout, _} ->
      :timeout
    end
  end

  def unlock(id, timeout // :infinity) do
    pid = :gproc.lookup_local_name(id)
    :gen_server.call(pid, :unlock, timeout)
  end

  def start_link(id, max) do
  	:gen_server.start_link __MODULE__, [id, max], []
  end

  defrecordp :state, id: nil, max: nil, counter: 0, pending: []

  def init([id, max]) do
  	:gproc.add_local_name(id)
  	{:ok, state(id: id, max: max)}
  end

  def handle_call(:lock, _from, state(counter: counter, max: max) = s) when counter < max do
  	{:reply, :ok, state(s, counter: counter + 1)}
  end
  def handle_call(:lock, from, state(pending: pending) = s) do
  	{:noreply, state(s, pending: [from|pending])}
  end

  def handle_call(:unlock, _from, state(pending: [from|tail]) = s) do
    :gen_server.reply(from, :ok)
    {:reply, :ok, state(s, pending: tail)}
  end

  def handle_call(:unlock, _from, state(counter: counter, max: max) = s) when counter <= max do
    {:reply, :ok, state(s, counter: counter - 1)}
  end

end

defmodule Hypnotoad.Semaphore.Sup do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def init([]) do
    children = [
    ]
    supervise(children, strategy: :one_for_one)
  end
end