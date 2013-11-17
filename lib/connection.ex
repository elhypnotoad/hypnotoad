defmodule Hypnotoad.Connection do

  def execute(host, cmd, opts // []) do
    {pid, opts} = connection(host, opts)
    :gen_server.call(pid, {:execute, cmd, opts}, :infinity)
  end

  def upload(host, file, content, opts // []) do
    {pid, opts} = connection(host, opts)
    :gen_server.call(pid, {:upload, file, content, opts}, :infinity)
  end

  def download(host, file, opts // []) do
    {pid, opts} = connection(host, opts)
    :gen_server.call(pid, {:download, file, opts}, :infinity)
  end

  def forward(host, opts) do
    {pid, _opts} = connection(host, [])
    :gen_server.call(pid, {:forward, opts}, :infinity)    
  end

  def stop_forward(host, ref) do
    {pid, _opts} = connection(host, [])
    :gen_server.call(pid, {:stop_forward, ref}, :infinity)    
  end

  defp connection(host, opts) do
    host_spec = Hypnotoad.Hosts.host(host)
    if nil?(host_spec) do
      raise ArgumentError, message: "host #{inspect(host)} not found"
    end
    case :supervisor.start_child(Hypnotoad.Connection.Sup, Supervisor.Behaviour.worker(Hypnotoad.Connection, [host_spec], id: host, restart: :transient)) do
      {:ok, pid} -> :ok
      {:error, {:already_started, pid}} -> :ok
    end
    opts = Keyword.put(opts, :ref, Process.get(:ref))
    {pid, opts}
  end

  use GenServer.Behaviour

  def start_link(conn) do
    :gen_server.start_link __MODULE__, conn, []
  end

  defrecordp :state, connection: nil

  def init(conn) do
    conn = Hypnotoad.Host.connect!(conn)
    {:ok, state(connection: conn)}
  end

  def handle_call({:execute, cmd, opts}, from, state(connection: connection) = s) do
    spawn_link(fn ->
      {status, data} = Hypnotoad.Host.execute(connection, cmd, opts[:ref])
      reply = if opts[:status] do
        {status, data}
      else
        data
      end
      :gen_server.reply(from, reply)
    end)
    {:noreply, s}
  end

  def handle_call({:upload, file, content, opts}, from, state(connection: connection) = s) do
    spawn_link(fn ->
      data = Hypnotoad.Host.upload(connection, file, content, opts[:ref])
      :gen_server.reply(from, data)
    end)
    {:noreply, s}
  end

  def handle_call({:download, file, opts}, from, state(connection: connection) = s) do
    spawn_link(fn ->
      data = Hypnotoad.Host.download(connection, file, opts[:ref])
      :gen_server.reply(from, data)
    end)
    {:noreply, s}
  end

  def handle_call({:forward, opts}, _from, state(connection: connection) = s) do
    {:reply, Hypnotoad.Host.forward(connection, opts), s}
  end

  def handle_call({:stop_forward, ref}, _from, state(connection: connection) = s) do
    {:reply, Hypnotoad.Host.stop_forward(connection, ref), s}
  end

end