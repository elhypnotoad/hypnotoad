defmodule Hypnotoad.Hosts do
  use Hypnotoad.Common
  use GenServer.Behaviour

  @default_type :ssh
  @types [ssh: Hypnotoad.Host.SSH]

  def start_link do
  	:gen_server.start_link {:local, __MODULE__}, __MODULE__, [], []
  end

  def host(name) do
    :gen_server.call(__MODULE__, {:host, name})
  end

  def hosts do
    :gen_server.call(__MODULE__, :hosts)
  end

  def facts(host) do
    :gen_server.call(__MODULE__, {:facts, host})
  end

  defrecordp :state, hosts: []

  def init(_) do
  	:gen_server.cast(self, :init)
  	{:ok, state()}
  end

  def handle_cast(:init, state()) do
    {hosts, _} = Path.join(Hypnotoad.path, "hosts") |> File.read! |> Code.eval_string()
    hosts =
  	Enum.map(hosts, fn
      ({{name, opts}, facts}) when is_list(facts) ->
        type = opts[:type] || @default_type
        {name, {@types[type].new(opts), facts}}
      ({name, opts}) when is_list(opts) ->
        type = opts[:type] || @default_type
        {name, {@types[type].new(opts), []}}
    end)
    L.debug "Loaded hosts ${hosts}", hosts: hosts
  	{:noreply, state(hosts: hosts)}
  end

  def handle_call({:add_fact, host, fact}, _from, state(hosts: hosts) = s) do
    {host_data, facts} = hosts[host]
    facts = [fact|facts]
    hosts = Dict.put(hosts, host, {host_data, facts})
    {:reply, :ok, state(s, hosts: hosts)}
  end

  def handle_call({:host, name}, _from, state(hosts: hosts) = s) do
    {info, _} = hosts[name]
    {:reply, info, s}
  end

  def handle_call({:facts, name}, _from, state(hosts: hosts) = s) do
    {_, facts} = hosts[name]
    {:reply, facts, s}
  end

  def handle_call(:hosts, _from, state(hosts: hosts) = s) do
    {:reply, hosts, s}
  end

end