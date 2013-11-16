defmodule Hypnotoad.WebSocket do
  @behaviour :cowboy_websocket_handler
  use Hypnotoad.Common

  def init({:tcp, :http}, _Req, _Opts) do
  	{:upgrade, :protocol, :cowboy_websocket}
  end

  defrecordp :state, subscriptions: nil

  def websocket_init(_transport, req, _opts) do
  	{:ok, req, state(subscriptions: :ets.new(__MODULE__.Subscriptions, []))}
  end

  def websocket_handle({:text, msg}, req, s) do
  	packet = JSEX.decode!(msg)
    L.verbose "Received WebSocket request: ${packet}", packet: packet
    handle_command(packet["command"], packet)
  	{:ok, req, s}
  end

  def websocket_handle(_, req, s) do
  	{:ok, req, s}
  end

  def websocket_info({:send, packet}, req, s) do
  	L.verbose "Sending WebSocket response: ${packet}", packet: packet |> JSEX.encode! |> JSEX.decode!
  	{:reply, {:text, JSEX.encode!(packet)}, req, s}
  end


  def websocket_info({:subscribe, id, event, fun}, req, state(subscriptions: subscriptions) = s) do
  	try do
	  	:gproc_ps.subscribe(:l, event)
  	rescue _ ->
	    :ok
    end
  	:ets.insert(subscriptions, {event, {id, fun}})
  	{:ok, req, s}
  end

  def websocket_info({:gproc_ps_event, event, msg}, req, state(subscriptions: subscriptions) = s) do
  	L.verbose "Event ${event} fired, with ${msg}", event: event, msg: msg
  	:ets.lookup(subscriptions, event) 
  	|> Enum.each(fn({_, {_id, fun}}) ->
  	  fun.(msg)
    end)
  	{:ok, req, s}
  end
  def websocket_info(_info, req, s) do
  	{:ok, req, s}
  end

  def websocket_terminate(_reason, _req, _state) do
  	:ok
  end

  defp handle_command("info", packet) do
  	{:ok, keys} = :application.get_all_key(:hypnotoad)
  	version = "#{keys[:vsn]}"
  	reply(packet, [config: [path: Hypnotoad.path, version: version]])
  end

  defp handle_command("hosts", packet) do
  	hosts = Hypnotoad.Hosts.hosts 
  	|> Enum.map(fn({name, {info, facts}}) ->
  	  [name: "#{name}", value: Hypnotoad.Host.to_json(info), facts: Dict.to_list(facts)]
  	end)
  	reply(packet, [hosts: hosts])
  end

  defp handle_command("modules", packet) do
  	modules = Hypnotoad.Modules.modules 
  	|> Enum.map(fn({name, path}) ->
  	  [name: inspect(name), path: path, description: name.description]
  	end)
  	subscribe(packet, {Hypnotoad.Modules, :modules_reloaded}, fn(modules) ->
  	  modules = Enum.map(modules, fn({name, path}) ->
  	    [name: inspect(name), path: path, description: name.description]
      end)
      reply(packet, [modules: modules])
  	end)
  	reply(packet, [modules: modules])
  end

  defp handle_command("plans", packet) do
    subscribe(packet, Hypnotoad.Plan, fn(_) ->
      plans(packet)
    end)
    subscribe(packet, {Hypnotoad.Modules, :modules_reloaded}, fn(_) ->
      plans(packet)
    end)

    plans(packet)
  end

  defp handle_command("run_plan", packet) do
    server = Module.safe_concat([packet["plan"]])
    Hypnotoad.Plan.run(server)
    reply(packet, [status: "ok"])
  end

  defp handle_command("jobs", packet) do
    subscribe(packet, Hypnotoad.Job, fn(_) ->
      jobs(packet)
    end)
    jobs(packet)
  end

  defp handle_command("reload_modules", packet) do
    :gen_server.cast(Hypnotoad.Modules, :init)
  	reply(packet, [status: "ok"])
  end


  defp plans(packet) do
    plans = Enum.map(:gproc.lookup_local_properties(Hypnotoad.Plan), fn({_pid, opts}) ->
      [name: inspect(opts[:module]), description: opts[:module].description, status: "#{opts[:status]}"]
    end)
    reply(packet, [plans: plans])
  end

  defp jobs(packet) do
    jobs = Enum.filter_map(:gproc.lookup_local_properties(Hypnotoad.Job), fn({_pid, opts}) ->
      inspect(Hypnotoad.Plan.module(opts[:plan])) == packet["plan"]
    end, fn({_pid, opts}) ->
      host = opts[:host]
      unless is_binary(host), do: host = inspect(host)
      options = opts[:options]
      if options == [], do: options = [{}]
      [module: inspect(opts[:module]), options: options, host: host,
       status: "#{opts[:status]}", output: opts[:output]]
    end)
    reply(packet, [jobs: jobs])
  end

  defp reply(packet, response) do
  	id = packet["id"]
  	response = Dict.put(response, "id", id)
  	send(response)
  end
  defp send(packet) do
  	self <- {:send, packet}
  end

  defp subscribe(packet, event, fun) do
  	self <- {:subscribe, packet["id"], event, fun}
  end
end