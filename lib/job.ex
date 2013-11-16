defmodule Hypnotoad.Job do

  use GenServer.Behaviour
  use Hypnotoad.Common

  def start(opts) do
    case :supervisor.start_child(Hypnotoad.Job.Sup, Supervisor.Behaviour.worker(Hypnotoad.Job, [opts], id: opts, restart: :temporary)) do
      {:ok, pid} ->
        {:ok, pid}
      {:error, {:already_started, pid}} ->
        {:ok, pid}
    end
  end

  def start_link(opts) do
    :gen_server.start_link __MODULE__, opts, []
  end

  def ready(server) do
    :gen_server.cast(server, :ready)
  end

  def done(server) do
    :gen_server.cast(server, :done)
  end

  defrecordp :state, opts: [], host: nil, requirements: [], module: nil, options: [], output: ""

  def init(opts) do
    L.debug "Starting job ${opts}", opts: opts
    :gproc.add_local_property(__MODULE__, Keyword.put(opts, :status, :ready))
    :gproc_ps.subscribe(:l, {Hypnotoad.Job, opts[:host], :success})
    :gproc_ps.subscribe(:l, {Hypnotoad.Job, opts[:host], :failed})
    :gproc_ps.subscribe(:l, {Hypnotoad.Shell, self})
    {:ok, state(opts: opts, host: opts[:host], requirements: opts[:requirements], module: opts[:module], options: opts[:options])}
  end

  def handle_cast(:ready, state(requirements: requirements) = s) do
    if requirements == [] do
      :gen_server.cast(self, :run)
    else
      update_status(:pending, s, self)
    end
    {:noreply, s}
  end

  def handle_cast(:done, state() = s) do
    {:stop, :normal, s}
  end

  def handle_cast(:run, state(host: host, module: module, options: options) = s) do
    opts = options
    L.debug "Running job ${module} ${options} on host ${host}", module: module, options: options, host: host
    me = self
    spawn_link(fn ->
      Process.put(:ref, me)
      Process.put(:host, host)
      update_status(:testing, s, me)
      unless module.test(opts) do
        L.debug "Running job ${module} ${options} on host ${host}: test didn't pass", module: module, options: options, host: host
        update_status(:running, s, me)
        module.run(opts)
        L.debug "Running job ${module} ${options} on host ${host}: executed", module: module, options: options, host: host
        update_status(:post_test, s, me)
        if module.test(opts) do
          L.debug "Job ${module} ${options} succeeded on host ${host}", module: module, options: options, host: host
          update_status(:success, s, me)
        else
          L.debug "Job ${module} ${options} failed on host ${host}", module: module, options: options, host: host
          update_status(:failed, s, me)
        end
      else
        L.debug "Job ${module} ${options} already succeeded on host ${host}", module: module, options: options, host: host
        update_status(:success, s, me)
      end
    end)
    {:noreply, s}
  end

  def handle_cast({:update_status, type, state(opts: opts, module: module, options: options, host: host)}, state(output: output) = s) do
    :gproc.unreg({:p,:l, __MODULE__})
    :gproc.add_local_property(__MODULE__, Keyword.put(opts, :status, type) |> Keyword.put(:output, output)) 
    if type in %w(success failed)a do
      :gproc_ps.publish(:l, {Hypnotoad.Job, host, type}, {module, options})
    end
    :gproc_ps.publish(:l, Hypnotoad.Job, self)
    {:noreply, s}
  end

  def handle_info({:gproc_ps_event, {Hypnotoad.Job, _host, :success}, job}, state(module: _module, options: _options, requirements: requirements) = s) do
    new_requirements = requirements -- [job]
    if new_requirements != requirements and new_requirements == [] do
      :gen_server.cast(self, :run)
    end
    {:noreply, state(s, requirements: new_requirements)}
  end

  def handle_info({:gproc_ps_event, {Hypnotoad.Job, _host, :failed}, job}, state(module: _module, options: _options, requirements: requirements) = s) do
    if job in requirements do
      update_status(:failed, s, self)
    end
    new_requirements = requirements -- [job]
    s = state(s, requirements: new_requirements)
    {:noreply, s}
  end

  def handle_info({:gproc_ps_event, {Hypnotoad.Shell, pid}, data}, state(output: output) = s) when pid == self do
    s = state(s, output: output <> data)
    update_job(s)
    {:noreply, s}
  end

  defp update_job(state(opts: opts, output: output)) do
    props = :gproc.lookup_local_properties(__MODULE__)
    status = props[self][:status]
    :gproc.unreg({:p,:l, __MODULE__})
    :gproc.add_local_property(__MODULE__, Dict.put(opts, :output, output) |> Dict.put(:status, status)) 
    :gproc_ps.publish(:l, Hypnotoad.Job, self)
  end
  defp update_status(type, state() = s, me) do
    :gen_server.cast(me, {:update_status, type, s})
  end
end

defmodule Hypnotoad.Job.Sup do
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