defmodule Hypnotoad.Plan do

  use GenServer.Behaviour
  use Hypnotoad.Common

  def start(opts) do
    :supervisor.start_child(Hypnotoad.Plan.Sup, [opts])
  end

  def start_link(opts) do
    :gen_server.start_link {:local, opts[:module]}, __MODULE__, opts, []
  end

  def module(server) do
    :gen_server.call(server, :module)
  end

  def run(server) do
    :gen_server.cast(server, :run)
  end

  defrecordp :state, module: nil, jobs: [], done_jobs: [], running?: false, failed: false

  def init(opts) do
    :gproc.add_local_property(__MODULE__, Keyword.put(opts, :status, :ready))
    {:ok, state(module: opts[:module])}
  end

  def handle_call(:module, _from, state(module: module) = s) do
    {:reply, module, s}
  end

  defp start_module(host, module, args) do
    start_job(host, module, args, module.requirements(args))
    lc {req_mod, req_args} inlist module.requirements(args) do
      start_module(host, req_mod, req_args)
    end
  end

  def handle_cast(:run, state(module: module, done_jobs: done_jobs) = s) do
    update_status(:preparing, s)
    lc {_, _, _, job} inlist done_jobs, do: Hypnotoad.Job.done(job)
    # Start jobs
    Enum.each(Hypnotoad.Hosts.hosts, fn({host, _}) ->
      try do
        :gproc_ps.subscribe(:l, {Hypnotoad.Job, host, :success})
        :gproc_ps.subscribe(:l, {Hypnotoad.Job, host, :failed})
      rescue _ ->
      end
      start_module(host, module, [])
    end)
    :gen_server.cast(self, :ready)
    {:noreply, state(s, jobs: [], done_jobs: [], running?: true, failed: nil)}
  end

  def handle_cast(:ready, state(jobs: jobs) = s) do
    update_status(:running, s)
    lc {_, _, _, job} inlist jobs, do: Hypnotoad.Job.ready(job)
    {:noreply, s}
  end

  def handle_cast({:new_job, pid}, state(jobs: jobs) = s) do
    {:noreply, state(s, jobs: Enum.uniq([pid|jobs]))}
  end

  def handle_info({:gproc_ps_event, {Hypnotoad.Job, host, status}, {module, options}}, state(jobs: jobs, done_jobs: done_jobs, running?: true, failed: failed) = s) do
    {_, _, _, pid} = Enum.find(jobs, fn({host1, module1, options1, _pid}) -> host1 == host and module1 == module and options1 == options end)
    jobs = jobs -- [{host, module, options, pid}]
    done_jobs = [{host, module, options, pid}|done_jobs]
    cond do
      jobs == [] and status == :failed ->
        update_status(:failed, s)
      jobs == [] and failed ->
        update_status(:failed, s)
      jobs == [] ->
        update_status(:done, s)
      true ->
        :ok
    end
    {:noreply, state(s, jobs: jobs, done_jobs: done_jobs, running?: jobs != [], failed: failed || (status == :failed))}
  end

  def handle_info(_, state() = s) do
    {:noreply, s}
  end

  defp update_status(type, state(module: module)) do
    :gproc.unreg({:p,:l, __MODULE__})
    :gproc.add_local_property(__MODULE__, [module: module, status: type])    
    :gproc_ps.publish(:l, Hypnotoad.Plan, self)
  end

  defp start_job(host, module, options, reqs) do
    {:ok, pid} = Hypnotoad.Job.start(host: host, module: module, requirements: reqs, options: options, plan: self)
    :gen_server.cast(self, {:new_job, {host, module, options, pid}})
  end

end

defmodule Hypnotoad.Plan.Sup do
  use Supervisor.Behaviour

  def start_link do
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def init([]) do
    children = [
      worker(Hypnotoad.Plan, [], restart: :transient)
    ]
    supervise(children, strategy: :simple_one_for_one)
  end
end