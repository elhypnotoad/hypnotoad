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

  defp start_module(host, module, args, extra_req // nil) do
    case extra_req do
      {m, a} -> extra_req = {m, a, nil}
      _ -> :ok
    end
    Process.put(:host, host)
    result = if module.before_filter(args) do
      reqs = if extra_req do
        [extra_req|module.requirements(args)]
      else
        module.requirements(args)
      end
      requirements = Enum.reduce(reqs, [], fn({req_mod, req_args, parent}, acc) ->
        if start_module(host, req_mod, req_args, parent) do
          [{req_mod, req_args}|acc]
        else
          acc
        end
      end)
      start_job(host, module, args, requirements)
      true
    else
      false
    end
    Process.delete(:host)
    result
  end

  def handle_cast(:run, state(module: module, done_jobs: done_jobs, running?: false) = s) do
    update_status(:preparing, s)
    lc {_, _, _, job} inlist done_jobs, do: Hypnotoad.Job.done(job)
    # Start jobs
    Enum.each(Hypnotoad.Hosts.hosts, fn({host, _}) ->
      try do
        :gproc_ps.subscribe(:l, {Hypnotoad.Job, host, :success})
        :gproc_ps.subscribe(:l, {Hypnotoad.Job, host, :failed})
        :gproc_ps.subscribe(:l, {Hypnotoad.Job, host, :excluded})
      rescue _ ->
      end
      start_module(host, module, [])
    end)
    :gen_server.cast(self, :ready)
    {:noreply, state(s, jobs: [], done_jobs: [], running?: true, failed: nil)}
  end

  def handle_cast(:run, state() = s) do
    {:noreply, s}
  end

  def handle_cast(:ready, state(jobs: jobs) = s) do
    update_status(:running, s, start_timestamp: Hypnotoad.Utils.timestamp)
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
        update_status(:failed, s, end_timestamp: Hypnotoad.Utils.timestamp)
      jobs == [] and failed ->
        update_status(:failed, s, end_timestamp: Hypnotoad.Utils.timestamp)
      jobs == [] ->
        update_status(:done, s, end_timestamp: Hypnotoad.Utils.timestamp)
      true ->
        :ok
    end
    {:noreply, state(s, jobs: jobs, done_jobs: done_jobs, running?: jobs != [], failed: failed || (status == :failed))}
  end

  def handle_info({:"DOWN", _ref, _type, pid, info}, state(jobs: jobs, running?: true) = s) do
    {host, module, options, _pid} = Enum.find(jobs, fn({_, _, _, pid1}) -> pid1 == pid end)
    L.error "Job ${module} ${options} at host ${host} exited with ${info}", module: module, options: options, host: host, info: info
    {:noreply, s}
  end

  def handle_info(_, state() = s) do
    {:noreply, s}
  end

  defp update_status(type, state(module: module), extra // []) do
    props = :gproc.lookup_local_properties(__MODULE__)
    start_timestamp = props[self][:start_timestamp]
    :gproc.unreg({:p,:l, __MODULE__})
    :gproc.add_local_property(__MODULE__, Keyword.merge([module: module, status: type, start_timestamp: start_timestamp], extra))
    :gproc_ps.publish(:l, Hypnotoad.Plan, self)
  end

  defp start_job(host, module, options, reqs) do
    {:ok, pid} = Hypnotoad.Job.start(host: host, module: module, requirements: reqs, options: options, plan: self)
    Process.monitor(pid)
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