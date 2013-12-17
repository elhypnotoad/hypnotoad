defmodule Hypnotoad.Modules do
  use Hypnotoad.Common
  use GenServer.Behaviour

  def start_link do
  	:gen_server.start_link {:local, __MODULE__}, __MODULE__, [], []
  end

  def modules do
    :gen_server.call(__MODULE__, :modules)
  end

  defrecordp :state, modules: []

  def init(_) do
  	:gen_server.cast(self, :init)
  	{:ok, state()}
  end

  def handle_cast(:init, state(modules: modules) = s) do
    Enum.each(modules, fn({module, _}) ->
      :code.delete(module)
      :code.purge(module)
    end)
  	modules = Path.join([Hypnotoad.path, "**/**.exs"])
  	|> Path.wildcard
  	|> Enum.map(fn(file) ->
      try do
        loaded = Code.load_file(file)
        Enum.reduce(loaded, [], fn({module, _binary}, acc) ->
          if hypnotoad_module?(module) do
            [{module, file}|acc]
          else
            acc
          end
        end)
      rescue e in [CompileError] ->
        unless e.message =~ %r{you must require Hypnotoad.Config before invoking the macro Hypnotoad.Config.config} do
          L.alert "Failed compiling ${file}, compile error ${e}", file: file, e: e
        end
        []
      end
  	end)
    |> List.flatten |> Enum.reverse
    Enum.each(modules, fn({module, _}) ->
      if module.plan?, do: Hypnotoad.Plan.start(module: module)      
    end)
    Enum.each(modules, fn({module, file}) ->
      L.debug "Loaded module ${module} (${file})", module: module, file: file
    end)
    :gproc_ps.publish :l, {__MODULE__, :modules_reloaded}, modules
  	{:noreply, state(s, modules: modules)}
  end

  def handle_call(:modules, _from, state(modules: modules) = s) do
    {:reply, modules, s}
  end

  defp hypnotoad_module?(module) do
    if function_exported?(module, :__info__, 1) do
      module.__info__(:attributes)[:hypnotoad_module] == [true]
    else
      false
    end
  end
end