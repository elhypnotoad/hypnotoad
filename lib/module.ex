defmodule Hypnotoad.Module do

  use Hypnotoad.Common

  defmacro requires(name, opts, block_opts) do
    __requires__(name, opts, block_opts)
  end

  defmacro requires(name, opts // []) do
    cond do
      Keyword.has_key?(opts, :do) or Keyword.has_key?(opts, :only)  ->
        __requires__(name, [], opts)
      true ->
        __requires__(name, opts, do: nil)
    end
  end

  defp __requires__(name, opts, block_opts) do
    name = Macro.escape(name)
    opts = Macro.escape(opts)
    do_block = block_opts[:do]
    only_block = if Keyword.has_key?(block_opts, :only), do: Macro.escape(block_opts[:only]), else: true
    quote do
      import Hypnotoad.Module
      name = unquote(name)
      opts = unquote(opts)
      only = unquote(only_block)
      parent = Enum.first(@hypnotoad_requires_parent)
      @hypnotoad_requirements (quote do
        if unquote(only) do
          {unquote(name), unquote(opts), unquote(parent)}
        end
      end)
      @hypnotoad_requires_parent [{unquote(name), unquote(opts)}|@hypnotoad_requires_parent]
      unquote(do_block)
      @hypnotoad_requires_parent tl(@hypnotoad_requires_parent)
    end
  end

  def __using__(opts) do
    quote do
      use Hypnotoad.Common
      Module.register_attribute __MODULE__, :hypnotoad_module, persist: true, accumulate: false
      Module.register_attribute __MODULE__, :hypnotoad_requirements, persist: false, accumulate: true
      Module.register_attribute __MODULE__, :hypnotoad_requires_parent, persist: false, accumulate: false
      Module.register_attribute __MODULE__, :hypnotoad_default_attributes, persist: false, accumulate: false
      Module.register_attribute __MODULE__, :shortdoc, persist: true, accumulate: false

      @hypnotoad_module true
      @hypnotoad_default_attributes unquote(opts[:default_attributes] || [])
      @hypnotoad_requires_parent []

      def test(_opts), do: test
      def test, do: true
      def run(_opts), do: run
      def run, do: :ok
      def filter(_opts), do: filter
      def filter, do: true
      def before_filter(_opts), do: before_filter
      def before_filter, do: true

      defoverridable test: 0, test: 1, run: 0, run: 1, filter: 0, filter: 1, before_filter: 0, before_filter: 1

      def plan? do
        unquote(opts[:plan] || false)
      end

      def description do
        case __info__(:attributes)[:shortdoc] do
         nil -> ""
         [doc] -> doc
        end
      end

      import Hypnotoad.Module
      @before_compile Hypnotoad.Module
    end
  end

  defmacro __before_compile__(env) do
    requirements = Module.get_attribute env.module, :hypnotoad_requirements
    quote do
      def requirements(attributes // @hypnotoad_default_attributes) do
        var!(attributes) = Keyword.merge(@hypnotoad_default_attributes, attributes)
        var!(attributes)
        unquote(requirements) |> Enum.filter(&(not nil?(&1)))
      end

      defoverridable test: 0, test: 1, run: 0, run: 1, filter: 0, filter: 1, before_filter: 0, before_filter: 1

      def filter(opts) do
        super(Keyword.merge(@hypnotoad_default_attributes, opts))
      end

      def before_filter(opts) do
        super(Keyword.merge(@hypnotoad_default_attributes, opts))
      end

      def run(opts) do
        super(Keyword.merge(@hypnotoad_default_attributes, opts))
      end

      def test(opts) do
        super(Keyword.merge(@hypnotoad_default_attributes, opts))
      end

    end
  end

  def lock(name) do
    log "Locking #{inspect(name)}"
    :global.set_lock({name, self})
    L.debug "Locked ${name} for ${pid}", name: name, pid: self
    log "Locked #{inspect(name)}"
  end

  def unlock(name) do
    :global.del_lock({name, self})
    L.debug "Unlocked ${name} by ${pid}", name: name, pid: self
    log "Unlocked #{inspect(name)}"
  end

  defmacro lock(name, do: body) do
    quote do
      lock(unquote(name))
      result = unquote(body)
      unlock(unquote(name))
      result
    end
  end

  def put(name, value) do
    Process.put(name, value)
  end

  def get(name) do
    Process.get(name)
  end

  def delete(name) do
    Process.delete(name)
  end

  def host_assert(fact) do
    :gen_server.call(Hypnotoad.Hosts, {:add_fact, host, fact})
  end

  def host_assertions do
    Hypnotoad.Hosts.facts(host)
  end

  def log(text) do
    :gproc_ps.publish(:l, {Hypnotoad.Shell, Process.get(:ref)}, "# #{text}\n")
  end

  def host do
    Process.get(:host)
  end

  def shell(command) do
    Hypnotoad.Connection.execute(host, command)
  end
  def shell(command, opts) do
    Hypnotoad.Connection.execute(host, command, opts)
  end

  def upload(file, content) do
    Hypnotoad.Connection.upload(host, file, content)
  end

  def download(file) do
    Hypnotoad.Connection.download(host, file)
  end

  defmacro tcpip_forward(options, do: block) do
    quote do
      ref = Hypnotoad.Connection.forward(host, unquote(options))
      result = unquote(block)
      Hypnotoad.Connection.stop_forward(host)
      result
    end
  end

end