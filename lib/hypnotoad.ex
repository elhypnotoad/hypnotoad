defmodule Hypnotoad do
  use Application.Behaviour
  use Hypnotoad.Common
  require Hypnotoad.Config

  defmacro __using__(opts) do
    Hypnotoad.Module.__using__(opts)
  end

  def start(_type, _args) do
  	{options, _args, _rest} = OptionParser.parse(System.argv)
  	:application.set_env(:hypnotoad, :path, options[:path] || System.get_env("HYPNOTOAD_PATH") || File.cwd!)
    {:ok, pid} = Hypnotoad.Sup.start_link
    start_http
    {:ok, pid}
  end

  def path do
  	{:ok, path} = :application.get_env(:hypnotoad, :path)
  	Path.absname(path)
  end

  defp start_http do
    dispatch = [
      {:_, [
        {"/", Hypnotoad.WebSocket, []},
      ]},
    ] |> :cowboy_router.compile

    {:ok, _} = :cowboy.start_http(Hypnotoad.HTTP, 100,
                                  [port: http_port],
                                  [env: [dispatch: dispatch]])
  end

  defp http_port do
    if (port = System.get_env("HYPNOTOAD_PORT")) do
      {port, _} = Integer.parse(port)
    end    
    env = :application.get_all_env(:hypnotoad)
    port || config.http_port || env[:http_port] || 15080
  end

  def config do
    specified_config = System.get_env("HYPNOTOAD_CONFIG")
    config_file = specified_config || Path.join(path, "config.exs")
    cond do
      File.exists?(config_file) -> Hypnotoad.Config.file!(config_file)
      not nil?(specified_config) and not File.exists?(specified_config) ->
        L.error "Config file #{specified_config} does not exist"
        :init.stop
      true ->
        Hypnotoad.Config.config do end
    end
  end

end
