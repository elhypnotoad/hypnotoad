defmodule Hypnotoad.KeyManager do
  @behaviour :ssh_client_key_api

  defdelegate [add_host_key(hostnames, key, connect_options),
               is_host_key(key, host, algorithm, connect_options)], to: :ssh_file

  def user_key(algorithm, connect_options) do
  	:gen_server.call(__MODULE__, {:user_key, algorithm, connect_options})
  end

  use GenServer.Behaviour

  def keys do
    :gen_server.call(__MODULE__, :keys)
  end

  def start_link do
  	:gen_server.start_link {:local, __MODULE__}, __MODULE__, [], []
  end

  defrecordp :state, keys: []

  def init(_) do
  	:gen_server.cast(self, :init)
  	{:ok, state()}
  end

  def handle_cast(:init, state()) do
  	keys = Path.join([Hypnotoad.path, ".keys", "**/**"])
  	|> Path.wildcard
  	|> Enum.reduce([], fn(file, acc) ->
  	  acc ++
  	  (:public_key.pem_decode(File.read!(file))
  	   |> Enum.filter(fn(key) -> is_record(key, :RSAPrivateKey) end)
  	   |> Enum.map(fn(key) -> {file, :public_key.pem_entry_decode(key)} end))
  	end)
  	{:noreply, state(keys: keys)}
  end

  def handle_call({:user_key, :"ssh-rsa", _connect_options}, _from, state(keys: keys) = s) do
  	{_file, first_key} = Enum.find(keys, fn({_file, key}) -> is_record(key, :RSAPrivateKey) end)
  	{:reply, {:ok, first_key}, s}
  end

  def handle_call({:user_key, :"ssh-dss", _connect_options}, _from, state(keys: _keys) = s) do
  	{:reply, {:error, :not_found}, s}
  end

  def handle_call(:keys, _from, state(keys: keys) = s) do
    {:reply, keys, s}
  end

end