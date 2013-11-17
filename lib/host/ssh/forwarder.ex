defmodule Hypnotoad.Host.SSH.Forwarder do

  use Hypnotoad.Common
  use GenServer.Behaviour  

  def start(options) do
  	ref = make_ref
  	{:ok, _pid} = :ranch.start_listener(ref, 100, :ranch_tcp, [port: options[:listen_port]], __MODULE__, options)
  	ref
  end

  def stop(ref) do
  	:ranch.stop_listener(ref)
  end

  def start_link(ref, socket, transport, opts) do
  	:proc_lib.start_link __MODULE__, :init, [ref, socket, transport, opts]
  end

  defrecordp :state, ref: nil, socket: nil, transport: nil, channel: nil, opts: []

  def init(ref, socket, transport, opts) do
  	:ok = :proc_lib.init_ack({:ok, self})
  	:ok = :ranch.accept_ack(ref)
  	:ok = transport.setopts(socket, active: :once)
  	{:ok, channel} = :ssh_connection.direct_tcpip(opts[:connection]._connection, String.to_char_list!(opts[:host]),
			  		 opts[:port], opts[:orig_ip] || {0,0,0,0}, opts[:orig_port] || 0, opts[:timeout] || :infinity)
  	:gen_server.enter_loop(__MODULE__, [], state(ref: ref, socket: socket, transport: transport, channel: channel, opts: opts))
  end

  def handle_info({:tcp, _, data}, state(channel: channel, opts: opts, transport: transport, socket: socket) = s) do
  	:ssh_connection.send(opts[:connection]._connection, channel, data)
  	transport.setopts(socket, active: :once)
    {:noreply, s}
  end

  def handle_info({:ssh_cm, _, {:data, channel, _, data}}, state(channel: channel, transport: transport, socket: socket) = s) do
  	transport.send(socket, data)
  	{:noreply, s}
  end

  def handle_info({:tcp_closed, _}, state(opts: opts, channel: channel) = s) do
  	:ssh_connection.close(opts[:connection]._connection, channel: channel)
  	{:stop, :normal, s}
  end

  def handle_info({:ssh_cm, _, {:eof, channel}}, state(channel: channel) = s) do
  	{:stop, :normal, s}
  end

end