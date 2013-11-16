defrecord Hypnotoad.Host.SSH, host: nil, port: nil, user: nil, password: nil, _connection: nil

defimpl Hypnotoad.Host, for: Hypnotoad.Host.SSH do
  alias Hypnotoad.Host.SSH

  def connect!(SSH[host: host, port: port, user: user, password: nil] = ssh) do
    {:ok, c} = :ssh.connect(String.to_char_list!(host), port,
    	                   [user_interaction: false, user: String.to_char_list!(user), key_cb: Hypnotoad.KeyManager, silently_accept_hosts: true])
    ssh.update(_connection: c)
  end

  def connect!(SSH[host: host, port: port, user: user, password: password] = ssh) do
    {:ok, c} = :ssh.connect(String.to_char_list!(host), port,
                         [user_interaction: false, user: String.to_char_list!(user), key_cb: Hypnotoad.KeyManager, silently_accept_hosts: true,
                          password: String.to_char_list!(password)])
    ssh.update(_connection: c)
  end

  def execute(SSH[] = ssh, cmd, ref) do
  	{:ok, pid} = :supervisor.start_child(Hypnotoad.Host.SSH.Channel.Sup, [ssh])
  	Hypnotoad.Host.SSH.Channel.execute(pid, cmd, ref)
  end

  def upload(SSH[] = ssh, file, content, ref) do
    {:ok, pid} = :supervisor.start_child(Hypnotoad.Host.SSH.Channel.Sup, [ssh])
    Hypnotoad.Host.SSH.Channel.upload(pid, file, content, ref)
  end

  def download(SSH[] = ssh, file, ref) do
    {:ok, pid} = :supervisor.start_child(Hypnotoad.Host.SSH.Channel.Sup, [ssh])
    Hypnotoad.Host.SSH.Channel.download(pid, file, ref)
  end

  def to_json(SSH[] = ssh) do
    [host: ssh.host, port: ssh.port, user: ssh.user, password: ssh.password, type: "ssh"]
  end
end