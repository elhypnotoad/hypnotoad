defprotocol Hypnotoad.Host do
  @type t

  @spec connect!(t) :: t
  def connect!(t)

  @spec execute(t, String.t, term) :: term
  def execute(t, cmd, ref)

  @spec upload(t, Path.t, String.t, term) :: term
  def upload(t, file, content, ref)

  @spec download(t, Path.t, term) :: term
  def download(t, file, ref)

  @spec forward(t, Keyword.t) :: term
  def forward(t, options)

  @spec stop_forward(t, term) :: term
  def stop_forward(t, ref)

  @spec to_json(t) :: term
  def to_json(t)
end