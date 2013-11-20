defmodule Hypnotoad.Test.Semaphore do
  use Hypnotoad.TestCase

  test "will not lock until counter is less than max" do
  	ref = make_ref
  	Hypnotoad.Semaphore.start(ref, 10)
  	Enum.each(1..10, fn _ -> assert :ok = Hypnotoad.Semaphore.lock(ref, 100) end)
  	assert :timeout = Hypnotoad.Semaphore.lock(ref, 100)
  	assert :ok = Hypnotoad.Semaphore.unlock(ref)
  	assert :ok = Hypnotoad.Semaphore.lock(ref, 100)
  	assert :timeout = Hypnotoad.Semaphore.lock(ref, 100)
  end
end
