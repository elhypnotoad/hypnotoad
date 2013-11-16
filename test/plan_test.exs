defmodule Hypnotoad.Test.Plan do
  use Hypnotoad.TestCase

  test "only modules with plan: true are plans" do
    assert [{pid, status: :ready, module: TargetSystem}] = :gproc.lookup_local_properties(Hypnotoad.Plan)
    assert is_pid(pid)
    assert Process.alive?(pid)
  end
end
