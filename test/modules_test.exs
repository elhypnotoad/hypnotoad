defmodule Hypnotoad.Test.Modules do
  use Hypnotoad.TestCase

  defmodule RequiresExpr do
  	use Hypnotoad

	  requires Some, time: :erlang.now
  end

  test "requirements can be passed as expressions" do
  	# the following is always valid because :erlang.now always returns unique values
    assert Enum.find(RequiresExpr.requirements, fn({Some, _}) -> true; (_) -> false end) != 
           Enum.find(RequiresExpr.requirements, fn({Some, _}) -> true; (_) -> false end)
  end

  defmodule DefaultAttributes do
  	use Hypnotoad, default_attributes: [version: "0.6.6"]

   	requires Package, name: "lxc-docker-#{attributes[:version]}"
    requires SomePackage do
      if attributes[:extra_package] do
        [name: "my-package"]
      end
    end
  end

  test "attributes can be passed to requirements" do
    assert {Package, [name: "lxc-docker-0.6.7"]} = Enum.find(DefaultAttributes.requirements(version: "0.6.7"), fn({Package, _}) -> true; (_) -> false end)
  end

  test "attributes can be set to defaults" do
    assert {Package, [name: "lxc-docker-0.6.6"]} = Enum.find(DefaultAttributes.requirements, fn({Package, _}) -> true; (_) -> false end)
  end

  test "requirements can be skipped depending on attributes" do
    assert {SomePackage, [name: "my-package"]} = Enum.find(DefaultAttributes.requirements(extra_package: true), fn({SomePackage, _}) -> true; (_) -> false end)    
    assert nil = Enum.find(DefaultAttributes.requirements, fn({SomePackage, _}) -> true; (_) -> false end)    
  end

end
