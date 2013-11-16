# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "ubuntu_raring64"
  config.vm.box_url = "https://s3.amazonaws.com/life360-vagrant/raring64.box"
  config.vm.hostname = "hypnotoad"
  # Host SSH
  config.vm.network :forwarded_port,
                    guest: 22,
                    host: 13222,
                    id: "ssh",
                    auto_correct: true
end
