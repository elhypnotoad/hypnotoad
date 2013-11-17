UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    VMWARE=vmware_workstation
endif
ifeq ($(UNAME_S),Darwin)
    VMWARE=vmware_fusion
endif

HYPNOTOAD_PATH ?= example

.PHONY: test all iex

all:
	@mix deps.get
	@mix compile

vagrant.up:
	@mkdir -p ~/.vagrant_vmware
	@VAGRANT_VMWARE_CLONE_DIRECTORY=~/.vagrant_vmware vagrant up --provider=$(VMWARE)

vagrant.destroy:
	@vagrant destroy

test:
	@MIX_ENV=test mix deps.get
	@mix test --no-start

start:
	@HYPNOTOAD_PATH=$(HYPNOTOAD_PATH) mix run --no-halt

iex:
	@HYPNOTOAD_PATH=$(HYPNOTOAD_PATH) iex -S mix