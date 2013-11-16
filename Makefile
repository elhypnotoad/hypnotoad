UNAME_S := $(shell uname -s)
ifeq ($(UNAME_S),Linux)
    VMWARE=vmware_workstation
endif
ifeq ($(UNAME_S),Darwin)
    VMWARE=vmware_fusion
endif

HYPNOTOAD_PATH ?= example

.PHONY: test all

all:
	@mix compile

vagrant.up:
	@mkdir -p ~/.vagrant_vmware
	@VAGRANT_VMWARE_CLONE_DIRECTORY=~/.vagrant_vmware vagrant up --provider=$(VMWARE)

vagrant.destroy:
	@vagrant destroy

test:
	@mix test --no-start

start:
	@HYPNOTOAD_PATH=$(HYPNOTOAD_PATH) mix run --no-halt