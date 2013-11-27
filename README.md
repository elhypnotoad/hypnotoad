# Hypnotoad
## Everybody Loves Server Orchestration


__WARNING__: Hypnotoad is an alpha version quality project, use at your own risk. Some features may be incomplete or buggy.

### Objective

Hypnotoad is a tool for orchestrating servers and their provisioning. There is a lot of similar tools out there (most notable ones include Chef, Bcfg2, Puppet, Ansible, SaltStack and others) but the author of this one claims he never heard of the NIH syndrome.

### Features

* Nothing to install on the servers
* Aggressive SSH multiplexing
* Concurrent execution, limited by dependencies and SSH limits
* Test-Run-Test workflow
* Simple and flexible locking primitives
* Modern Web UI with in-progress introspection
