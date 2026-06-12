======
docker
======

notes on making docker work nicely in development environments like laptops.

secret-service credentials store
================================

store credentials in the ‘secret service’, i.e. the keyring used by gnome etc.

https://docs.docker.com/engine/reference/commandline/login/#credentials-store

- arch aur packages: ``docker-credential-secretservice-bin`` / ``docker-credential-secretservice``
- ubuntu package: ``apt install golang-docker-credential-helpers``

configure in ``~/.docker/config.json``::

  {
    "credsStore": "secretservice"
  }


dns via systemd-resolved
========================

docker does not play nice with ``systemd-resolved``, especially when running in ``resolv.conf`` stub mode. this manifests as local lookup and dns from vpn connections not working inside containers, etc.

method 1: extra systemd-resolved listener
-----------------------------------------

configure docker ``/etc/docker/daemon.json`` to use a dns server on its default bridge network::

  {
    "dns": [
      "172.17.0.1"
    ]
  }


create a drop-in directory for custom ``systemd-resolved`` configuration::

  sudo mkdir /etc/systemd/resolved.conf.d

create ``/etc/systemd/resolved.conf.d/custom.conf`` and make it listen on an extra ip address::

  [Resolve]
  DNSStubListenerExtra=172.17.0.1

then::

  sudo systemctl restart docker.service systemd-resolved.service

method 2: dnsmasq (old)
-----------------------

alternatively, `dnsmasq can proxy from the bridge network`__. in ``/etc/dnsmasq.conf``::

  interface=docker0
  except-interface=lo
  bind-interfaces

then::

  systemctl enable --now dnsmasq

however this approach suffers from some 🐔/🥚 issues because the `docker0` interface is not always available, requiring manual restarts, etc.

__ https://imagineer.in/blog/docker-container-dns-issue-in-airgapped-network/


keyboard shortcut to detach
===========================

by default docker uses ``ctrl-p``, which is annoying, since it’s also used to navigate readline history, e.g. in a shell.

in ``~/.docker/config.json``::

  {
    "detachKeys": "ctrl-z,ctrl-z,ctrl-z"
  }
