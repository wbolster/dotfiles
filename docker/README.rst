======
docker
======

credentials store
=================

store credentials in keyring used by gnome etc.

https://docs.docker.com/engine/reference/commandline/login/#credentials-store

arch aur packages:

- ``docker-credential-secretservice-bin``
- ``docker-credential-secretservice``

in ``~/.docker/config.json``::

  {
    "credsStore": "secretservice"
  }


dns via systemd-resolved
========================

docker does not play nice with ``systemd-resolved``, especially when running in ``resolv.conf`` stub mode. this manifests as local lookup and dns from vpn connections not working inside containers, etc.

configure docker ``/etc/docker/daemon.json`` to use a dns server on its default bridge network::

  {
    "dns": [
      "172.17.0.1"
    ]
  }

to make ``systemd-resolved`` also listen there, edit ``/etc/systemd/resolved.conf``::

  DNSStubListenerExtra=172.17.0.1

then::

  sudo systemctl restart docker.service systemd-resolved.service

alternatively, `dnsmasq can proxy from the bridge network`__. in ``/etc/dnsmasq.conf``::

  interface=docker0
  except-interface=lo
  bind-interfaces

then::

  systemctl enable --now dnsmasq

__ https://imagineer.in/blog/docker-container-dns-issue-in-airgapped-network/


keyboard shortcut to detach
===========================

by default docker uses ``ctrl-p``, which is annoying, since it’s also used to navigate readline history, e.g. in a shell.

in ``~/.docker/config.json``::

  {
    "detachKeys": "ctrl-z,ctrl-z,ctrl-z"
  }
