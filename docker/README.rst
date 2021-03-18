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


systemd-resolved (dns)
======================

docker dns does not play nice with systemd-resolved, especially in ``resolv.conf`` stub mode, which can result in dns from vpn connections not working inside containers etc. there is a `workaround using dnsmasq to proxy`__, with this ``/etc/dnsmasq.conf``::

  interface=docker0
  except-interface=lo
  bind-interfaces

__ https://imagineer.in/blog/docker-container-dns-issue-in-airgapped-network/

to use it on the default docker bridge network, add this to ``/etc/docker/daemon.json``::

  {
    "dns": [
      "172.17.0.1"
    ]
  }

profit::

  systemctl enable --now dnsmasq
  systemctl restart docker


keyboard shortcut to detach
===========================

by default docker uses ``ctrl-p`` is used, which is annoying, since
it’s also used to navigate readline history, e.g. in a shell.

in ``~/.docker/config.json``::

  {
    "detachKeys": "ctrl-z,ctrl-z,ctrl-z"
  }
