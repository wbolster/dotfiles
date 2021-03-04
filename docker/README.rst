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


keyboard shortcut to detach
===========================

by default docker uses ``ctrl-p`` is used, which is annoying, since
it’s also used to navigate readline history, e.g. in a shell.

in ``~/.docker/config.json``::

  {
    "detachKeys": "ctrl-z,ctrl-z,ctrl-z"
  }
