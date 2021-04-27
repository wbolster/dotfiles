=======
backups
=======

borg + borgmatic
================

- sample config in ``system.yaml``

- permissions::

    sudo chmod 644 /etc/borgmatic.d/*.yaml
    sudo chmod 600 /etc/borgmatic.d/*.key

- init a new repo::

    borg init --encryption repokey user@host:/.../example

- store the password in the ``.key`` file

- make backup using ``sudo borgmatic`` or via the backup helper in ``bin/``
