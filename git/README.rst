==========
git config
==========

custom configs per directory tree
=================================

useful for config that should apply to all repos kept under the same parent directory, e.g. all work repos, or when using multiple github accounts. does not require per repo setup, works also for new clones.

in ``~/.config/git/config``, or in ``~/.config/git/config-local`` to keep it it out of version control (that file is included in my setup)::

  [includeIf "gitdir/i:~/Projects/work/"]
  path = config-work

in ``~/.config/git/config-work``::

  [user]
  email = someone@example.org

  [core]
  sshCommand = ssh -o 'IdentityFile ~/.ssh/id_ed25519_work' -o 'IdentitiesOnly yes'


gnome libsecret credential storage
==================================

useful for remotes that don't use ssh key auth, e.g. https remotes

::

  [credential]
  helper = /usr/lib/git-core/git-credential-libsecret


custom remote urls
==================

::

  [url "ssh://git@gitea-host:port/user/…"]
  insteadOf = gitea:
