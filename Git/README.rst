==========
git config
==========

include the version controlled config from ``~/.gitconfig``. avoid symlinking, so that per-machine config can be added outside the version control for the main config. see below for examples.

::

  [include]
  path = Configuration/Git/config

custom configs per directory tree
=================================

useful for config that should apply to all repos kept under the same parent directory, e.g. all work repos, or when using multiple github accounts. does not require per repo setup, works also for new clones.

in ``~/.gitconfig``::

  [includeIf "gitdir:~/example/"]
  path = .gitconfig_example

in ``~/.gitconfig_example``::

  [user]
  email = someone@example

  [core]
  sshCommand = ssh -i ~/.ssh/id_ed25519_example -o 'IdentitiesOnly yes'


gnome libsecret credential storage
==================================

useful for remotes that don't use ssh key auth, e.g. https remotes

::

  [credential]
  helper = /usr/lib/git-core/git-credential-libsecret
