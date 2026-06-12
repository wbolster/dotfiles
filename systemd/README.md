=======
systemd
=======

systemd-tmpfiles
================

see systemd-tmpfiles(8) and tmpfiles.d(5)

::

  systemctl --user enable systemd-tmpfiles-clean.timer

to test::

  SYSTEMD_LOG_LEVEL=debug systemd-tmpfiles --user --clean
