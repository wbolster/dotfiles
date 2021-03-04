============
certificates
============

custom certificate authority
============================

custom ca, to be trusted by most software installed on a system, e.g. web browsers, http clients, docker, etc.

debian/ubuntu::

  file=custom.ca.crt

  sudo install -d -m755 /usr/share/ca-certificates/extra
  sudo install -m644 "${file}" "/usr/share/ca-certificates/extra/${file}"
  sudo dpkg-reconfigure ca-certificates

arch::

  file=custom.ca.crt

  sudo install -m644 "${file}" "/etc/ca-certificates/trust-source/anchors/${file}"
  sudo update-ca-trust
