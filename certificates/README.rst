============
certificates
============

custom certificate authority
============================

custom ca, to be trusted by most software installed on a system, e.g. web browsers, http clients, docker, etc.

debian/ubuntu::

  file=custom.ca.crt  # must have .crt extension

  dir=/usr/local/share/ca-certificates/extra
  sudo install -d -m755 "$dir"
  sudo install -m644 "$file" "${dir}/${file}"
  sudo update-ca-certificates

  sudo dpkg-reconfigure ca-certificates  # alternatively

arch::

  file=custom.ca.crt

  sudo install -m644 "${file}" "/etc/ca-certificates/trust-source/anchors/${file}"
  sudo update-ca-trust

fedora/centos/rhel::

  file=custom.ca.crt

  sudo install -m644 "${file}" "/etc/pki/ca-trust/source/anchors/${file}"
  sudo update-ca-trust
