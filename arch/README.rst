=============================
arch linux installation notes
=============================

installing on a reasonably modern laptop (uefi, ssd, …), e.g. dell xps

⚠ the machine will be completely wiped! ⚠

getting started
===============

- put disk in ahci mode instead of raid; dell xps
- disable secure boot (temporarily)
- delete/clear secure boot PK (and other keys)
- boot arch from boot media
- note: on dell xps, booting from usb sometimes only works when using a specific usb port!

keyboard
========

::

  loadkeys colemak  # 🤓
  # loadkeys us  # 😞

network
=======

wi-fi::

  iwctl station list
  iwctl station wlan0 get-networks
  iwctl station wlan0 connect <ESSID>

ntp::

  timedatectl set-ntp true

prepare for remote install (optional)::

  passwd  # temporary password
  ip addr show

…and continue from another machine::

  ssh -o PubkeyAuthentication=no root@...

disk layout
===========

show block devices::

  blkid
  lsblk
  lsblk --fs

choose the target disk::

  disk=/dev/nvme0n1

wipe disk using nvme sanitize::

  nvme id-ctrl --human-readable $disk  # look for sanicap
  nvme sanitize $disk --sanact=0x02
  watch --interval 1 nvme sanitize-log $disk

‼ wait until completed. status should *not* be ``0x102``, e.g.

::

  Sanitize Progress                      (SPROG) :  65535
  Sanitize Status                        (SSTAT) :  0x101

if not supported, try one of these::

  nvme format $disk --force --ses=2  # cryptographic erase
  nvme format $disk --force --ses=1  # user data erase

create a new gpt layout using discoverable partition types::

  partprobe
  wipefs $disk
  cat << EOF | sfdisk $disk
  label: gpt
  size=1GiB, type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, name="EFI"
  type=4F68BCE3-E8CD-4DB1-96E7-FBCAF984B709, name="encrypted-system"
  EOF

prepare efi partition (for ``/boot``)::

  mkfs.fat -F32 -n EFI /dev/disk/by-partlabel/EFI

prepare luks with multiple passphrases (e.g. qwerty and colemak)::

  blockdev=/dev/disk/by-partlabel/encrypted-system
  cryptsetup luksFormat --label=encrypted-system $blockdev
  cryptsetup luksAddKey $blockdev  # add backup key
  cryptsetup luksDump $blockdev  # verify luks2, crypto params, etc.
  cryptsetup --allow-discards --persistent open $blockdev system

prepare ``btrfs`` with subvolumes::

  # note: zsh array syntax
  subvolumes=(home srv swap var/log)

  mkfs.btrfs --force --label system /dev/mapper/system
  mount LABEL=system /mnt
  btrfs subvolume create /mnt/@
  btrfs subvolume set-default /mnt/@
  btrfs subvolume create /mnt/@snapshots
  for s in $subvolumes; do btrfs subvolume create "/mnt/@${s/\//-}"; done
  umount /mnt

prepare final system layout::

  o_btrfs=defaults,X-mount.mkdir,compress=zstd:1,noatime
  mount -o $o_btrfs LABEL=system /mnt
  mount -o X-mount.mkdir LABEL=EFI /mnt/boot
  mount -o $o_btrfs,subvol=@snapshots LABEL=system /mnt/.snapshots
  for s in $subvolumes; do
    mount -o "${o_btrfs},subvol=@${s/\//-}" LABEL=system "/mnt/$s";
  done
  mount | grep /mnt

swap file
=========

::

  swap_size=8G

  sw=/mnt/swap/swapfile
  touch $sw
  chmod 600 $sw
  chattr +C $sw  # disable cow
  btrfs property set $sw compression none
  fallocate --length $swap_size $sw
  mkswap $sw
  swapon $sw
  cat /proc/swaps

bootstrap
=========

install system::

  pacstrap /mnt base dracut linux linux-headers linux-lts linux-lts-headers linux-firmware btrfs-progs etckeeper intel-ucode networkmanager sudo vim wget

minimal ``fstab``::

  genfstab -L /mnt >> /mnt/etc/fstab.generated  # not used; too much unnecessary noise
  {
    echo "LABEL=system / btrfs compress=zstd:1,noatime 0 0"
    echo "LABEL=system /.snapshots btrfs noatime,subvol=@snapshots 0 0"
    for s in $subvolumes; do
      echo "LABEL=system /$s btrfs noatime,subvol=@${s/\//-} 0 0"
    done
    echo "/swap/swapfile none swap defaults 0 0"
  } >> /mnt/etc/fstab
  cat /mnt/etc/fstab

enter new system
================

`ensure password-less root logins work`__, also when doing this over a ssh connection::

  sed -i -e 's/^root:\*:/root::/' /mnt/etc/shadow

  cp -a /mnt/etc/securetty /mnt/etc/securetty.backup
  (for i in $(seq 0 9); do printf 'pts/%s\n' $i; done) >> /mnt/etc/securetty

__ https://bugs.archlinux.org/task/45903

open ``root`` shell (instead of ``arch-chroot`` which can't use some systemd stuff)::

  systemd-nspawn --boot --directory=/mnt

once inside::

  mv /etc/securetty.backup /etc/securetty

etckeeper
=========

::

  git config --global user.name root
  git config --global user.email "root@$(hostnamectl status --static)"
  etckeeper init
  etckeeper commit -m 'initial import'

time and date
=============

::

  timezone=Europe/Amsterdam

  timedatectl set-ntp 1
  timedatectl set-timezone $timezone

locales
=======

::

  cat << EOF >> /etc/locale.gen
  en_GB.UTF-8 UTF-8
  en_US.UTF-8 UTF-8
  nl_NL.UTF-8 UTF-8
  EOF

  locale-gen
  localectl set-locale LANG=$(< /etc/locale.gen grep '^[^#]' | head -n 1 | cut -d' ' -f1)
  localectl set-keymap colemak
  localectl

network
=======

::

  hostname=my-laptop

  hostnamectl set-hostname ${hostname}
  hostname=$(hostnamectl status --static)
  hostnamectl

note: ``/etc/hosts`` `stays empty`__

__ https://www.freedesktop.org/software/systemd/man/nss-myhostname.html

systemd-resolved::

  ln -s /usr/lib/systemd/resolv.conf /etc/resolv.conf

user account
============

root password::

  passwd

user account::

  user=wbolster

  useradd -m $user
  passwd $user  # user password

admin access for ``sudo`` + ``polkit``::

  usermod -aG wheel $user
  echo '%wheel ALL=(ALL) ALL' > /etc/sudoers.d/wheel

packages
========

🌈😎::

  sed -i -e 's/^#\(Color\)$/\1/' /etc/pacman.conf

`paru`__ aur helper:

__ https://github.com/Morganamilo/paru

::

  # check latest version, though this is only for one-off bootstrap use
  url='https://github.com/Morganamilo/paru/releases/download/v1.3.0/paru-v1.3.0-x86_64.tar.zst'

  cd /tmp
  wget "$url"
  tar --zstd -xf paru-*.tar.zst paru
  pacman -S base-devel
  sudo -u $user ./paru -S paru-bin

…or install manually (takes much longer)::

  pacman -S base-devel rustup

  su - $user
  rustup toolchain install stable
  git clone https://aur.archlinux.org/paru.git
  cd paru
  makepkg -si
  exit  # back to root shell

booting
=======

secure boot::

  sudo -u $user paru -S efitools gnu-efi sbkeys sbsigntools

  dir=/etc/secure-boot
  mkdir $dir
  cd $dir

  sbkeys  # enter name

  mkdir PK KEK db
  ln -s ../PK.auth PK/
  ln -s ../KEK.auth KEK/
  ln -s ../DB.auth db/

  mkdir /boot/secure-boot/
  cp -v PK.auth KEK.auth DB.auth /boot/secure-boot/

  sbkeysync --verbose --pk --keystore $dir

alternatively, enroll keys from bios menu.

initramfs with ``dracut``::

  sudo -u $user paru -S busybox dracut dracut-hook-uefi plymouth

  cat << EOF >> /etc/dracut.conf.d/config.conf
  kernel_cmdline="quiet splash loglevel=3 rd.udev.log_priority=3 vt.global_cursor_default=0"
  omit_dracutmodules+=" brltty "
  compress="zstd"
  uefi_secureboot_cert="/etc/secure-boot/DB.crt"
  uefi_secureboot_key="/etc/secure-boot/DB.key"
  # uefi_splash_image="/usr/share/systemd/bootctl/splash-arch.bmp"
  install_items+=" /bin/vim "
  EOF

  echo something | /usr/share/libalpm/scripts/dracut-install

``systemd-boot``::

  bootctl --path=/boot install

  for file in $(find /boot/EFI/systemd/ /boot/EFI/BOOT/ -iname '*.efi'); do
    sbsign --cert /etc/secure-boot/DB.crt --key /etc/secure-boot/DB.key --output "$file" "$file"
  done

yubikey
=======

yubikey for ``sudo`` + ``polkit``::

  pacman -S pam-u2f
  line='auth sufficient pam_u2f.so appid=sudo cue [cue_prompt=touch hardware key 🔐👈]'
  sed -i -s -e "1a\\${line}" /etc/pam.d/sudo /etc/pam.d/polkit-1

enroll later (when logged in as regular user)::

  mkdir -p ~/.config/Yubico
  pamu2fcfg -v -u $(id --user --name) -i sudo > ~/.config/Yubico/u2f_keys

packages
========

system::

  sudo -u $user paru -S - << EOF
  base-devel
  bash-completion
  binutils
  efibootmgr
  fwupd
  htop
  iotop
  kernel-modules-hook
  man-db
  man-pages
  moreutils
  nvme-cli
  openssh
  powertop
  python
  screen
  strace
  sysstat
  tmux
  udisks2
  usbutils
  EOF

desktop environment::

  sudo -u $user paru -S - << EOF
  bluez-utils
  checkupdates+aur
  chrome-gnome-shell
  emacs
  firefox
  gnome
  gnome-extra
  gobject-introspection
  google-chrome
  inkscape
  keepassxc
  libreoffice-fresh
  noto-fonts
  noto-fonts-emoji
  pacman-contrib
  polkit-gnome
  ripgrep
  syncthing
  xdg-utils
  xterm
  EOF

services
========

::

  systemctl daemon-reload
  systemctl enable bluetooth
  systemctl enable linux-modules-cleanup
  systemctl enable fstrim.timer
  systemctl enable NetworkManager
  systemctl enable sshd
  systemctl enable gdm

reboot
======

exit ``systemd-nspawn``::

  exit  # then press ^]^]^] at login prompt

back in the installer shell::

  systemctl reboot

- maybe enroll secure boot keys in bios
- enable secure boot
- set bios admin password
- 🤞
- use ``nmtui`` to connect to wi-fi (if needed from console)
- note: gdm password input likely uses qwerty

references
==========

- https://wiki.archlinux.org/index.php/Installation_guide
- https://fedoraproject.org/wiki/Changes/BtrfsTransparentCompression
