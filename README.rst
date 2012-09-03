
Configuration
=============

This is my repository of personal configuration settings for various pieces of
software. Feel free to look around.

In case you wonder how I use this repository, this is how:

* A clone (and checkout) of this repo is placed somewhere in my home directory,
  e.g. in ~/Configuration/ or in ~/dotfiles/

* My home directory contains various symlinks to the files and directories in
  this repository, e.g. ~/.vimrc points to ~/Configuration/Vim/vimrc

* To manage these symlinks, I use a straight-forward 'update-symlinks' script
  (written in Python) to install (or update) the required symlinks, based on the
  configuration in 'symlinks.conf'.
