;;; vdiff-magit-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from vdiff-magit.el

 (autoload 'vdiff-magit "vdiff-magit" nil)
(autoload 'vdiff-magit-resolve "vdiff-magit" "\
Resolve outstanding conflicts in FILE using vdiff.
FILE has to be relative to the top directory of the repository.

In the rare event that you want to manually resolve all
conflicts, including those already resolved by Git, use
`ediff-merge-revisions-with-ancestor'.

(fn FILE)" t)
(autoload 'vdiff-magit-stage "vdiff-magit" "\
Stage and unstage changes to FILE using vdiff.
FILE has to be relative to the top directory of the repository.

(fn FILE)" t)
(autoload 'vdiff-magit-dwim "vdiff-magit" "\
Compare, stage, or resolve using vdiff.

This command tries to guess what file, and what commit or range
the user wants to compare, stage, or resolve using vdiff.  It
might only be able to guess either the file, or range or commit,
in which case the user is asked about the other.  It might not
always guess right, in which case the appropriate `vdiff-magit-*'
command has to be used explicitly.  If it cannot read the user's
mind at all, then it asks the user for a command to run." t)
(autoload 'vdiff-magit-show-unstaged "vdiff-magit" "\
Show unstaged changes using vdiff.

This only allows looking at the changes; to stage, unstage,
and discard changes using vdiff, use `vdiff-magit-stage'.

FILE must be relative to the top directory of the repository.

(fn FILE)" t)
(register-definition-prefixes "vdiff-magit" '("vdiff-magit-"))

;;; End of scraped data

(provide 'vdiff-magit-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; vdiff-magit-autoloads.el ends here
