;;; virtual-auto-fill-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from virtual-auto-fill.el

(autoload 'virtual-auto-fill-mode "virtual-auto-fill" "\
Visually wrap lines between wrap prefix and `fill-column'.

This is a minor mode.  If called interactively, toggle the
`Virtual-Auto-Fill mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable the
mode if ARG is nil, omitted, or is a positive number.  Disable the mode
if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate the variable `virtual-auto-fill-mode'.

The mode's hook is called both when the mode is enabled and when it is
disabled.

(fn &optional ARG)" t)
(register-definition-prefixes "virtual-auto-fill" '("virtual-auto-fill-"))

;;; End of scraped data

(provide 'virtual-auto-fill-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; virtual-auto-fill-autoloads.el ends here
