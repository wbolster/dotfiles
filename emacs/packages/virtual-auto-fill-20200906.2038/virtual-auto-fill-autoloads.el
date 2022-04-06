;;; virtual-auto-fill-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "virtual-auto-fill" "virtual-auto-fill.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from virtual-auto-fill.el

(autoload 'virtual-auto-fill-mode "virtual-auto-fill" "\
Visually wrap lines between wrap prefix and `fill-column'.

This is a minor mode.  If called interactively, toggle the
`Virtual-Auto-Fill mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `virtual-auto-fill-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "virtual-auto-fill" '("virtual-auto-fill-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; virtual-auto-fill-autoloads.el ends here
