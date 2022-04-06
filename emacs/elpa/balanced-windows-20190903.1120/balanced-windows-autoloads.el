;;; balanced-windows-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "balanced-windows" "balanced-windows.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from balanced-windows.el

(defvar balanced-windows-mode nil "\
Non-nil if Balanced-Windows mode is enabled.
See the `balanced-windows-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `balanced-windows-mode'.")

(custom-autoload 'balanced-windows-mode "balanced-windows" nil)

(autoload 'balanced-windows-mode "balanced-windows" "\
Global minor mode to keep windows balanced at all times.

This is a minor mode.  If called interactively, toggle the
`Balanced-Windows mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='balanced-windows-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "balanced-windows" '("balanced-windows-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; balanced-windows-autoloads.el ends here
