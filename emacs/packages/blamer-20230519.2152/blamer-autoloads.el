;;; blamer-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "blamer" "blamer.el" (0 0 0 0))
;;; Generated autoloads from blamer.el

(autoload 'blamer-mode "blamer" "\
Blamer mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

This is a minor mode.  If called interactively, toggle the
`Blamer mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `blamer-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

When `blamer-mode' is enabled, the popup message with commit info
will appear after BLAMER-IDLE-TIME.  It works only inside git repo

\(fn &optional ARG)" t nil)

(put 'global-blamer-mode 'globalized-minor-mode t)

(defvar global-blamer-mode nil "\
Non-nil if Global Blamer mode is enabled.
See the `global-blamer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-blamer-mode'.")

(custom-autoload 'global-blamer-mode "blamer" nil)

(autoload 'global-blamer-mode "blamer" "\
Toggle Blamer mode in all buffers.
With prefix ARG, enable Global Blamer mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Blamer mode is enabled in all buffers where `(lambda nil (unless
blamer-mode (blamer-mode)))' would do it.

See `blamer-mode' for more information on Blamer mode.

\(fn &optional ARG)" t nil)

(autoload 'blamer-show-commit-info "blamer" "\
Show commit info from git blame.

TYPE - optional parameter, by default will use `overlay-popup'.

\(fn &optional TYPE)" t nil)

(autoload 'blamer-show-posframe-commit-info "blamer" "\
Show commit info from git blame using posframe." t nil)

(register-definition-prefixes "blamer" '("blamer-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; blamer-autoloads.el ends here
