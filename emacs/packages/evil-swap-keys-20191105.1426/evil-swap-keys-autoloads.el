;;; evil-swap-keys-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-swap-keys" "evil-swap-keys.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from evil-swap-keys.el

(autoload 'evil-swap-keys-mode "evil-swap-keys" "\
Minor mode to intelligently swap keyboard keys during text input.

This is a minor mode.  If called interactively, toggle the
`Evil-Swap-Keys mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-swap-keys-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-evil-swap-keys-mode 'globalized-minor-mode t)

(defvar global-evil-swap-keys-mode nil "\
Non-nil if Global Evil-Swap-Keys mode is enabled.
See the `global-evil-swap-keys-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-swap-keys-mode'.")

(custom-autoload 'global-evil-swap-keys-mode "evil-swap-keys" nil)

(autoload 'global-evil-swap-keys-mode "evil-swap-keys" "\
Toggle Evil-Swap-Keys mode in all buffers.
With prefix ARG, enable Global Evil-Swap-Keys mode if ARG is positive;
otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Evil-Swap-Keys mode is enabled in all buffers where `(lambda nil
\(evil-swap-keys-mode t))' would do it.

See `evil-swap-keys-mode' for more information on Evil-Swap-Keys
mode.

\(fn &optional ARG)" t nil)

(autoload 'evil-swap-keys-add-mapping "evil-swap-keys" "\
Add a one-way mapping from key FROM to key TO.

\(fn FROM TO)" nil nil)

(autoload 'evil-swap-keys-add-pair "evil-swap-keys" "\
Add a two-way mapping to swap keys FROM and TO.

\(fn FROM TO)" nil nil)

(autoload 'evil-swap-keys-swap-number-row "evil-swap-keys" "\
Swap the keys on the number row." t nil)

(autoload 'evil-swap-keys-swap-underscore-dash "evil-swap-keys" "\
Swap the underscore and the dash." t nil)

(autoload 'evil-swap-keys-swap-colon-semicolon "evil-swap-keys" "\
Swap the colon and semicolon." t nil)

(autoload 'evil-swap-keys-swap-tilde-backtick "evil-swap-keys" "\
Swap the backtick and tilde." t nil)

(autoload 'evil-swap-keys-swap-double-single-quotes "evil-swap-keys" "\
Swap the double and single quotes." t nil)

(autoload 'evil-swap-keys-swap-square-curly-brackets "evil-swap-keys" "\
Swap the square and curly brackets." t nil)

(autoload 'evil-swap-keys-swap-pipe-backslash "evil-swap-keys" "\
Swap the pipe and backslash." t nil)

(autoload 'evil-swap-keys-swap-question-mark-slash "evil-swap-keys" "\
Swap the question mark and slash." t nil)

(register-definition-prefixes "evil-swap-keys" '("evil-swap-keys-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-swap-keys-autoloads.el ends here
