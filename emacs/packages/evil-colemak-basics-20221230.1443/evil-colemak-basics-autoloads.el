;;; evil-colemak-basics-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-colemak-basics" "evil-colemak-basics.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from evil-colemak-basics.el

(autoload 'evil-colemak-basics-mode "evil-colemak-basics" "\
Minor mode with evil-mode enhancements for the Colemak keyboard layout.

This is a minor mode.  If called interactively, toggle the
`Evil-Colemak-Basics mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-colemak-basics-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'global-evil-colemak-basics-mode 'globalized-minor-mode t)

(defvar global-evil-colemak-basics-mode nil "\
Non-nil if Global Evil-Colemak-Basics mode is enabled.
See the `global-evil-colemak-basics-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-evil-colemak-basics-mode'.")

(custom-autoload 'global-evil-colemak-basics-mode "evil-colemak-basics" nil)

(autoload 'global-evil-colemak-basics-mode "evil-colemak-basics" "\
Toggle Evil-Colemak-Basics mode in all buffers.
With prefix ARG, enable Global Evil-Colemak-Basics mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Evil-Colemak-Basics mode is enabled in all buffers where `(lambda nil
\(evil-colemak-basics-mode t))' would do it.

See `evil-colemak-basics-mode' for more information on
Evil-Colemak-Basics mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-colemak-basics" '("evil-colemak-basics-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-colemak-basics-autoloads.el ends here
