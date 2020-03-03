;;; evil-swap-keys-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-swap-keys" "evil-swap-keys.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from evil-swap-keys.el

(autoload 'evil-swap-keys-mode "evil-swap-keys" "\
Minor mode to intelligently swap keyboard keys during text input.

\(fn &optional ARG)" t nil)

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
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Evil-Swap-Keys mode is enabled in all buffers where
`(lambda nil (evil-swap-keys-mode t))' would do it.
See `evil-swap-keys-mode' for more information on Evil-Swap-Keys mode.

\(fn &optional ARG)" t nil)

(autoload 'evil-swap-keys-add-mapping "evil-swap-keys" "\
Add a one-way mapping from key FROM to key TO.

\(fn FROM TO)" nil nil)

(autoload 'evil-swap-keys-add-pair "evil-swap-keys" "\
Add a two-way mapping to swap keys FROM and TO.

\(fn FROM TO)" nil nil)

(autoload 'evil-swap-keys-swap-number-row "evil-swap-keys" "\
Swap the keys on the number row.

\(fn)" t nil)

(autoload 'evil-swap-keys-swap-underscore-dash "evil-swap-keys" "\
Swap the underscore and the dash.

\(fn)" t nil)

(autoload 'evil-swap-keys-swap-colon-semicolon "evil-swap-keys" "\
Swap the colon and semicolon.

\(fn)" t nil)

(autoload 'evil-swap-keys-swap-tilde-backtick "evil-swap-keys" "\
Swap the backtick and tilde.

\(fn)" t nil)

(autoload 'evil-swap-keys-swap-double-single-quotes "evil-swap-keys" "\
Swap the double and single quotes.

\(fn)" t nil)

(autoload 'evil-swap-keys-swap-square-curly-brackets "evil-swap-keys" "\
Swap the square and curly brackets.

\(fn)" t nil)

(autoload 'evil-swap-keys-swap-pipe-backslash "evil-swap-keys" "\
Swap the pipe and backslash.

\(fn)" t nil)

(autoload 'evil-swap-keys-swap-question-mark-slash "evil-swap-keys" "\
Swap the question mark and slash.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-swap-keys" '("evil-swap-keys-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-swap-keys-autoloads.el ends here
