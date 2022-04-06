;;; whitespace-cleanup-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "whitespace-cleanup-mode" "whitespace-cleanup-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from whitespace-cleanup-mode.el

(let ((loads (get 'whitespace-cleanup 'custom-loads))) (if (member '"whitespace-cleanup-mode" loads) nil (put 'whitespace-cleanup 'custom-loads (cons '"whitespace-cleanup-mode" loads))))

(autoload 'whitespace-cleanup-mode "whitespace-cleanup-mode" "\
Automatically call `whitespace-cleanup' on save.

This is a minor mode.  If called interactively, toggle the
`Whitespace-Cleanup mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `whitespace-cleanup-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(put 'whitespace-cleanup-mode 'safe-local-variable 'booleanp)

(put 'global-whitespace-cleanup-mode 'globalized-minor-mode t)

(defvar global-whitespace-cleanup-mode nil "\
Non-nil if Global Whitespace-Cleanup mode is enabled.
See the `global-whitespace-cleanup-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-whitespace-cleanup-mode'.")

(custom-autoload 'global-whitespace-cleanup-mode "whitespace-cleanup-mode" nil)

(autoload 'global-whitespace-cleanup-mode "whitespace-cleanup-mode" "\
Toggle Whitespace-Cleanup mode in all buffers.
With prefix ARG, enable Global Whitespace-Cleanup mode if ARG is
positive; otherwise, disable it.

If called from Lisp, toggle the mode if ARG is `toggle'.
Enable the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

Whitespace-Cleanup mode is enabled in all buffers where
`whitespace-cleanup-mode--maybe' would do it.

See `whitespace-cleanup-mode' for more information on
Whitespace-Cleanup mode.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "whitespace-cleanup-mode" '("whitespace-cleanup-mode-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; whitespace-cleanup-mode-autoloads.el ends here
