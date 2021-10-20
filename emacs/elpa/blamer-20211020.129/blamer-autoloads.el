;;; blamer-autoloads.el --- automatically extracted autoloads
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

If called interactively, enable Blamer mode if ARG is positive,
and disable it if ARG is zero or negative.  If called from Lisp,
also enable the mode if ARG is omitted or nil, and toggle it if
ARG is `toggle'; disable the mode otherwise.

When blamer-mode is enabled, the popup message with commit info
will appear after BLAMER-IDLE-TIME. It works only inside git repo

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
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Blamer mode is enabled in all buffers where
`(lambda nil (when (not blamer-mode) (blamer-mode)))' would do it.
See `blamer-mode' for more information on Blamer mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "blamer" '("blamer-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; blamer-autoloads.el ends here
