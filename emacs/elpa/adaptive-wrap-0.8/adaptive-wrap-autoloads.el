;;; adaptive-wrap-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "adaptive-wrap" "adaptive-wrap.el" (0 0 0 0))
;;; Generated autoloads from adaptive-wrap.el

(autoload 'adaptive-wrap-prefix-mode "adaptive-wrap" "\
Wrap the buffer text with adaptive filling.

If called interactively, enable Adaptive-Wrap-Prefix mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "adaptive-wrap" '("adaptive-wrap-" "lookup-key")))

;;;***

;;;### (autoloads nil nil ("adaptive-wrap-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; adaptive-wrap-autoloads.el ends here
