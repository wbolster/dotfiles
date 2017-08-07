;;; syntactic-close-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "syntactic-close" "syntactic-close.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from syntactic-close.el

(autoload 'syntactic-close "syntactic-close" "\
Command will insert closing delimiter whichever needed.

With \\[universal-argument]: close everything at point. 

\(fn &optional ARG BEG FORCE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "syntactic-close" '("syntactic-close" "nth-1-pps-complement-char-maybe" "haskell-interactive-mode-prompt-start")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; syntactic-close-autoloads.el ends here
