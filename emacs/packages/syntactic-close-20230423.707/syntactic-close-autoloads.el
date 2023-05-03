;;; syntactic-close-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "syntactic-close" "syntactic-close.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from syntactic-close.el

(autoload 'syntactic-close "syntactic-close" "\
Insert closing delimiter.

With \\[universal-argument]: close everything at point.
For example
\"\\(^ *\\|^Some: *\\|\\( FOO\\|'s\\|Bar\\|BAZ\\|Outer\\(?: \\(?:\\(?:sim\\|zh

should end up as
\"\\(^ *\\|^Some: *\\|\\( FOO\\|'s\\|Bar\\|BAZ\\|Outer\\(?: \\(?:\\(?:sim\\|zh\\)\\)\\)\"

Optional argument ARG signals interactive use.
Optional argument BEG sets the lesser border.
Argument PPS, the result of ‘parse-partial-sexp’.

\(fn &optional ARG BEG PPS IACT)" t nil)

(register-definition-prefixes "syntactic-close" '("syntactic-close-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; syntactic-close-autoloads.el ends here
