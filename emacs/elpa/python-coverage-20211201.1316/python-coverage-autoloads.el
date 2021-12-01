;;; python-coverage-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "python-coverage" "python-coverage.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from python-coverage.el

(autoload 'python-coverage-select-coverage-file "python-coverage" "\
Explicitly set the COVERAGE-FILE-NAME to use for the current buffer.

This is only needed if autodetection does not work.

\(fn &optional COVERAGE-FILE-NAME)" t nil)

(autoload 'python-coverage-overlay-mode "python-coverage" "\
Minor mode to show Python coverage results as overlays.

If called interactively, enable Python-Coverage-Overlay mode if
ARG is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(autoload 'python-coverage-overlay-refresh "python-coverage" "\
Refresh the overlays." t nil)

(autoload 'python-coverage-overlay-remove-all "python-coverage" "\
Remove all overlays." t nil)

(autoload 'python-coverage-overlay-jump-next "python-coverage" "\
Jump to the next overlay." t nil)

(autoload 'python-coverage-overlay-jump-previous "python-coverage" "\
Jump to the previous overlay." t nil)

(autoload 'python-coverage-overlay-jump-first "python-coverage" "\
Jump to the first overlay." t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-coverage" '("python-coverage-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-coverage-autoloads.el ends here
