;;; python-docstring-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "python-docstring" "python-docstring.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from python-docstring.el

(autoload 'python-docstring-fill "python-docstring" "\
Wrap Python docstrings as epytext or ReStructured Text." t nil)

(autoload 'python-docstring-mode "python-docstring" "\
Toggle python-docstring-mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

This is a minor mode.  If called interactively, toggle the
`Python-Docstring mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `python-docstring-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(autoload 'python-docstring-install "python-docstring" "\
Add python-docstring-mode as a hook to python.mode." nil nil)

(register-definition-prefixes "python-docstring" '("python-docstring-"))

;;;***

;;;### (autoloads nil nil ("python-docstring-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-docstring-autoloads.el ends here
