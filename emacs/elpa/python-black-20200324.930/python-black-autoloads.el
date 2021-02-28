;;; python-black-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "python-black" "python-black.el" (0 0 0 0))
;;; Generated autoloads from python-black.el
 (autoload 'python-black-buffer "python-black" nil t)
 (autoload 'python-black-region "python-black" nil t)
 (autoload 'python-black-on-save-mode "python-black" nil t)

(autoload 'python-black-statement "python-black" "\
Reformats the current statement.

When called interactively with a prefix argument, or when
DISPLAY-ERRORS is non-nil, shows a buffer if the formatting fails.

\(fn &optional DISPLAY-ERRORS)" t nil)

(autoload 'python-black-partial-dwim "python-black" "\
Reformats the active region or the current statement.

This runs ‘python-black-region’ or ‘python-black-statement’ depending
on whether the region is currently active.

When called interactively with a prefix argument, or when
DISPLAY-ERRORS is non-nil, shows a buffer if the formatting fails.

\(fn &optional DISPLAY-ERRORS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "python-black" '("python-black-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; python-black-autoloads.el ends here
