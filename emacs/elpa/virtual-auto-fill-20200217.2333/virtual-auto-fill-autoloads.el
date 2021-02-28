;;; virtual-auto-fill-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "virtual-auto-fill" "virtual-auto-fill.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from virtual-auto-fill.el

(autoload 'virtual-auto-fill-mode "virtual-auto-fill" "\
Visually wrap lines between wrap prefix and `fill-column'.

If called interactively, enable Virtual-Auto-Fill mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "virtual-auto-fill" '("virtual-auto-fill-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; virtual-auto-fill-autoloads.el ends here
