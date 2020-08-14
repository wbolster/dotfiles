;;; balanced-windows-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "balanced-windows" "balanced-windows.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from balanced-windows.el

(defvar balanced-windows-mode nil "\
Non-nil if Balanced-Windows mode is enabled.
See the `balanced-windows-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `balanced-windows-mode'.")

(custom-autoload 'balanced-windows-mode "balanced-windows" nil)

(autoload 'balanced-windows-mode "balanced-windows" "\
Global minor mode to keep windows balanced at all times.

If called interactively, enable Balanced-Windows mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "balanced-windows" '("balanced-windows-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; balanced-windows-autoloads.el ends here
