;;; rich-minority-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rich-minority" "rich-minority.el" (0 0 0 0))
;;; Generated autoloads from rich-minority.el

(autoload 'rm--mode-list-as-string-list "rich-minority" "\
Return `minor-mode-list' as a simple list of strings." nil nil)

(defvar rich-minority-mode nil "\
Non-nil if Rich minority mode is enabled.
See the `rich-minority-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `rich-minority-mode'.")

(custom-autoload 'rich-minority-mode "rich-minority" nil)

(autoload 'rich-minority-mode "rich-minority" "\
Toggle Rich minority mode on or off.

This is a minor mode.  If called interactively, toggle the `Rich
minority mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='rich-minority-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\\{rich-minority-mode-map}

\(fn &optional ARG)" t nil)

(register-definition-prefixes "rich-minority" '("rm-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rich-minority-autoloads.el ends here
