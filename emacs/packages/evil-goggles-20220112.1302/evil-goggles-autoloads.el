;;; evil-goggles-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-goggles" "evil-goggles.el" (0 0 0 0))
;;; Generated autoloads from evil-goggles.el

(defvar evil-goggles-mode nil "\
Non-nil if Evil-Goggles mode is enabled.
See the `evil-goggles-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `evil-goggles-mode'.")

(custom-autoload 'evil-goggles-mode "evil-goggles" nil)

(autoload 'evil-goggles-mode "evil-goggles" "\
evil-goggles global minor mode.

This is a minor mode.  If called interactively, toggle the
`Evil-Goggles mode' mode.  If the prefix argument is positive,
enable the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='evil-goggles-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-goggles" '("evil-goggles-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-goggles-autoloads.el ends here
