;;; evil-goggles-autoloads.el --- automatically extracted autoloads
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

If called interactively, enable Evil-Goggles mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "evil-goggles" '("evil-goggles-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-goggles-autoloads.el ends here
