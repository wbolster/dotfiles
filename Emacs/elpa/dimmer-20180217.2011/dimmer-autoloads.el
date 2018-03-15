;;; dimmer-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dimmer" "dimmer.el" (23209 39165 468267 814000))
;;; Generated autoloads from dimmer.el

(defvar dimmer-mode nil "\
Non-nil if Dimmer mode is enabled.
See the `dimmer-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `dimmer-mode'.")

(custom-autoload 'dimmer-mode "dimmer" nil)

(autoload 'dimmer-mode "dimmer" "\
visually highlight the selected buffer

\(fn &optional ARG)" t nil)

(define-obsolete-function-alias 'dimmer-activate 'dimmer-mode)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dimmer-autoloads.el ends here
