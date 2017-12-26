;;; evil-string-inflection-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "evil-string-inflection" "evil-string-inflection.el"
;;;;;;  (23106 39697 650395 625000))
;;; Generated autoloads from evil-string-inflection.el

(evil-define-operator evil-operator-string-inflection (beg end _type) "Define a new evil operator that cicles underscore -> UPCASE -> CamelCase." :move-point nil (interactive "<R>") (let ((str (buffer-substring-no-properties beg end))) (save-excursion (delete-region beg end) (insert (string-inflection-all-cycle-function str)))))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; evil-string-inflection-autoloads.el ends here
