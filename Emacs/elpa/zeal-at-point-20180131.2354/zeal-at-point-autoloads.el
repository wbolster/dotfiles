;;; zeal-at-point-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "zeal-at-point" "zeal-at-point.el" (0 0 0 0))
;;; Generated autoloads from zeal-at-point.el

(autoload 'zeal-at-point "zeal-at-point" "\
Search for the word at point in Zeal.

\(fn &optional EDIT-SEARCH)" t nil)

(autoload 'zeal-at-point-set-docset "zeal-at-point" "\
Set current buffer's docset.

\(fn)" t nil)

(autoload 'zeal-at-point-search "zeal-at-point" "\
Prompt and search in zeal.

\(fn &optional EDIT-SEARCH)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "zeal-at-point" '("zeal-at-point-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; zeal-at-point-autoloads.el ends here
