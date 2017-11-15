;;; ivy-rich-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "ivy-rich" "ivy-rich.el" (0 0 0 0))
;;; Generated autoloads from ivy-rich.el

(autoload 'ivy-rich-switch-buffer-transformer "ivy-rich" "\
Transform STR to more readable format.

Currently the transformed format is

| Buffer name | Buffer indicators | Major mode | Project | Path (Based on project root) |.

\(fn STR)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ivy-rich" '("ivy-rich-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ivy-rich-autoloads.el ends here
