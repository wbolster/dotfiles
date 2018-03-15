;;; multi-line-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "multi-line" "multi-line.el" (23209 39199 76472
;;;;;;  8000))
;;; Generated autoloads from multi-line.el

(autoload 'multi-line-enable-mode-hooks "multi-line" "\
Set default language specific strategies for multi-line.

\(fn)" t nil)

(autoload 'multi-line-disable-mode-hooks "multi-line" "\
Remove default language specific strategies for multi-line.

\(fn)" t nil)

(autoload 'multi-line "multi-line" "\
Multi-line the statement at point.

When ARG is provided single-line the statement at point instead.

\(fn ARG)" t nil)

(autoload 'multi-line-single-line "multi-line" "\
Single-line the statement at point.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "multi-line-highlight" "multi-line-highlight.el"
;;;;;;  (23209 39199 88472 81000))
;;; Generated autoloads from multi-line-highlight.el

(autoload 'multi-line-clear-highlights "multi-line-highlight" "\
Remove any existing multi-line highlight overlays.

\(fn)" t nil)

(autoload 'multi-line-highlight-current-candidates "multi-line-highlight" "\
Highlight the positions at which multi-line will consider adding newlines.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("multi-line-candidate.el" "multi-line-cycle.el"
;;;;;;  "multi-line-decorator.el" "multi-line-enter.el" "multi-line-find.el"
;;;;;;  "multi-line-pkg.el" "multi-line-respace.el" "multi-line-shared.el")
;;;;;;  (23209 39199 164472 541000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; multi-line-autoloads.el ends here
