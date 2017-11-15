;;; magithub-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "magithub" "magithub.el" (0 0 0 0))
;;; Generated autoloads from magithub.el
 (autoload 'magithub-dispatch-popup "magithub" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub" '("magithub-")))

;;;***

;;;### (autoloads nil "magithub-ci" "magithub-ci.el" (0 0 0 0))
;;; Generated autoloads from magithub-ci.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-ci" '("magit")))

;;;***

;;;### (autoloads nil "magithub-comment" "magithub-comment.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from magithub-comment.el

(autoload 'magithub-comment-new "magithub-comment" "\
Comment on ISSUE in a new buffer.

\(fn ISSUE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-comment" '("magit")))

;;;***

;;;### (autoloads nil "magithub-core" "magithub-core.el" (0 0 0 0))
;;; Generated autoloads from magithub-core.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-core" '("magit")))

;;;***

;;;### (autoloads nil "magithub-dash" "magithub-dash.el" (0 0 0 0))
;;; Generated autoloads from magithub-dash.el

(autoload 'magithub-dashboard "magithub-dash" "\
View your GitHub dashboard.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-dash" '("magithub-dash")))

;;;***

;;;### (autoloads nil "magithub-edit-mode" "magithub-edit-mode.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from magithub-edit-mode.el

(autoload 'magithub-edit-mode "magithub-edit-mode" "\
Major mode for editing GitHub issues and pull requests.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-edit-mode" '("magithub-edit-")))

;;;***

;;;### (autoloads nil "magithub-issue" "magithub-issue.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magithub-issue.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-issue" '("magit")))

;;;***

;;;### (autoloads nil "magithub-issue-post" "magithub-issue-post.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from magithub-issue-post.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-issue-post" '("magithub-")))

;;;***

;;;### (autoloads nil "magithub-issue-tricks" "magithub-issue-tricks.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from magithub-issue-tricks.el

(autoload 'magithub-pull-request-merge "magithub-issue-tricks" "\
Merge PULL-REQUEST with ARGS.
See `magithub-pull-request--completing-read'.  If point is on a
pull-request object, that object is selected by default.

\(fn PULL-REQUEST &optional ARGS)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-issue-tricks" '("magithub-")))

;;;***

;;;### (autoloads nil "magithub-issue-view" "magithub-issue-view.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from magithub-issue-view.el

(autoload 'magithub-issue-view "magithub-issue-view" "\
View ISSUE in a new buffer.

\(fn ISSUE)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-issue-view" '("magithub-issue-view-")))

;;;***

;;;### (autoloads nil "magithub-label" "magithub-label.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magithub-label.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-label" '("magit")))

;;;***

;;;### (autoloads nil "magithub-notification" "magithub-notification.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from magithub-notification.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-notification" '("magit")))

;;;***

;;;### (autoloads nil "magithub-orgs" "magithub-orgs.el" (0 0 0 0))
;;; Generated autoloads from magithub-orgs.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-orgs" '("magithub-orgs-list")))

;;;***

;;;### (autoloads nil "magithub-proxy" "magithub-proxy.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from magithub-proxy.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-proxy" '("magithub-proxy-set")))

;;;***

;;;### (autoloads nil "magithub-repo" "magithub-repo.el" (0 0 0 0))
;;; Generated autoloads from magithub-repo.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-repo" '("magit")))

;;;***

;;;### (autoloads nil "magithub-user" "magithub-user.el" (0 0 0 0))
;;; Generated autoloads from magithub-user.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "magithub-user" '("magit")))

;;;***

;;;### (autoloads nil nil ("magithub-faces.el" "magithub-pkg.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; magithub-autoloads.el ends here
