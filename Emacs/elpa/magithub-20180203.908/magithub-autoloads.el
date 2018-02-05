;;; magithub-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "magithub" "magithub.el" (23158 5974 935098
;;;;;;  793000))
;;; Generated autoloads from magithub.el
 (autoload 'magithub-dispatch-popup "magithub" nil t)

(eval-after-load "magit" '(progn (magit-define-popup-action 'magit-dispatch-popup 72 "Magithub" #'magithub-dispatch-popup 33) (define-key magit-status-mode-map "H" #'magithub-dispatch-popup)))

;;;***

;;;### (autoloads nil "magithub-ci" "magithub-ci.el" (23158 5974
;;;;;;  971098 933000))
;;; Generated autoloads from magithub-ci.el

(autoload 'magithub-maybe-insert-ci-status-header "magithub-ci" "\
If this is a GitHub repository, insert the CI status header.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "magithub-comment" "magithub-comment.el" (23158
;;;;;;  5974 947098 839000))
;;; Generated autoloads from magithub-comment.el

(autoload 'magithub-comment-new "magithub-comment" "\
Comment on ISSUE in a new buffer.
If prefix argument DISCARD-DRAFT is specified, the draft will not
be considered.

If INITIAL-CONTENT is specified, it will be inserted as the
initial contents of the reply if there is no draft.

\(fn ISSUE &optional DISCARD-DRAFT INITIAL-CONTENT)" t nil)

;;;***

;;;### (autoloads nil "magithub-core" "magithub-core.el" (23158 5974
;;;;;;  991099 11000))
;;; Generated autoloads from magithub-core.el

(autoload 'magithub-feature-autoinject "magithub-core" "\
Configure FEATURE to recommended settings.
If FEATURE is `all' or t, all known features will be loaded.  If
FEATURE is a list, then it is a list of FEATURE symbols to load.

See `magithub-feature-list' for a list of available features and
`magithub-features' for a list of currently-installed features.

\(fn FEATURE)" nil nil)

(autoload 'magithub--section-value-at-point "magithub-core" "\
Determine the thing of TYPE at point.
This is intended for use as a resolving function for
`thing-at-point'.

The following symbols are defined, but other values may work with
this function: `github-user', `github-issue', `github-label',
`github-comment', `github-repository', `github-pull-request',
`github-notification',

\(fn TYPE)" nil nil)

(put 'github-user 'thing-at-point (lambda nil (magithub--section-value-at-point 'user)))

(put 'github-issue 'thing-at-point (lambda nil (or magithub-issue (magithub--section-value-at-point 'issue))))

(put 'github-label 'thing-at-point (lambda nil (magithub--section-value-at-point 'label)))

(put 'github-comment 'thing-at-point (lambda nil (or magithub-comment (magithub--section-value-at-point 'comment))))

(put 'github-notification 'thing-at-point (lambda nil (magithub--section-value-at-point 'notification)))

(put 'github-repository 'thing-at-point (lambda nil (or (magithub--section-value-at-point 'repository) magithub-repo (magithub-repo))))

(put 'github-pull-request 'thing-at-point (lambda nil (or (magithub--section-value-at-point 'pull-request) (when-let* ((issue (thing-at-point 'github-issue))) (and (magithub-issue--issue-is-pull-p issue) (magithub-cache :issues `(magithub-request (ghubp-get-repos-owner-repo-pulls-number ',(magithub-issue-repo issue) ',issue))))))))

;;;***

;;;### (autoloads nil "magithub-dash" "magithub-dash.el" (23158 5974
;;;;;;  999099 42000))
;;; Generated autoloads from magithub-dash.el

(autoload 'magithub-dashboard "magithub-dash" "\
View your GitHub dashboard.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magithub-edit-mode" "magithub-edit-mode.el"
;;;;;;  (23158 5974 911098 699000))
;;; Generated autoloads from magithub-edit-mode.el

(autoload 'magithub-edit-mode "magithub-edit-mode" "\
Major mode for editing GitHub issues and pull requests.

\(fn)" t nil)

;;;***

;;;### (autoloads nil "magithub-issue" "magithub-issue.el" (23158
;;;;;;  5975 11099 89000))
;;; Generated autoloads from magithub-issue.el

(autoload 'magithub-issue--insert-issue-section "magithub-issue" "\
Insert GitHub issues if appropriate.

\(fn)" nil nil)

(autoload 'magithub-issue--insert-pr-section "magithub-issue" "\
Insert GitHub pull requests if appropriate.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "magithub-issue-tricks" "magithub-issue-tricks.el"
;;;;;;  (23158 5975 7099 73000))
;;; Generated autoloads from magithub-issue-tricks.el

(autoload 'magithub-pull-request-merge "magithub-issue-tricks" "\
Merge PULL-REQUEST with ARGS.
See `magithub-pull-request--completing-read'.  If point is on a
pull-request object, that object is selected by default.

\(fn PULL-REQUEST &optional ARGS)" t nil)

;;;***

;;;### (autoloads nil "magithub-issue-view" "magithub-issue-view.el"
;;;;;;  (23158 5974 959098 886000))
;;; Generated autoloads from magithub-issue-view.el

(autoload 'magithub-issue-view "magithub-issue-view" "\
View ISSUE in a new buffer.
Return the new buffer.

\(fn ISSUE)" t nil)

;;;***

;;;### (autoloads nil "magithub-settings" "magithub-settings.el"
;;;;;;  (23158 5974 979098 964000))
;;; Generated autoloads from magithub-settings.el
(autoload 'magithub-settings-popup "magithub-settings" nil t)

;;;***

;;;### (autoloads nil nil ("magithub-faces.el" "magithub-issue-post.el"
;;;;;;  "magithub-label.el" "magithub-notification.el" "magithub-orgs.el"
;;;;;;  "magithub-pkg.el" "magithub-repo.el" "magithub-user.el")
;;;;;;  (23158 5975 19099 120000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magithub-autoloads.el ends here
