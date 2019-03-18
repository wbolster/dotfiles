;;; magit-imerge-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "magit-imerge" "magit-imerge.el" (23236 44921
;;;;;;  331144 263000))
;;; Generated autoloads from magit-imerge.el

(autoload 'magit-imerge-merge "magit-imerge" "\
Incrementally merge BRANCH into the current branch.
$ git imerge merge [ARGS] BRANCH

\(fn BRANCH &optional ARGS)" t nil)

(autoload 'magit-imerge-rebase "magit-imerge" "\
Incrementally rebase the current branch onto BRANCH.
$ git imerge rebase [ARGS] BRANCH

\(fn BRANCH &optional ARGS)" t nil)

(autoload 'magit-imerge-revert "magit-imerge" "\
Incrementally revert COMMIT.

If a region selects multiple commits, revert all of them.

$ git imerge revert [ARGS] COMMIT
$ git imerge drop [ARGS] <range>

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-imerge-drop "magit-imerge" "\
Incrementally drop COMMIT from the current branch.

If a region selects multiple commits, drop all of them.

$ git imerge drop [ARGS] COMMIT
$ git imerge drop [ARGS] <range>

\(fn COMMIT &optional ARGS)" t nil)

(autoload 'magit-imerge-resume "magit-imerge" "\
Resume an incremental merge.
This can resume a previous git-imerge sequence that was suspended
with `magit-imerge-suspend'.  More generally, it marks a previous
incremental merge as the active one.

\(fn)" t nil)
 (autoload 'magit-imerge-popup "magit-imerge" nil t)

(eval-after-load 'magit '(progn (magit-define-popup-action 'magit-merge-popup 73 "Incremental merge" 'magit-imerge-popup) (magit-define-popup-sequence-action 'magit-merge-popup 73 "Incremental merge" 'magit-imerge-popup)))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; magit-imerge-autoloads.el ends here
