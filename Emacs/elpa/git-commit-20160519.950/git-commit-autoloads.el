;;; git-commit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "git-commit" "../../../../.emacs.d/elpa/git-commit-20160519.950/git-commit.el"
;;;;;;  "082aed7abaa37980e300fac71b8979a2")
;;; Generated autoloads from ../../../../.emacs.d/elpa/git-commit-20160519.950/git-commit.el

(defvar global-git-commit-mode t "\
Non-nil if Global Git-Commit mode is enabled.
See the `global-git-commit-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-git-commit-mode'.")

(custom-autoload 'global-git-commit-mode "git-commit" nil)

(autoload 'global-git-commit-mode "git-commit" "\
Edit Git commit messages.
This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message.

\(fn &optional ARG)" t nil)

;;;### (autoloads "actual autoloads are elsewhere" "git-commit" "../../../../.emacs.d/elpa/git-commit-20160519.950/git-commit.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from ../../../../.emacs.d/elpa/git-commit-20160519.950/git-commit.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "git-commit" '("git-commit-")))

;;;***

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/git-commit-20160519.950/git-commit-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/git-commit-20160519.950/git-commit.el")
;;;;;;  (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; git-commit-autoloads.el ends here
