;;; terminal-here-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "terminal-here" "terminal-here.el" (0 0 0 0))
;;; Generated autoloads from terminal-here.el

(autoload 'terminal-here-launch "terminal-here" "\
Launch a terminal in the current working directory.

This is the directory of the current buffer unless you have
changed it by running `cd'.

\(fn &optional INNER-COMMAND)" t nil)

(defalias 'terminal-here 'terminal-here-launch)

(autoload 'terminal-here-project-launch "terminal-here" "\
Launch a terminal in the current project root.

Uses `terminal-here-project-root-function' to determine the
project root.

\(fn &optional INNER-COMMAND)" t nil)

(register-definition-prefixes "terminal-here" '("terminal-here-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; terminal-here-autoloads.el ends here
