;;; dash-docs-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from dash-docs.el

(autoload 'dash-docs-activate-docset "dash-docs" "\
Activate DOCSET.  If called interactively prompts for the docset name.

(fn DOCSET)" t)
(autoload 'dash-docs-deactivate-docset "dash-docs" "\
Deactivate DOCSET.  If called interactively prompts for the docset name.

(fn DOCSET)" t)
(autoload 'dash-docs-install-user-docset "dash-docs" "\
Download an unofficial docset with specified DOCSET-NAME and move its stuff to docsets-path.

(fn DOCSET-NAME)" t)
(autoload 'dash-docs-install-docset-from-file "dash-docs" "\
Extract the content of DOCSET-TMP-PATH, move it to `dash-docs-docsets-path` and activate the docset.

(fn DOCSET-TMP-PATH)" t)
(autoload 'dash-docs-install-docset "dash-docs" "\
Download an official docset with specified DOCSET-NAME and move its stuff to docsets-path.

(fn DOCSET-NAME)" t)
(autoload 'dash-docs-async-install-docset "dash-docs" "\
Asynchronously download docset with specified DOCSET-NAME and move its stuff to docsets-path.

(fn DOCSET-NAME)" t)
(autoload 'dash-docs-async-install-docset-from-file "dash-docs" "\
Asynchronously extract the content of DOCSET-TMP-PATH, move it to `dash-docs-docsets-path` and activate the docset.

(fn DOCSET-TMP-PATH)" t)
(autoload 'dash-docs-ensure-docset-installed "dash-docs" "\
Install DOCSET if it is not currently installed.

(fn DOCSET)")
(autoload 'dash-docs-search "dash-docs" "\
Given a string PATTERN, query docsets and retrieve result.

(fn PATTERN)")
(register-definition-prefixes "dash-docs" '("dash-docs-"))


;;; Generated autoloads from use-package-dash-docs.el

(autoload 'use-package-normalize/:dash "use-package-dash-docs" "\
Normalize use-package customize keyword.

(fn NAME-SYMBOL KEYWORD ARG)")
(autoload 'use-package-handler/:dash "use-package-dash-docs" "\
Generate use-package customize keyword code.

(fn NAME KEYWORD ARGS REST STATE)")

;;; End of scraped data

(provide 'dash-docs-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; dash-docs-autoloads.el ends here
