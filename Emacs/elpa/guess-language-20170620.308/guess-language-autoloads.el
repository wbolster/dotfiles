;;; guess-language-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "guess-language" "guess-language.el" (23209
;;;;;;  39207 284521 653000))
;;; Generated autoloads from guess-language.el

(autoload 'guess-language-mode "guess-language" "\
Toggle guess-language mode.

Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

Guess-language is a buffer-local minor mode.  It guesses the
language of the current paragraph when flyspell detects an
incorrect word and changes ispell's dictionary and typo-mode
accordingly.  If the language settings change, flyspell is rerun
on the current paragraph.  If the paragraph is shorter than
`guess-language-min-paragraph-length', none of the above happens
because there is likely not enough text to guess the language
correctly.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("guess-language-pkg.el") (23209 39207
;;;;;;  76520 396000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; guess-language-autoloads.el ends here
