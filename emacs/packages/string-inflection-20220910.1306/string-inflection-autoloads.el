;;; string-inflection-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from string-inflection.el

(autoload 'string-inflection-ruby-style-cycle "string-inflection" "\
foo_bar => FOO_BAR => FooBar => foo_bar" t)
(autoload 'string-inflection-elixir-style-cycle "string-inflection" "\
foo_bar => FooBar => foo_bar" t)
(autoload 'string-inflection-python-style-cycle "string-inflection" "\
foo_bar => FOO_BAR => FooBar => foo_bar" t)
(autoload 'string-inflection-java-style-cycle "string-inflection" "\
fooBar => FOO_BAR => FooBar => fooBar" t)
(autoload 'string-inflection-all-cycle "string-inflection" "\
foo_bar => FOO_BAR => FooBar => fooBar => foo-bar => Foo_Bar => foo_bar" t)
(autoload 'string-inflection-toggle "string-inflection" "\
toggle foo_bar <=> FooBar" t)
(autoload 'string-inflection-camelcase "string-inflection" "\
FooBar format" t)
(autoload 'string-inflection-lower-camelcase "string-inflection" "\
fooBar format" t)
(autoload 'string-inflection-underscore "string-inflection" "\
foo_bar format" t)
(autoload 'string-inflection-capital-underscore "string-inflection" "\
Foo_Bar format" t)
(autoload 'string-inflection-upcase "string-inflection" "\
FOO_BAR format" t)
(autoload 'string-inflection-kebab-case "string-inflection" "\
foo-bar format" t)
(register-definition-prefixes "string-inflection" '("string-inflection-"))


;;; End of scraped data

(provide 'string-inflection-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; string-inflection-autoloads.el ends here