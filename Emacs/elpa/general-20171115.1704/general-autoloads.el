;;; general-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "general" "general.el" (0 0 0 0))
;;; Generated autoloads from general.el

(autoload 'general-define-key "general" "\
The primary key definition function provided by general.

PREFIX corresponds to a prefix key and defaults to none. STATES corresponds to
the evil state(s) to bind the keys in. Non-evil users should not set STATES.
When STATES is non-nil, `evil-define-key*' will be used; otherwise `define-key'
will be used (unless DEFINER is specified). Evil users may also want to leave
STATES nil and set KEYMAPS to a keymap such as `evil-normal-state-map' for
global mappings. KEYMAPS defaults to 'global (`general-default-keymaps' and
`general-default-states' can be changed by the user). There is also 'local,
which create buffer-local keybindings for both evil and non-evil keybindings.
There are other special, user-alterable \"shorthand\" symbols for keymaps and
states (see `general-keymap-aliases' and `general-state-aliases').

Unlike with normal key definitions functions, the keymaps in KEYMAPS should be
quoted (this allows using the keymap name for other purposes, e.g. deferment,
inferring major mode names by removing \"-map\" for which-key, easily storing
the keymap name for use with `general-describe-keybindings', etc.). Note that
STATES and KEYMAPS can either be a list or a single symbol. If any keymap does
not exist, those keybindings will be deferred until the keymap does exist, so
using `eval-after-load' is not necessary with this function.

MAPS consists of paired keys and definitions. Each pair (if not ignored) will be
recorded for later use with `general-describe-keybindings'.

If NON-NORMAL-PREFIX is specified, this prefix will be used for emacs and insert
state keybindings instead of PREFIX. This argument will only have an effect if
'insert and/or 'emacs is one of the STATES or if 'evil-insert-state-map and/or
'evil-emacs-state-map is one of the KEYMAPS. Alternatively, GLOBAL-PREFIX can be
used with PREFIX and/or NON-NORMAL-PREFIX to bind keys in all states under a
specified prefix. Like with NON-NORMAL-PREFIX, GLOBAL-PREFIX will prevent PREFIX
from applying to insert and emacs states. Note that these keywords are only
useful for evil users.

INFIX can be used to append a string to all of the specified prefixes. This is
potentially useful when you are using GLOBAL-PREFIX and/or NON-NORMAL-PREFIX so
that you can sandwich keys in between all the prefixes and the specified keys in
MAPS. This may be particularly useful if you are using default prefixes in a
wrapper so that you can add to them without needing to re-specify all of them.
If none of the other prefix arguments are specified, INFIX will have no effect.

If PREFIX-COMMAND or PREFIX-MAP is specified, a prefix command and/or keymap.
 PREFIX-NAME can be additionally specified to set the keymap mene name/prompt.
 If PREFIX-COMMAND is specified `define-prefix-command' will be used. Otherwise,
 only a prefix keymap will be created. Previously created prefix
 commands/keymaps will never be redefined/cleared. All prefixes (including the
 INFIX key, if specified) will then be bound to PREFIX-COMMAND or PREFIX-MAP.

If DEFINER is specified, a custom key definer will be used. See the README for
more information.

MAJOR-MODES, WK-MATCH-KEYS, WK-MATCH-BINDINGS, and WK-FULL-KEYS are the
corresponding global versions of which-key extended definition keywords. They
will only have an effect for extended definitions that specify :which-key
or :wk. See the section on extended definitions in the README for more
information.

LISPY-PLIST and WORF-PLIST are the corresponding global versions of extended
definition keywords that are used for the corresponding custom DEFINER

\(fn &rest MAPS &key DEFINER (STATES general-default-states) (KEYMAPS general-default-keymaps) (PREFIX general-default-prefix) (NON-NORMAL-PREFIX general-default-non-normal-prefix) (GLOBAL-PREFIX general-default-global-prefix) INFIX PREFIX-COMMAND PREFIX-MAP PREFIX-NAME PREDICATE PACKAGE MAJOR-MODES (WK-MATCH-KEYS t) (WK-MATCH-BINDING t) (WK-FULL-KEYS t) LISPY-PLIST WORF-PLIST &allow-other-keys)" nil nil)

(autoload 'general-create-definer "general" "\
A helper macro to create key definitions functions.
This allows the creation of key definition functions that
will use a certain keymap, evil state, and/or prefix key by default.
NAME will be the function name and ARGS are the keyword arguments that
are intended to be the defaults.

\(fn NAME &rest ARGS)" nil t)

(autoload 'general-emacs-define-key "general" "\
A wrapper for `general-define-key' that is similar to `define-key'.
It has a positional argument for KEYMAPS (that will not be overridden by a later
:keymaps argument). Besides this, it acts the same as `general-define-key', and
ARGS can contain keyword arguments in addition to keybindings. This can
basically act as a drop-in replacement for `define-key', and unlike with
`general-define-key', KEYMAPS does not need to be quoted.

\(fn KEYMAPS &rest ARGS)" nil t)

(function-put 'general-emacs-define-key 'lisp-indent-function '1)

(autoload 'general-evil-define-key "general" "\
A wrapper for `general-define-key' that is similar to `evil-define-key'.
It has positional arguments for STATES and KEYMAPS (that will not be overridden
by a later :keymaps or :states argument). Besides this, it acts the same as
`general-define-key', and ARGS can contain keyword arguments in addition to
keybindings. This can basically act as a drop-in replacement for
`evil-define-key', and unlike with `general-define-key', KEYMAPS does not need
to be quoted.

\(fn STATES KEYMAPS &rest ARGS)" nil t)

(function-put 'general-evil-define-key 'lisp-indent-function '2)

(autoload 'general-def "general" "\
General definer that takes a variable number of positional arguments in ARGS.
This macro will act as `general-define-key', `general-emacs-define-key', or
`general-evil-define-key' based on how many of the initial arguments do not
correspond to keybindings.

\(fn &rest ARGS)" nil t)

(function-put 'general-def 'lisp-indent-function 'defun)

(autoload 'general-describe-keybindings "general" "\
Show all keys that have been bound with general in an org buffer.
Any local keybindings will be shown first followed by global keybindings.

\(fn)" t nil)

(autoload 'general-simulate-keys "general" "\
Create a function to simulate KEYS in STATE and KEYMAP.
STATE should only be specified by evil users and can be a quoted evil state or
t (in which case emacs state will be used). When neither STATE or KEYMAP are
specified, the keys will be simulated in the current context. Normally the
generated function will look up KEYS in the correct context to try to match a
command or keymap. To prevent this lookup, NO-LOOKUP can be specified as
non-nil. See the docstring for `general--simulate-keys' for some insight as to
why you might want to use this.

KEYS should be a string given in `kbd' notation. It can also be a list of a
single command followed by a string of the keys to simulate after calling that
command. If DOCSTRING is given, it will replace the automatically generated
docstring. If NAME is given, it will replace the automatically generated
function name. NAME should not be quoted.

The advantages of this over a keyboard macro are as follows:
- The user can control the context in which the keys are simulated
- The user can simulate both a command and keys
- The user can simulate a partial key sequence (e.g. for a keymap)

\(fn KEYS &optional STATE KEYMAP NO-LOOKUP DOCSTRING NAME)" nil t)

(autoload 'general-key-dispatch "general" "\
Create a function that will run FALLBACK-COMMAND or a command from MAPS.
MAPS consists of <key> <command> pairs. If a key in MAPS is matched, the
corresponding command will be run. Otherwise FALLBACK-COMMAND will be run with
the unmatched keys. So, for example, if \"ab\" was pressed, and \"ab\" is not
one of the key sequences from MAPS, the FALLBACK-COMMAND will be run followed by
the simulated keypresses of \"ab\". Prefix arguments will still work regardless
of which command is run. This is useful for binding under non-prefix keys. For
example, this can be used to redefine a sequence like \"cw\" or \"cow\" in evil
but still have \"c\" work as `evil-change'. If TIMEOUT is specified,
FALLBACK-COMMAND will also be run in the case that the user does not press the
next key within the TIMEOUT (e.g. 0.5).

NAME and DOCSTRING are optional keyword arguments. They can be used to replace
the automatically generated name and docstring for the created function and are
potentially useful if you want to create multiple, different commands using the
same FALLBACK-COMMAND (e.g. `self-insert-command').

When INHERIT-KEYMAP is specified, all the keybindings from that keymap will be
inherited in MAPS.

WHICH-KEY can also be specified, in which case the description WHICH-KEY will
replace the command name in the which-key popup. Note that this requires a
version of which-key from after 2016-11-21.

\(fn FALLBACK-COMMAND &rest MAPS &key TIMEOUT INHERIT-KEYMAP NAME DOCSTRING WHICH-KEY &allow-other-keys)" nil t)

(function-put 'general-key-dispatch 'lisp-indent-function '1)

(autoload 'general-translate-keys "general" "\
Translate keys in the keymap corresponding to STATE and KEYMAP-NAME.
STATE should be the name of an evil state or nil. KEYMAP-NAME should be a symbol
corresponding to the keymap to make the translations in. MAPS corresponds to a
list of translations (key replacement pairs). For example, specifying \"a\"
\"b\" will bind \"a\" to \"b\"'s definition in the keymap. If DESTRUCTIVE is
non-nil, the keymap will be destructively altered without a backup being
created. If DESTRUCTIVE is nil, a backup of the keymap will be stored on the
initial invocation, and future invocations will always reference the backup
keymap, meaning that invocations are idempotent. On the other hand, if
DESTRUCTIVE is non-nil, calling this function multiple times with \"a\" \"b\"
\"b\" \"a\", for example, would continue to swap and unswap the definitions of
these keys. This means that when DESTRUCTIVE is non-nil, all related
swaps/cycles should be done in the same invocation.

\(fn STATE KEYMAP-NAME &rest MAPS &key DESTRUCTIVE &allow-other-keys)" nil nil)

(function-put 'general-translate-keys 'lisp-indent-function 'defun)

(autoload 'general-add-hook "general" "\
A drop-in replacement for `add-hook'.
HOOKS and FUNCTIONS can be single items or lists.

\(fn HOOKS FUNCTIONS &optional APPEND LOCAL)" nil nil)

(autoload 'general-remove-hook "general" "\
A drop-in replacement for `remove-hook'.
HOOKS and FUNCTIONS can be single items or lists.

\(fn HOOKS FUNCTIONS &optional LOCAL)" nil nil)

(autoload 'general-add-advice "general" "\
A drop-in replacement for `advice-add'.
SYMBOLS and FUNCTIONS can be single items or lists.

\(fn SYMBOLS WHERE FUNCTIONS &optional PROPS)" nil nil)

(defalias 'general-advice-add #'general-add-advice)

(autoload 'general-remove-advice "general" "\
A drop-in replacement for `advice-remove'.
SYMBOLS and FUNCTIONS can be single items or lists.

\(fn SYMBOLS FUNCTIONS)" nil nil)

(autoload 'general-create-vim-definer "general" "\
A helper function to create vim-like wrappers over `general-define-key'.
The function created will be called NAME and will have the keymaps default to
KEYMAPS or the states default to STATES (both should be quoted). If
DEFAULT-TO-STATES is non-nil, :states STATES will be used. Otherwise :keymaps
KEYMAPS will be used. This can be overriden later by setting the global
`general-vim-definer-default' option.

\(fn NAME KEYMAPS &optional STATES DEFAULT-TO-STATES)" nil t)

(autoload 'general-create-dual-vim-definer "general" "\
Like `general-create-vim-definer', create a \"vim definer\" called NAME.
Only the short names in the STATES list need to be specified, but this will only
work for valid evil states.

\(fn NAME STATES &optional DEFAULT-TO-STATES)" nil t)

(autoload 'general-evil-setup "general" "\
Set up some basic equivalents for vim mapping functions.
This creates global key definition functions for the evil states.
Specifying SHORT-NAMES as non-nil will create non-prefixed function
aliases such as `nmap' for `general-nmap'.

\(fn &optional SHORT-NAMES DEFAULT-TO-STATES)" nil t)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "general" '("general-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; general-autoloads.el ends here
