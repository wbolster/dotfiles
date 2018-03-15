;;; el-patch-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "el-patch" "el-patch.el" (23209 39254 24802
;;;;;;  759000))
;;; Generated autoloads from el-patch.el

(defvar el-patch--patches (make-hash-table :test 'equal) "\
Hash table of patches that have been defined.
The keys are symbols naming the objects that have been patched.
The values are hash tables mapping definition types (symbols
`defun', `defmacro', etc.) to patch definitions, which are lists
beginning with `defun', `defmacro', etc.

Note that the symbols are from the versions of patches that have
been resolved in favor of the modified version, when a patch
renames a symbol.")

(autoload 'el-patch-validate "el-patch" "\
Validate the patch with given NAME and TYPE.
This means el-patch will attempt to find the original definition
for the function, and verify that it is the same as the original
function assumed by the patch. A warning will be signaled if the
original definition for a patched function cannot be found, or if
there is a difference between the actual and expected original
definitions.

Interactively, use `completing-read' to select a function to
inspect the patch of.

NAME is a symbol naming the object being patched; TYPE is a
symbol `defun', `defmacro', etc.

Returns nil if the patch is not valid, and otherwise returns t.
If NOMSG is non-nil, does not signal a message when the patch is
valid.

If RUN-HOOKS is non-nil, runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'. Interactively, this happens unless
a prefix argument is provided.

See also `el-patch-validate-all'.

\(fn NAME TYPE &optional NOMSG RUN-HOOKS)" t nil)

(autoload 'el-patch-validate-all "el-patch" "\
Validate all currently defined patches.
Runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'.

See `el-patch-validate'.

\(fn)" t nil)

(autoload 'el-patch-defun "el-patch" "\
Patch a function. The ARGS are the same as for `defun'.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-defun 'doc-string-elt '3)

(function-put 'el-patch-defun 'lisp-indent-function 'defun)

(autoload 'el-patch-defmacro "el-patch" "\
Patch a macro. The ARGS are the same as for `defmacro'.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-defmacro 'doc-string-elt '3)

(function-put 'el-patch-defmacro 'lisp-indent-function 'defun)

(autoload 'el-patch-defsubst "el-patch" "\
Patch an inline function. The ARGS are the same as for `defsubst'.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-defsubst 'doc-string-elt '3)

(function-put 'el-patch-defsubst 'lisp-indent-function 'defun)

(autoload 'el-patch-defvar "el-patch" "\
Patch a variable. The ARGS are the same as for `defvar'.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-defvar 'lisp-indent-function 'defun)

(autoload 'el-patch-defconst "el-patch" "\
Patch a constant. The ARGS are the same as for `defconst'.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-defconst 'lisp-indent-function 'defun)

(autoload 'el-patch-defcustom "el-patch" "\
Patch a customizable variable. The ARGS are the same as for `defcustom'.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-defcustom 'lisp-indent-function 'defun)

(autoload 'el-patch-define-minor-mode "el-patch" "\
Patch a minor mode. The ARGS are the same as for `define-minor-mode'.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-define-minor-mode 'doc-string-elt '2)

(function-put 'el-patch-define-minor-mode 'lisp-indent-function 'defun)

(autoload 'el-patch-feature "el-patch" "\
Declare that some patches are only defined after FEATURE is loaded.
This is a convenience macro that creates a function for invoking
`require' on that feature, and then adds it to
`el-patch-pre-validate-hook' so that your patches are loaded and
`el-patch' can properly validate them.

FEATURE should be an unquoted symbol. ARGS, if given, are passed
as quoted literals along with FEATURE to
`el-patch-require-function' when `el-patch-validate-all' is
called.

\(fn FEATURE &rest ARGS)" nil t)

(autoload 'el-patch-add "el-patch" "\
Patch directive for inserting forms.
In the original definition, the ARGS and their containing form
are removed. In the new definition, the ARGS are spliced into the
containing s-expression.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-add 'lisp-indent-function '0)

(autoload 'el-patch-remove "el-patch" "\
Patch directive for removing forms.
In the original definition, the ARGS are spliced into the
containing s-expression. In the new definition, the ARGS and
their containing form are removed.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-remove 'lisp-indent-function '0)

(autoload 'el-patch-swap "el-patch" "\
Patch directive for swapping forms.
In the original definition, OLD is spliced into the containing
s-expression. In the new definition, NEW is spliced instead.

\(fn OLD NEW)" nil t)

(function-put 'el-patch-swap 'lisp-indent-function '0)

(autoload 'el-patch-wrap "el-patch" "\
Patch directive for wrapping forms.
TRIML and TRIMR are optional arguments. If only one is provided,
it is assumed to be TRIML. ARGS is required, and it must be a
list.

In the original definition, the ARGS are spliced into the
containing s-expression. If TRIML is provided, the first TRIML of
the ARGS are removed first. If TRIMR is provided, the last TRIMR
are also removed. In the new definition, the ARGS and their
containing list are spliced into the containing s-expression.

\(fn &optional TRIML TRIMR ARGS)" nil t)

(function-put 'el-patch-wrap 'lisp-indent-function 'defun)

(autoload 'el-patch-splice "el-patch" "\
Patch directive for splicing forms.
TRIML and TRIMR are optional arguments. If only one is provided,
it is assumed to be TRIML. ARGS is required, and it must be a
list.

In the original definition, the ARGS and their containing list
are spliced into the containing s-expression. In the new
definition, the ARGS are spliced into the containing
s-expression. If TRIML is provided, the first TRIML of the ARGS
are removed first. If TRIMR is provided, the last TRIMR are also
removed.

\(fn &optional TRIML TRIMR ARGS)" nil t)

(function-put 'el-patch-splice 'lisp-indent-function 'defun)

(autoload 'el-patch-let "el-patch" "\
Patch directive for creating local el-patch bindings.
Creates local bindings according to VARLIST, then resolves to ARG
in both the original and new definitions. You may bind symbols
that are also patch directives, but the bindings will not have
effect if the symbols are used at the beginning of a list (they
will act as patch directives).

\(fn VARLIST ARG)" nil t)

(function-put 'el-patch-let 'lisp-indent-function '1)

(autoload 'el-patch-literal "el-patch" "\
Patch directive for treating patch directives literally.
Resolves to ARG, which is not processed further by el-patch.

\(fn ARG)" nil t)

(function-put 'el-patch-literal 'lisp-indent-function '0)

(autoload 'el-patch-get "el-patch" "\
Return the patch for object NAME of the given TYPE.
NAME is a symbol for the name of the definition that was patched,
and TYPE is a symbol `defun', `defmacro', etc. If the patch could
not be found, return nil.

\(fn NAME TYPE)" nil nil)

(autoload 'el-patch-ediff-patch "el-patch" "\
Show the patch for an object in Ediff.
NAME and TYPE are as returned by `el-patch-get'.

\(fn NAME TYPE)" t nil)

(autoload 'el-patch-ediff-conflict "el-patch" "\
Show a patch conflict in Ediff.
This is a diff between the expected and actual values of a
patch's original definition. NAME and TYPE are as returned by
`el-patch-get'.

\(fn NAME TYPE)" t nil)

(autoload 'el-patch-unpatch "el-patch" "\
Remove the patch given by the PATCH-DEFINITION.
This restores the original functionality of the object being
patched. NAME and TYPE are as returned by `el-patch-get'.

\(fn NAME TYPE)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; el-patch-autoloads.el ends here
