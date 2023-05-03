;;; el-patch-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "el-patch" "el-patch.el" (0 0 0 0))
;;; Generated autoloads from el-patch.el

(let ((loads (get 'el-patch 'custom-loads))) (if (member '"el-patch" loads) nil (put 'el-patch 'custom-loads (cons '"el-patch" loads))))

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
Creates local bindings according to VARLIST, then splices ARGS
into both the original and new definitions. You may bind symbols
that are also patch directives, but the bindings will not have
effect if the symbols are used at the beginning of a list (they
will act as patch directives).

\(fn VARLIST &rest ARGS)" nil t)

(function-put 'el-patch-let 'lisp-indent-function '1)

(autoload 'el-patch-literal "el-patch" "\
Patch directive for treating patch directives literally.
ARGS are spliced into the containing s-expression, but are not
processed further by el-patch.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-literal 'lisp-indent-function '0)

(autoload 'el-patch-concat "el-patch" "\
Patch directive for modifying string literals.
ARGS should resolve to strings; those strings are passed to
`concat' and spliced into the containing s-expression in both the
original and new definitions.

\(fn &rest ARGS)" nil t)

(function-put 'el-patch-concat 'lisp-indent-function '0)

(autoload 'el-patch--definition "el-patch" "\
Activate a PATCH-DEFINITION and update `el-patch--patches'.
PATCH-DEFINITION is an unquoted list starting with `defun',
`defmacro', etc., which may contain patch directives.

\(fn PATCH-DEFINITION)" nil t)

(autoload 'el-patch-unpatch "el-patch" "\
Remove the patch given by the PATCH-DEFINITION.
This restores the original functionality of the object being
patched. NAME, TYPE, and VARIANT are as returned by
`el-patch-get'.

\(fn NAME TYPE VARIANT)" t nil)

(autoload 'el-patch-deftype "el-patch" "\
Allow `el-patch' to patch definitions of the given TYPE.
TYPE is a symbol like `defun', `define-minor-mode', etc. This
updates `el-patch-deftype-alist' (which see for explanations of
CLASSIFY, LOCATE, DECLARE, MACRO-NAME, and FONT-LOCK) with the
provided KWARGS and defines a macro named like `el-patch-defun',
`el-patch-define-minor-mode', etc. (which can be overridden by
MACRO-NAME).

\(fn TYPE &rest KWARGS &key CLASSIFY LOCATE DECLARE MACRO-NAME FONT-LOCK)" nil t)

(function-put 'el-patch-deftype 'lisp-indent-function 'defun)

(autoload 'el-patch-fontify-as-defun "el-patch" "\
Fontify `el-patch' macro with given NAME as function definition.

\(fn NAME)" nil nil)

(autoload 'el-patch-fontify-as-variable "el-patch" "\
Fontify `el-patch' macro with given NAME as variable definition.

\(fn NAME)" nil nil)
(require 'el-patch-stub)
(el-patch--deftype-stub-setup)

(el-patch-deftype cl-defun :classify el-patch-classify-function :locate el-patch-locate-function :font-lock el-patch-fontify-as-defun :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defconst :classify el-patch-classify-variable :locate el-patch-locate-variable :font-lock el-patch-fontify-as-variable :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defcustom :classify el-patch-classify-variable :locate el-patch-locate-variable :font-lock el-patch-fontify-as-variable :declare ((doc-string 3) (indent defun)))

(el-patch-deftype define-minor-mode :classify el-patch-classify-define-minor-mode :locate el-patch-locate-function :font-lock el-patch-fontify-as-defun :declare ((doc-string 2) (indent defun)))

(el-patch-deftype defmacro :classify el-patch-classify-function :locate el-patch-locate-function :font-lock el-patch-fontify-as-defun :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defsubst :classify el-patch-classify-function :locate el-patch-locate-function :font-lock el-patch-fontify-as-defun :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defun :classify el-patch-classify-function :locate el-patch-locate-function :font-lock el-patch-fontify-as-defun :declare ((doc-string 3) (indent defun)))

(el-patch-deftype defvar :classify el-patch-classify-variable :locate el-patch-locate-variable :font-lock el-patch-fontify-as-variable :declare ((doc-string 3) (indent defun)))

(autoload 'el-patch-validate "el-patch" "\
Validate the patch with given NAME and TYPE.
This means el-patch will attempt to find the original definition
for the function, and verify that it is the same as the original
function assumed by the patch. A warning will be signaled if the
original definition for a patched function cannot be found, or if
there is a difference between the actual and expected original
definitions.

If multiple variants exist for the same patch, then select the
one specified by VARIANT (defaults to nil, like
`el-patch-variant'). For advanced usage only.

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

\(fn NAME TYPE &optional NOMSG RUN-HOOKS VARIANT)" t nil)

(autoload 'el-patch-validate-all "el-patch" "\
Validate all currently defined patches.
Runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'.

See `el-patch-validate'." t nil)

(autoload 'el-patch-feature "el-patch" "\
Declare that some patches are only defined after FEATURE is loaded.
This is a convenience macro that creates a function for invoking
`require' on that feature, and then adds it to
`el-patch-pre-validate-hook' so that your patches are loaded and
`el-patch' can properly validate them.

FEATURE should be an unquoted symbol. ARGS, if given, are passed
unchanged along with FEATURE to `el-patch-require-function' when
`el-patch-validate-all' is called.

\(fn FEATURE &rest ARGS)" nil t)

(autoload 'el-patch-get "el-patch" "\
Return the patch for object NAME of the given TYPE.
NAME is a symbol for the name of the definition that was patched,
and TYPE is a symbol `defun', `defmacro', etc. If the patch could
not be found, return nil.

If VARIANT is provided, select that variant of the patch. This is
useful only if patches were defined using `el-patch-variant'.

\(fn NAME TYPE &optional VARIANT)" nil nil)

(autoload 'el-patch-ediff-patch "el-patch" "\
Show the patch for an object in Ediff.
NAME, TYPE, and VARIANT are as returned by `el-patch-get'.

\(fn NAME TYPE &optional VARIANT)" t nil)

(autoload 'el-patch-ediff-conflict "el-patch" "\
Show a patch conflict in Ediff.
This is a diff between the expected and actual values of a
patch's original definition. NAME, TYPE, and VARIANT are as
returned by `el-patch-get'.

\(fn NAME TYPE &optional VARIANT)" t nil)

(register-definition-prefixes "el-patch" '("el-patch-"))

;;;***

;;;### (autoloads nil "el-patch-stub" "el-patch-stub.el" (0 0 0 0))
;;; Generated autoloads from el-patch-stub.el

(register-definition-prefixes "el-patch-stub" '("el-patch--deftype-stub-setup"))

;;;***

;;;### (autoloads nil "el-patch-template" "el-patch-template.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from el-patch-template.el

(defvar el-patch--templates (make-hash-table :test 'equal) "\
Hash table of templates that have been defined.
The keys are symbols naming the objects that have been patched.
The values are hash tables mapping definition types (symbols
`defun', `defmacro', etc.) to patch definitions, which are lists
beginning with `defun', `defmacro', etc.")

(autoload 'el-patch-insert-template "el-patch-template" "\
Resolve a template to an el-patch definition and insert it at point.

Template should have been defined using
`el-patch-define-template'. NAME is a symbol naming the object
being patched; TYPE is a symbol `defun', `defmacro', etc.

\(fn NAME TYPE)" t nil)

(autoload 'el-patch-eval-template "el-patch-template" "\
Resolve a template to an el-patch definition and evaluate it.

Template should have been defined using
`el-patch-define-template'. NAME is a symbol naming the object
being patched; TYPE is a symbol `defun', `defmacro', etc.

\(fn NAME TYPE)" t nil)

(autoload 'el-patch-define-template "el-patch-template" "\
Define an el-patch template.
TYPE-NAME is a list whose first element is a type which can be
any type from `el-patch-deftype-alist', e.g., `defun',
`defmacro', etc, and the second element is the name of the elisp
object to be patched or an `el-patch-*' form that resolves to
that name. Return the new-resolved name of the object.

A template in TEMPLATES can contain `...', which greedily matches
one or more forms, and `el-patch-*' directives which are resolved
before being matched. A template must match exactly one form in
the definition of the elisp object, and should not match a
subform in another template. The checks along with the actual
matching are done when the functions `el-patch-eval-template' or
`el-patch-insert-template' are called.

\(fn TYPE-NAME &rest TEMPLATES)" nil t)

(autoload 'el-patch-define-and-eval-template "el-patch-template" "\
Define and evaluate an el-patch template.

The meaning of TYPE-NAME and TEMPLATES are the same as
`el-patch-define-template'. If called in compile-time,
macro-expand the resolved template after defining the template.
If called in runtime, evaluate the resolved template instead and,
if `el-patch-warn-on-eval-template' is non-nil, print a warning.

\(fn TYPE-NAME &rest TEMPLATES)" nil t)

(autoload 'el-patch-validate-template "el-patch-template" "\
Validate the template with given NAME and TYPE.
This means el-patch will verify that the template is applicable
to the original function assumed by the patch. A warning will be
signaled if the original definition for a patched function cannot
be found, or if the template is not applicable.

Interactively, use `completing-read' to select a function to
inspect the template of.

NAME is a symbol naming the object being patched; TYPE is a
symbol `defun', `defmacro', etc.

Returns nil if the template is not valid, and otherwise returns t.
If NOMSG is non-nil, does not signal a message when the patch is
valid.

If RUN-HOOKS is non-nil, runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'. Interactively, this happens unless
a prefix argument is provided.

See also `el-patch-validate-all'.

\(fn NAME TYPE &optional NOMSG RUN-HOOKS)" t nil)

(autoload 'el-patch-validate-all-templates "el-patch-template" "\
Validate all currently defined patches.
Runs `el-patch-pre-validate-hook' and
`el-patch-post-validate-hook'.

See `el-patch-validate-template'." t nil)

(register-definition-prefixes "el-patch-template" '("el-patch-"))

;;;***

;;;### (autoloads nil nil ("el-patch-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; el-patch-autoloads.el ends here
