<!-- -*- mode: gfm; coding: utf-8; tab-width: 2; fill-column: 130; eval: (visual-line-mode t) -*- -->

# autorevert-tail-truncate.el

Emacs minor mode for auto-revert-tail-mode to optimize memory and CPU.

`auto-revert-tail-truncate-mode` is a veneer over `auto-revert-tail-mode` to automate truncating the tailed buffer to a
user-specified number of lines. This allows you, for example, to tail log files in an auto-reverting buffer forever without
running out of memory. By default, a newly tailed buffer is immediately truncated for the same reason. Also, by default, the
buffer's undo log is disabled along with font-lock to both preserve memory and optimize CPU consumption.

Use the command `auto-revert-tail-truncate-file` to open a file in a new buffer with `auto-revert-tail-truncate-mode`
enabled.

Add a function (example, below) to `auto-revert-tail-truncate-mode-hook` to control additional features in your tailed buffers;
e.g., `truncate-lines`, controlling `so-long-mode` threshold, disabling spell checking, or enabling other minor modes for
specific log-file formats (the visual features of which may require you to enable font-lock for those buffers).

Refer to the source code or docstrings for details on user options.

## Installation

`autorevert-tail-truncate.el` is not yet part of Emacs (it will be submitted for consideration to be included in Emacs 31) and has not yet been published on ELPA.

If you use Emacs 29.1+, use `package-vc-install`. In your `*scratch*` buffer, copy/paste or type the following, and run it via
the command `eval-last-sexp`, which see:

``` elisp
(package-vc-install "https://github.com/shipmints/autorevert-tail-truncate.el")
```

Or, if you use Emacs 30+, uncomment the :vc stanza, below.

``` elisp
(use-package autorevert-tail-truncate
  ;; :vc (
  ;;   :url "https://github.com/shipmints/autorevert-tail-truncate.el"
  ;;  :branch "master")
  :config
  (setq auto-revert-tail-truncate-max-lines 5000) ; default 5000
  (setq auto-revert-tail-truncate-file-size-hint 64) ; default 64
  (setq auto-revert-tail-truncate-immediately t) ; default t
  (setq auto-revert-tail-truncate-verbose nil) ; default nil
  (setq auto-revert-tail-truncate-immediately t) ; default t
  (setq auto-revert-tail-truncate-read-only t) ; deault t
  (setq auto-revert-tail-truncate-disable-undo t) ; default t
  (setq auto-revert-tail-truncate-disable-font-lock t) ; default t

  (defun my/auto-revert-tail-truncate-mode-hook ()
    "Custom `auto-revert-tail-truncate-mode' hook."
    (setq-local auto-revert-tail-truncate-max-lines 10000)
    (setq-local truncate-lines t)
    (when (fboundp 'so-long-mode)
      (setq-local so-long-threshold 25000))
    (when (eq my:spell-checker 'jinx)
      (jinx-mode -1))
    (when (fboundp 'show-smartparens-mode)
      (show-smartparens-mode -1)))
  (add-hook 'auto-revert-tail-truncate-mode-hook #'my/auto-revert-tail-truncate-mode-hook))
```

To invoke the mode interactively, use `M-x auto-revert-tail-truncate-mode`.

To enable `auto-revert-tail-truncate-mode` automatically when viewing files of certain modes, add something like the following to their major-mode hooks.

``` elisp
(defun my/text-mode-hook ()
  (auto-revert-tail-truncate-mode))
(add-hook 'text-mode-hook #'my/text-mode-hook)
```

If you want to enable `auto-revert-tail-truncate-mode` only for buffers with file names that have a .log suffix, but not others that share the same major mode, consider the following.

``` elisp
(defun my/text-mode-hook ()
  (when (and buffer-file-name
             (string-match-p "\\.log\\'" buffer-file-name))
    (auto-revert-tail-truncate-mode)))
(add-hook 'text-mode-hook #'my/text-mode-hook)
```

## Coda

I put this into the package because the earliest Emacs I can easily support is 29.x. This will likely be altered if the package is
published in ELPA and most certainly will be removed as a part of Emacs 31, if accepted.

    ;; Package-Requires: ((emacs "29.1"))

This is a new package, and though the author successfully uses it, your mileage may vary. The author does not used specialized
log-file modes such as https://github.com/doublep/logview or https://github.com/emacsmirror/log4j-mode -- if you do, and find
issues that you cannot resolve via your configuration, file an issue here.
