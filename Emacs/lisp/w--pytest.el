;;; w--pytest --- run pytest and pdb as a comint mode -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'compile)
(require 'projectile)
(require 'python)

(defgroup w--pytest nil
  "Run pytest and pdb."
  :prefix "w--pytest-"
  :group 'python)

(defcustom w--pytest-default-args "--pdb"
  "Default args for pytest."
  :group 'python
  :type 'string)

(defvar w--pytest-finished-hooks nil
  "Hooks to run after pytest finishes.")

(defvar w--pytest-arguments-history nil
  "Argument history for pytest invocations.")

(define-derived-mode w--pytest-mode
  comint-mode "pytest"
  "Major mode for pytest sessions (derived from comint-mode).")

(defun w--pytest-detect-test-file-name ()
  "Detect associated test file name for the current buffer."
  (or (if (projectile-test-file-p (buffer-file-name))
          (file-relative-name (buffer-file-name) (projectile-project-root)))
      (projectile-find-matching-test (buffer-file-name))))

(defun w--pytest-sensible-args ()
  "Figure out sensible pytest args for the current buffer and point."
  (let* ((test-file-name (w--pytest-detect-test-file-name))
         (buffer-is-test-file
          (file-equal-p (concat (projectile-project-root) test-file-name)
                        (buffer-file-name)))
         (python-function-name
          (save-excursion
            ;; jumping makes it work on empty lines
            (python-nav-backward-defun)
            (python-info-current-defun)))
         (args (or test-file-name "")))
    (when (and buffer-is-test-file python-function-name)
      (setq args (format "%s -k %s" args python-function-name)))
    (when w--pytest-default-args
      (setq args (format "%s %s" w--pytest-default-args args)))
    args))

;; todo: add factor out (defun w--pytest-run (args))

(defun w--pytest (&optional arg)
  "Run pytest after prompting for arguments.
With a prefix ARG (and on first invocation), suggests sensible
arguments based on the current file and point. Subsequent invocations
default to the previously used arguments."
  (interactive "P")
  (let ((arguments (car w--pytest-arguments-history)))
    (when (or arg (null arguments))
      (setq arguments (w--pytest-sensible-args)))
    (require 'comint)
    (let ((comint-buffer (get-buffer-create "*pytest*")))
      (with-current-buffer comint-buffer
        (when (comint-check-proc comint-buffer)
          (if (or compilation-always-kill (yes-or-no-p "Kill running pytest process?"))
              (kill-process (get-buffer-process comint-buffer))
            (user-error "Aborting; pytest still running")))
        ;; (setq arguments (read-from-minibuffer
        ;;                  "pytest args: " arguments nil nil
        ;;                  'w--pytest-arguments-history))
        (setq arguments (completing-read
                         "pytest args: "
                         w--pytest-arguments-history
                         nil nil
                         nil 'w--pytest-arguments-history arguments))
        (delete-region (point-min) (point-max))
        (add-hook
         'comint-output-filter-functions
         'python-pdbtrack-comint-output-filter-function
         nil t)
        (setq arguments (format "pytest %s" arguments))
        (insert (format "command: %s\nworking directory: %s\n\n"
                        arguments default-directory))
        (let ((default-directory (projectile-project-root)))
          (make-comint-in-buffer "pytest" comint-buffer "sh" nil "-c" arguments))
        (w--pytest-mode)
        (set-process-sentinel
         (get-buffer-process comint-buffer)
         (lambda (proc _state)
           (with-current-buffer (process-buffer proc)
             (run-hooks 'w--pytest-finished-hooks))))
        (display-buffer comint-buffer)))))

(provide 'w--pytest)
;;; w--pytest.el ends here
