;;; conda-project.el --- Work with conda-project environments -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Gilbert Wong
;; Author: Gilbert Wong
;; URL: http://github.com/gilbertwong96/conda-project.el
;; Version: 0.1
;; Package-Version: 0.1
;; Keywords: python, conda, project, tools
;; Package-Requires: ((emacs "25.1") (s "1.13.0") (cl-lib) (yaml))

;;; Commentary:

;; This package provides an interface to work with conda-project environments.
;; It wraps the conda-project command line tool to provide seamless integration
;; with Emacs.

;;; Code:

(require 's)
(require 'cl-lib)
(require 'yaml)
(require 'pythonic)

(defgroup conda-project nil
  "Conda project environment management."
  :group 'python
  :prefix "conda-project-")

;;============================
;; Customizable Variables
;;============================

(defcustom conda-project-executable "conda-project"
  "The conda-project executable."
  :type 'string
  :group 'conda-project)

(defcustom conda-project-system-gud-pdb-command-name
  (if (boundp 'gud-pdb-command-name)
      gud-pdb-command-name
    (setq gud-pdb-command-name "python -m pdb"))
  "Whatever `gud-pdb-command-name' is (usually \\[pdb])."
  :type 'string
  :group 'conda-project)

(defvar conda-project--env-executables-dir ;; copied from conda.el
  (if (eq system-type 'windows-nt) "Scripts" "bin")
  "Name of the directory containing executables.  It is system dependent.")

(defvar conda-project-env-meta-dir "conda-meta"
  "Name of the directory containing metadata.
This should be consistent across platforms.")

(defvar-local conda-project-env-current-name nil
  "Current conda project environment name.  Should always be buffer-local.")

(defvar conda-project-mode-line
  '(:propertize
    (:eval (when conda-project-env-current-name
             (propertize (concat "🅒 " conda-project-env-current-name " ")
                         'face '(:foreground "#5ABE37" :weight bold))
             ))
    help-echo "Current conda project env"))

(defun conda-project-mode-line-setup ()
  "Setup a basic mode-line display of current env."
  (add-to-list 'mode-line-misc-info  conda-project-mode-line))

(defun conda-project--call-process (&rest args)
  "Call conda-project with ARGS and return the output.
Ensure the local variable `conda-project--project-root-path' be set first."
  ;; (message "%s %s" conda-project--project-root-path args)
  (let ((default-directory conda-project--project-root-path))
   (with-temp-buffer
     (let ((exit-code (call-process conda-project--executable nil t nil args)))
       (if (zerop exit-code)
         (message "%s" (s-trim (buffer-string)))
         (message "%s %s" exit-code (buffer-string))
         )))))

(defun conda-project--find-root ()
  "Find the path fo the project root.
Return nil if path cannot be found."
  (interactive)
  (let ((current-dir (if buffer-file-name
                         (file-name-directory buffer-file-name)
                       default-directory)))
    (conda-project--locate-project-root current-dir)))

(defun conda-project--locate-project-root (current-dir)
  "Find the project root dir based on current `CURRENT-DIR'.
Return the project root path or if the conda-project root dir
cannot be found, then return nil."
  (if (conda-project--check-config-files current-dir) current-dir
    (if (conda-project--directory-is-root-p current-dir) nil
      (conda-project--locate-project-root
       (file-name-directory (directory-file-name current-dir)))
      )))

(defun conda-project--directory-is-root-p (directory)
  "Return t if `DIRECTORY' is a conda-project root directory."
  (let* ((dir (file-name-as-directory (expand-file-name directory)))
         (parent (file-name-directory (directory-file-name dir))))
    (string= dir parent)))

(defun conda-project--check-config-files (config-dir)
  "Check if required configuration files exist in `CONFIG-DIR'.
Return nil if there are no required file `conda-project.yml' and
 `environment.yml', otherwise return t."
  (let* ((required-files '("conda-project.yml" "environment.yml"))
         (missing-files
          (cl-remove-if
           (lambda (file) (file-exists-p (expand-file-name file config-dir)))
           required-files)))
    (if missing-files nil t)))

(defun conda-project--infer-env-from-buffer (project-root)
  "Get conda-project env from the `PROJECT-ROOT'."
  (when project-root
    ;; Set project root path
    (let ((env-name (conda-project--get-name-from-env-yml
                     (file-name-concat project-root "environment.yml"))))
      env-name)))

(defun conda-project--get-name-from-env-yml (env-yml-file)
  "Get env name from `ENV-YML-FILE'."
  (when env-yml-file
    (let ((env-content (with-temp-buffer (insert-file-contents env-yml-file)
                                         (buffer-string))))
      (gethash 'name (yaml-parse-string env-content)))))

(defun conda-project--get-env-bin-path (project-dir env)
  "Return the env bin path based on `PROJECT-DIR' and `ENV'."
  (file-name-concat project-dir "envs" env "bin"))

(defun conda-project--append-path (env-bin-path)
  "Append `ENV-BIN-PATH' to 'exec-path' and `PATH' environment variable."
  ;; Make buffer-local variable
  (make-local-variable 'exec-path)
  (setq exec-path (cons env-bin-path (exec-path)))
  (setenv "PATH" (concat env-bin-path path-separator (getenv "PATH"))))

(defun conda-project--remove-path (env-bin-path)
  "Remove `ENV-BIN-PATH' from 'exec-path' and `PATH' environment variable."
  (let ((path-remover (lambda (path) (string= env-bin-path path))))
    (make-local-variable 'exec-path)
    (setq exec-path (cl-remove-if path-remover exec-path))
    (setenv "PATH" (s-with (getenv "PATH")
                     (s-split path-separator)
                     (cl-remove-if path-remover)
                     (s-join path-separator)))
    ))

(defun conda-project--eshell-update-path ()
  "Update `eshell-path-env' from the current `PATH'."
  (if (version<= emacs-version "29.1")
      (setq eshell-path-env (getenv "PATH"))
    (setq-default eshell-get-path (getenv "PATH"))))

(defun conda-project--set-env-gud-pdb-command-name ()
  "When in a conda-project environment, call pdb as \\[python -m pdb]."
  (setq-default gud-pdb-command-name "python -m pdb"))

(defun conda-project--set-system-gud-pdb-command-name ()
  "Set the system \\[pdb] command."
  (setq-default gud-pdb-command-name conda-project-system-gud-pdb-command-name))

(defun conda-project--exec-activator-for-buffer (activator)
  "Exec the specific `ACTIVATOR' for current buffer."
  (let ((project-root (conda-project--find-root)))
   (when project-root
     (let ((inferred-env (conda-project--infer-env-from-buffer project-root)))
       (when inferred-env
         (funcall activator project-root inferred-env))))))

(defun conda-project--env-activate (project-root env)
  "Activate the conda environment with `PROJECT-ROOT' and `ENV'."

  ;; first, deactivate any existing env
  (conda-project--env-deactivate project-root env)

  ;; Use pythonic to activate the environment so that anaconda-mode and
  ;; others know how to work on this
  (pythonic-activate project-root)
  (setq python-shell-virtualenv-root project-root)

  (let ((env-bin-path (conda-project--get-env-bin-path project-root env)))
    (conda-project--append-path env-bin-path)
    (conda-project--eshell-update-path)
    (conda-project--set-env-gud-pdb-command-name)
    (setq conda-project-env-current-name env)
    ))

(defun conda-project--env-deactivate (project-root env)
  "Deactivate `ENV' in current `PROJECT-ROOT'."
  (setq python-shell-virtualenv-root nil)
  (pythonic-deactivate)
  (let ((env-bin-path (conda-project--get-env-bin-path project-root env)))
    (conda-project--remove-path env-bin-path)
    (conda-project--eshell-update-path)
    (setq conda-project-env-current-name nil)
    (setenv "TERM" "dumb")
    ))

;;;###autoload
(defun conda-project-env-activate-for-buffer ()
  "Activate the conda project environment for the current buffer."
  (interactive)
  (conda-project--exec-activator-for-buffer #'conda-project--env-activate))

;;;###autoload
(defun conda-project-env-deactivate-for-buffer ()
  "Deactivate the conda project environment for the current buffer."
  (interactive)
  (conda-project--exec-activator-for-buffer #'conda-project--env-deactivate))

(defun conda-project--set-python-terminal-env (orig-fun &rest args)
  "Set term env for `ORIG-FUN' of python repl along with `ARGS'."
  (let ((process-environment (append '("TERM=xterm-256color") process-environment)))
    (run-python)j))

;;;###autoload
(define-minor-mode conda-project-env-autoactivate-mode
  "Toggle conda-env-autoactivate mode.

This mode automatically tries to activate a conda environment for the current
buffer."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter "test"
  ;; The minor mode bindings.
  :keymap nil
  ;; Kwargs
  :group 'conda-project
  :global t
  ;; Forms
  (conda-project-env-activate-for-buffer)
  (conda-project-mode-line-setup))

(provide 'conda-project)
;;; conda-project.el ends here
