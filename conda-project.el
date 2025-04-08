;;; conda-project.el --- Work with conda-project environments -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Gilbert Wong

;; Author: Gilbert <gilbertwong96@icloud.com>
;; Maintainer: Gilbert <gilbertwong96@icloud.com>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; URL:  http://github.com/gilbertwong96/conda-project.el
;; Package-Version: 0.1.0
;; Package-file: conda-project.el
;; Package-Requires: ((emacs "28.1") (s "1.13.0") (yaml "0.1.1") (pythonic "0.2.0") (transient "0.8.6"))
;;
;; Keywords: python, conda-project, tools

;;; Commentary:

;; This package provides an interface to work with conda-project environments.
;; It wraps the conda-project command line tool to provide seamless integration
;; with Emacs.

;;; Code:

(require 's)
(require 'cl-lib)
(require 'yaml)
(require 'pythonic)
(require 'gud)
(require 'esh-util)
(require 'transient)
(require 'term)

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

(defcustom conda-project-env-mode-color "#5ABE37"
  "Set the color of the conda project env status in mode line."
  :type 'color
  :group 'conda-project)

(defcustom conda-project-output-height 15
  "Set the output window height of the conda-project."
  :type 'integer
  :group 'conda-project)

(defvar-local conda-project-env-current-name nil
  "Current conda project environment name.  Should always be buffer-local.")

(defvar conda-project-mode-line
  '(:propertize
    (:eval (when conda-project-env-current-name
             (propertize (concat "🅒 " conda-project-env-current-name " ")
                         'face `(:foreground ,conda-project-env-mode-color :weight bold))))
    help-echo "Current conda project env"))

(defun conda-project-mode-line-setup ()
  "Setup a basic mode-line display of current env."
  (interactive)
  (add-to-list 'mode-line-misc-info  conda-project-mode-line))

(defun conda-project--find-root ()
  "Find the path fo the project root.
Return nil if path cannot be found."
  (interactive)
  (conda-project--locate-project-root (conda-project--get-current-dir)))

(defun conda-project--get-current-dir ()
  "Get current dir."
  (if buffer-file-name
      (file-name-directory buffer-file-name)
    default-directory))

(defun conda-project--locate-project-root (current-dir)
  "Find the project root dir based on current `CURRENT-DIR'.
Return the project root path or if the conda-project root dir
cannot be found, then return nil."
  (if (conda-project--check-config-files current-dir) current-dir
    (if (conda-project--directory-is-root-p current-dir) nil
      (conda-project--locate-project-root
       (file-name-directory (directory-file-name current-dir))))))

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
    (let* ((project-yml-file (file-name-concat project-root "conda-project.yml"))
           (envs (conda-project--get-envs-from-yml project-yml-file)))
      (conda-project--select-env envs))))

(defun conda-project--get-envs-from-yml (project-yml-file)
  "Get envs from `PROJECT-YML-FILE'.
Note that the env is a hash table"
  (interactive)
  (let ((yml-content (with-temp-buffer (insert-file-contents project-yml-file)
                                       (buffer-string))))
    (gethash "environments" (yaml-parse-string yml-content :object-key-type 'string))))

(defun conda-project--select-env (envs)
  "Let user select the env from the hash table `ENVS'."
  (let ((envs-count (hash-table-count envs)))
    (cond ((< envs-count 1) nil)
          ;; Get the only environment from envs
          ((= envs-count 1)
           (catch 'get-key
             (maphash (lambda (k _) (throw 'get-key k)) envs) nil))
          ;; Let use select the environment
          ((> envs-count 1)
           (let ((env-names (cl-loop for k being the hash-keys of envs collect k)))
             (completing-read "Select environment:" env-names))))))

(defun conda-project--get-env-bin-path (project-dir env)
  "Return the env bin path based on `PROJECT-DIR' and `ENV'."
  (file-name-concat project-dir "envs" env "bin"))

(defun conda-project--append-path (env-bin-path)
  "Append `ENV-BIN-PATH' to `EXE-PATH' and `PATH' environment variable."
  ;; Make buffer-local variable
  (make-local-variable 'exec-path)
  (setq exec-path (cons env-bin-path (exec-path)))
  (setenv "PATH" (concat env-bin-path path-separator (getenv "PATH"))))

(defun conda-project--remove-path (project-path)
  "Remove the `PROJECT-PATH' from `EXEC-PATH' and `PATH' environment variable."
  (let ((path-remover (lambda (path) (string-match project-path path))))
    (make-local-variable 'exec-path)
    (setq exec-path (cl-remove-if path-remover exec-path))
    (setenv "PATH" (s-with (getenv "PATH")
                     (s-split path-separator)
                     (cl-remove-if path-remover)
                     (s-join path-separator)))))

(defun conda-project--eshell-update-path ()
  "Update `eshell-path-env' from the current `PATH'."
  (with-suppressed-warnings ((obsolete eshell-path-env))
    (if (version< emacs-version "29.1")
        (setq eshell-path-env (getenv "PATH"))
      (setq-default eshell-path-env-list exec-path))))

(defun conda-project--set-env-gud-pdb-command-name ()
  "When in a conda-project environment, call pdb as \\[python -m pdb]."
  (setq-default gud-pdb-command-name "python -m pdb"))

(defun conda-project--set-system-gud-pdb-command-name ()
  "Set the system \\[pdb] command."
  (setq-default gud-pdb-command-name conda-project-system-gud-pdb-command-name))

(defun conda-project--env-activate (project-root env)
  "Activate the conda environment with `PROJECT-ROOT' and `ENV'."

  ;; first, deactivate any existing env
  (conda-project--env-deactivate project-root)

  ;; Use pythonic to activate the environment so that anaconda-mode and
  ;; others know how to work on this
  (pythonic-activate project-root)
  (setq python-shell-virtualenv-root project-root)

  (let ((env-bin-path (conda-project--get-env-bin-path project-root env)))
    (conda-project--append-path env-bin-path)
    (conda-project--eshell-update-path)
    (conda-project--set-env-gud-pdb-command-name)
    (setq conda-project-env-current-name env)))

(defun conda-project--env-deactivate (project-root)
  "Deactivate `ENV' in current `PROJECT-ROOT'."
  (setq python-shell-virtualenv-root nil)
  (pythonic-deactivate)
  (conda-project--remove-path project-root)
  (conda-project--eshell-update-path)
  (conda-project--set-system-gud-pdb-command-name)
  (setq conda-project-env-current-name nil))

(defun conda-project--env-reader (prompt _initial-input _history)
  "Read the specific env with `PROMPT' based on conda-project.yml."
  (if-let* ((project-root (conda-project--find-root))
            (project-yml-file (file-name-concat project-root "conda-project.yml"))
            (is-file-exist (file-exists-p project-yml-file))
            (envs (conda-project--get-envs-from-yml project-yml-file))
            (env-names (cl-loop for k being the hash-keys of envs collect k)))
      (completing-read prompt env-names)
    (message "No environment found")
    nil))

;;;###autoload
(defun conda-project-env-activate-for-buffer ()
  "Activate the conda project environment for the current buffer."
  (interactive)
  (when-let* ((project-root (conda-project--find-root))
              (inferred-env (conda-project--infer-env-from-buffer project-root)))
    (conda-project--env-activate project-root inferred-env)))

;;;###autoload
(defun conda-project-env-deactivate-for-buffer ()
  "Deactivate the conda project environment for the current buffer."
  (interactive)
  (when-let ((project-root (conda-project--find-root)))
    (conda-project--env-deactivate project-root)))

(transient-define-infix conda-project--project-directory ()
  "Specify project directory."
  :description "Project directory (defaults to current directory)"
  :class 'transient-option
  :key "-d"
  :argument "--directory "
  :reader #'transient-read-directory)

(transient-define-infix conda-project--init-platforms ()
  :description "Input comma separated list of platforms"
  :class 'transient-option
  :key "-p"
  :argument "--platforms "
  :reader (lambda (prompt _initial-input _history)
            (read-string prompt "linux-64,osx-arm64,win-64,osx-64")))

(transient-define-infix conda-project--install-as-platform ()
  :description "Prepare the conda environment assuming a different platform/subdir name."
  :class 'transient-option
  :key "-p"
  :argument "--as-platform "
  :choices '("linux-64" "osx-arm64" "win-64" "osx-64"))

(transient-define-infix conda-project--project-archive ()
  :description "Extract and run directly from a project archive."
  :class 'transient-option
  :key "-p"
  :argument "--project-archive "
  :reader #'read-string)

(transient-define-infix conda-project--archive-storage-options ()
  :description ""
  :class 'transient-option
  :key "-a"
  :argument "--archive-storage-options "
  :reader #'read-string)

(transient-define-infix conda-project--from-env ()
  :description "Initialize the environment from an existing env"
  :class 'transient-option
  :key "-f"
  :argument "--from-environment "
  :reader #'conda-project--env-reader)

(transient-define-infix conda-project--set-environment ()
  :description "Specify the environment"
  :class 'transient-option
  :key "-e"
  :argument "--environment "
  :reader #'conda-project--env-reader)

(transient-define-infix conda-project--external-environment ()
  :description "Specify the name or prefix path to a conda environment"
  :class 'transient-option
  :key "-E"
  :argument "--external-environment "
  :reader #'read-string)

(transient-define-infix conda-project--set-channel ()
  :description "Specify the channel"
  :class 'transient-option
  :key "-c"
  :argument "--channel "
  :reader #'read-string)

(transient-define-suffix conda-project--init-suffix (packages)
  "Execute conda-project init command."
  :description "Initialize Project"
  :key "i"
  (interactive (list (read-string "Packages: ")))
  (let* ((trans-args (transient-args (oref transient-current-prefix command)))
         (args (mapcar (lambda (arg) (s-split " " arg)) trans-args))
         (package-list (s-split " " packages))
         (flatten-args (flatten-list (append args package-list))))
    (conda-project--execute-command (cons "init" flatten-args))))

(defmacro conda-project--read-env-in-suffix ()
  "Generate code to read env based on conda-project.yml."
  `(if-let* ((trans-args (or (transient-args (oref transient-current-prefix command))
                             ;; Ensure transient args non-nil
                             '("")))
             (dir-finder (lambda (str) (string-match "--directory" str)))
             (project-dir (if-let ((found-dir (cl-find-if dir-finder trans-args)))
                              (cl-second (s-split " " found-dir))
                            (conda-project--get-current-dir)))
             (project-yml-file (file-name-concat project-dir "conda-project.yml"))
             (is-file-exists (file-exists-p project-yml-file))
             (envs (conda-project--get-envs-from-yml project-yml-file))
             (env-names (cl-loop for k being the hash-keys of envs collect k))
             (is-env-exist (length> env-names 0)))
       (completing-read "environment: " env-names)
     (error "No environment found")))

(transient-define-suffix conda-project--lock-suffix (env)
  "Execute conda-project init command."
  :description "Lock conda environments"
  :key "l"
  (interactive (list (conda-project--read-env-in-suffix)))
  (let* ((trans-args (transient-args (oref transient-current-prefix command)))
         (args (mapcar (lambda (arg) (s-split " " arg)) trans-args))
         (flatten-args (flatten-list (append args (list env)))))
    (conda-project--execute-command (cons "lock" flatten-args))))

(transient-define-suffix conda-project--check-suffix ()
  "Execute conda-project check command."
  :description "Check the project for inconsistencies or errors."
  :key "c"
  (interactive)
  (let* ((trans-args (transient-args (oref transient-current-prefix command)))
         (args (mapcar (lambda (arg) (s-split " " arg)) trans-args))
         (flatten-args (flatten-list args)))
    (conda-project--execute-command (cons "check" flatten-args))))

(transient-define-suffix conda-project--install-suffix (env)
  "Execute conda-project install command."
  :description "Install the packages into the conda environments"
  :key "i"
  (interactive (list (conda-project--read-env-in-suffix)))
  (let* ((trans-args (transient-args (oref transient-current-prefix command)))
         (args (mapcar (lambda (arg) (s-split " " arg)) trans-args))
         (flatten-args (flatten-list args)))
    (conda-project--execute-command (cons "install" flatten-args))))

(transient-define-suffix conda-project--add-packages-suffix (packages)
  "Add the specified packages."
  :description "Add packages"
  :key "a"
  (interactive (list (read-string "Add packages: ")))
  (let* ((trans-args (transient-args (oref transient-current-prefix command)))
         (packages-list (cl-remove-if #'string-empty-p (s-split " " packages)))
         (args (mapcar (lambda (arg) (s-split " " arg)) trans-args))
         (flatten-args (flatten-list (append (append args packages-list)))))
    (conda-project--execute-command (cons "add" flatten-args))))

(transient-define-suffix conda-project--remove-packages-suffix (packages)
  "Add the specified packages."
  :description "Remove packages"
  :key "r"
  (interactive (list (read-string "Remove packages: ")))
  (let* ((trans-args (transient-args (oref transient-current-prefix command)))
         (packages-list (cl-remove-if #'string-empty-p (s-split " " packages)))
         (args (mapcar (lambda (arg) (s-split " " arg)) trans-args))
         (flatten-args (flatten-list (append (append args packages-list)))))
    (conda-project--execute-command (cons "remove" flatten-args))))

(transient-define-suffix conda-project--clean-suffix (env)
  "Execute conda-project install command."
  :description "Clean the conda environment"
  :key "c"
  (interactive (list (conda-project--read-env-in-suffix)))
  (let* ((trans-args (transient-args (oref transient-current-prefix command)))
         (args (mapcar (lambda (arg) (s-split " " arg)) trans-args))
         (flatten-args (flatten-list (append args (list env)))))
    (conda-project--execute-command (cons "clean" flatten-args))))

(transient-define-suffix conda-project--run-suffix (command)
  "Execute conda-project run command."
  :description "Run commands in project environments."
  :key "r"
  (interactive (list (read-string "Run command: ")))
  (let* ((trans-args (transient-args (oref transient-current-prefix command)))
         (args (mapcar (lambda (arg) (s-split " " arg)) trans-args))
         (flatten-args (flatten-list (append args (list command)))))
    (conda-project--execute-command (cons "run" flatten-args))))

(transient-define-prefix conda-project-init ()
  "Initialize a new project."
  ["Options:"
   ("-n" "Name for the project" "--name=" :always-read t :allow-empty t)
   (conda-project--project-directory)
   (conda-project--init-platforms)
   ("-l" "Create the conda-lock.<env>.yml" "--lock")
   ("-i" "Create the local conda environment" "--install")
   (conda-project--from-env)]
  ["Init:" (conda-project--init-suffix)])

(transient-define-prefix conda-project-lock ()
  "Lock conda environments by creating conda-lock.<env>.yml files."
  ["Options:"
   (conda-project--project-directory)
   (conda-project--project-archive)
   (conda-project--archive-storage-options)
   ("-f" "Remove and recreate existing conda-lock.<env>.yml file(s" "--force")]
  ["Lock:" (conda-project--lock-suffix)])

(transient-define-prefix conda-project-check ()
  "Check the project for inconsistencies or errors."
  ["Options:"
   (conda-project--project-directory)
   (conda-project--project-archive)
   (conda-project--archive-storage-options)]
  ["Check:" (conda-project--check-suffix)])

(transient-define-prefix conda-project-install ()
  "Install the packages into the conda environments."
  ["Options:"
   (conda-project--project-directory)
   (conda-project--project-archive)
   (conda-project--archive-storage-options)
   (conda-project--install-as-platform)
   ("-A" "Check or prepare all defined environments" "--all")
   ("-c" "Check that the prepared conda environment" "--check-only")
   ("-f" "Remove and recreate an existing environment" "--force")]
  ["Install:" (conda-project--install-suffix)])

(transient-define-prefix conda-project-add ()
  "Add packages to the environment."
  ["Options:"
   (conda-project--project-directory)
   (conda-project--project-archive)
   (conda-project--archive-storage-options)
   (conda-project--set-environment)
   (conda-project--set-channel)]
  ["Add:" (conda-project--add-packages-suffix)])

(transient-define-prefix conda-project-remove ()
  "Remove packages from environment."
  ["Options:"
   (conda-project--project-directory)
   (conda-project--project-archive)
   (conda-project--archive-storage-options)]
  ["Remove:"
   (conda-project--remove-packages-suffix)])

(transient-define-prefix conda-project-clean ()
  "Clean the conda environments."
  ["Options:"
   (conda-project--project-directory)
   ("-a" "Prepare all defined environments" "--all")]
  ["Clean" (conda-project--clean-suffix)])

(transient-define-prefix conda-project-run ()
  "Run commands in project environments."
  ["Options:"
   (conda-project--project-directory)
   (conda-project--project-archive)
   (conda-project--archive-storage-options)
   (conda-project--set-environment)
   (conda-project--external-environment)]
  ["Run" (conda-project--run-suffix)])

(defalias 'conda-project-activate #'conda-project-env-activate-for-buffer)
(defalias 'conda-project-deactivate #'conda-project-env-deactivate-for-buffer)

(defun conda-project--execute-command (args-list)
  "Execute conda-project `COMMAND' with `ARGS-LIST'.
If `SHOW-BUFFER' is non-nil, display the output buffer."
  (let* ((command "conda-project")
         (buffer-name "*Conda Project Output*")
         (output-buffer (progn
                          (when (get-buffer buffer-name)
                            (kill-buffer buffer-name))
                          (get-buffer-create buffer-name))))

    ;; Execute command
    (with-current-buffer output-buffer
      (term-mode)
      (term-exec output-buffer "Conda Project" command nil args-list)
      ;; After command completed, change back to conda-project-output-mode
      (set-process-sentinel
       (get-buffer-process output-buffer)
       (lambda (_process event)
         (with-current-buffer output-buffer
           (save-excursion (goto-char (point-max)) (insert event))
           (conda-project-output-mode)))))

    ;; Open window from bottom
    (let ((bottom-window (split-window (selected-window)
                                       (- conda-project-output-height)
                                       'below))
          (orig-window (selected-window)))
      (select-window bottom-window)
      (switch-to-buffer output-buffer)
      (set-window-dedicated-p bottom-window t)
      (select-window orig-window))))


;;;###autoload
(define-minor-mode conda-project-env-autoactivate-mode
  "Toggle conda-env-autoactivate mode.

This mode automatically tries to activate a conda environment for the current
buffer."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter nil
  ;; The minor mode bindings.
  :keymap nil
  ;; Kwargs
  :group 'conda-project
  :global t
  ;; Forms
  (conda-project-env-activate-for-buffer)
  (conda-project-mode-line-setup))

(defvar conda-project-output-mode-tool-bar-map
  (let ((map (make-sparse-keymap)))
    (tool-bar-local-item "close" 'quit-window 'quit map
                         :help "Quit Conda Project Output"
                         :vert-only t)
    map))

;;;###autoload
(define-derived-mode conda-project-output-mode special-mode "Conda Project"
  "Major mode for viewing conda-project output and navigating content in it.

Commands:
\\{conda-project-output-mode-map}"
  (setq-local tool-bar-map
              help-mode-tool-bar-map))


(provide 'conda-project)
;;; conda-project.el ends here
