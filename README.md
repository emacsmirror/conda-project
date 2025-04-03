[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# conda-project

[![CI](https://github.com/gilbertwong96/conda-project/actions/workflows/test.yml/badge.svg)](https://github.com/gilbertwong96/conda-project/actions/workflows/test.yml)

> Control conda-project CLI in Emacs

conda-project.el provides a convenient Emacs interface for working with the conda-project CLI tool,
allowing you to create, manage, and activate Conda environments directly from Emacs. It's designed
to make project-specific environment management seamless for Emacs users who work with Python
projects.


## Features

- Create and manage Conda environments based on project configuration
- Seamless integration with the conda-project CLI
- Activate/deactivate environments for your current buffer or project
- Auto-detect and use the correct environment based on conda-project.yml files
- Interactive environment selection with completion
- Transient interface for easy command access

## Requirements

- Emacs 28.1 or later
- [conda-project][conda-project] CLI tool installed

## Installation

### Manual Installation

Clone this repository and add it to your load path:

``` emacs-lisp
(add-to-list 'load-path "/path/to/conda-project.el")
(require 'conda-project)
```

### Using straight.el with use-package

``` emacs-lisp
(use-package conda-project
  :straight (:host github :repo "gilbertwong96/conda-project")
  :config
  (conda-project-initialize))
```

### Using MELPA (once available)

``` emacs-lisp
(use-package conda-project
  :ensure t
  :config
  (conda-project-initialize))
```

## Usage

**Interactive Commands**

- `M-x conda-project-activate` - Activate the conda project environment for the current buffer
- `M-x conda-project-deactivate` - Deactivate the conda project environment for the current buffer
- `M-x conda-project-init` - Initialize a new project.
- `M-x conda-project-lock` - Lock conda environments by creating conda-lock.<env>.yml files
- `M-x conda-project-check` - Check the project for inconsistencies or errors
- `M-x conda-project-install` - Install the packages into the conda environments
- `M-x conda-project-add` - Add packages to the environment
- `M-x conda-project-remove` - Remove packages from the environment
- `M-x conda-project-clean` - Clean the conda environments
- `M-x conda-project-run` - Run commands in project environments

## Configuration

Basic setup:

``` emacs-lisp
(require 'conda-project)
(add-hook 'python-mode-hook 'conda-project-env-autoactivate-mode)
```

Use [general][general] with [evil][evil] to define local leader keybindings:

``` emacs-lisp
(general-create-definer local-leader-def
    :states '(normal insert emacs)
    :prefix evil-local-leader
    :non-normal-prefix emacs-local-leader
    :prefix-command 'local-leader-prefix-command
    :prefix-map 'local-leader-prefix-map)

(local-leader-def
    "c" '(:ignore t :which-key "conda-project")
    "cI" '(conda-project-init :which-key "Init a new project")
    "cl" '(conda-project-lock :which-key "Lock environment")
    "cc" '(conda-project-check :which-key "Check inconsistencies or errors")
    "ci" '(conda-project-install :which-key "Install packages")
    "ca" '(conda-project-add :which-key "Add packages")
    "cr" '(conda-project-remove :which-key "Remove packages")
    "cC" '(conda-project-clean :which-key "Clean environment")
    "cR" '(conda-project-run :which-key "Run command in specific environment"))
```

## License

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.


[conda-project]: https://github.com/conda-incubator/conda-project
[general]: https://github.com/noctuid/general.el
[evil]: https://github.com/emacs-evil/evil
