
# Table of Contents

1.  [Preface](#org9cac999)
    1.  [Package infrastructure initialization](#org151e79d)
    2.  [Set-up `use-package`](#orgb1f5c27)
2.  [Aesthetics](#org8bf07bc)
    1.  [Theme](#org289f701)
    2.  [Font](#org8274b02)
    3.  [Rainbow delimiters](#orga14ceb3)
    4.  [Ruler at 80 characters](#org3afa5f9)
    5.  [Miscellaneous](#org061f97e)
3.  [Org Mode](#orgff5d0a0)
    1.  [Setup](#org323bde1)
4.  [Python IDE packages](#orgdbf9e2c)
    1.  [Elpy](#orgf0a07e0)
    2.  [py-autopep8](#orgef9afa4)
    3.  [Blacken](#orgbf057a3)
    4.  [Flycheck](#orge8f83fd)
5.  [Various IDE packages](#org1d38dc1)
    1.  [yaml-mode](#org3b6370b)
    2.  [Clojure mode](#org589a2f2)
    3.  [emacs-neotree](#org02d1590)
    4.  [Terminal emulator](#org7f6d63f)
6.  [Miscellaneous](#org6b0fd59)
    1.  [Normal copy-pasting in Windows](#org6e97787)

Create `init.el` by tangling the contents of the code blocks: "C-c C-v t" (`org-babel-tangle`). Furthermore, it is worth exporting the current file in markdown to create a beautiful README for Github: "C-c C-e m m" (`org-md-export-to-markdown`).


<a id="org9cac999"></a>

# Preface


<a id="org151e79d"></a>

## Package infrastructure initialization

    ;; Enables basic package supporting
    (require 'package)
    
    ;; Initialize the package infrastructure
    (package-initialize)
    
    ;; Add several archives to list of available repositories
    (setq package-archives '(("org"
    			  .
    			  "https://orgmode.org/elpa/")
    			 ("stable-melpa"
    			  .
    			  "https://stable.melpa.org/packages/")
    			 ("melpa"
    			  .
    			  "https://melpa.org/packages/")
    			 ("gnu"
    			  .
    			  "https://elpa.gnu.org/packages/")
    			))


<a id="orgb1f5c27"></a>

## Set-up `use-package`

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)
    (eval-when-compile (require 'use-package))


<a id="org8bf07bc"></a>

# Aesthetics


<a id="org289f701"></a>

## Theme

I still haven't been able to figure out when to use `use-package` or `load-theme` in this context.

    (use-package material-theme
      :ensure t
      :config (load-theme 'material t))


<a id="org8274b02"></a>

## Font

Absolutely always [Hack](https://sourcefoundry.org/hack/).

    (custom-set-faces
     '(default ((t (:height 160 :family "Hack")))))


<a id="orga14ceb3"></a>

## Rainbow delimiters

    (use-package rainbow-delimiters
      :ensure t
      :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


<a id="org3afa5f9"></a>

## Ruler at 80 characters

`display-fill-column-indicator-mode` was introduced with Emacs 27, so the version ought to be checked before adding the hook.

    (if (version< "27.0" emacs-version)
        (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))


<a id="org061f97e"></a>

## Miscellaneous

Disable menu bar.

    (setq menu-bar-mode -1)

Disable tool bar.

    (setq tool-bar-mode -1)

Hide startup message.

    (setq inhibit-startup-message t)

Hide splash screen.

    (setq inhibit-splash-screen t)

Enable line numbers globally, format them, and add a solid vertical bar.


<a id="orgff5d0a0"></a>

# Org Mode


<a id="org323bde1"></a>

## Setup

    (use-package org
      :ensure t
      :mode ("\\.org$" . org-mode)
      :config
      (setq org-startup-truncated t)
      ;; Custom todo keyword sequence and colours
      (setq org-todo-keywords '((sequence
    			    "TODO"
    			    "IN-PROGRESS"
    			    "WAITING"
    			    "CANCELLED"
    			    "DONE"))
    	org-todo-keyword-faces '(("TODO" . "white")
    				 ("IN-PROGRESS" . "orange")
    				 ("WAITING" . "red")
    				 ("CANCELLED" . "black")
    				 ("DONE" . "green")))
      ;; Linking shortcuts
      (global-set-key (kbd "C-c l") 'org-store-link)
      (global-set-key (kbd "C-c C-l") 'org-insert-link)
      (global-set-key (kbd "C-c a") 'org-agenda))


<a id="orgdbf9e2c"></a>

# Python IDE packages


<a id="orgf0a07e0"></a>

## [Elpy](https://github.com/jorgenschaefer/elpy)

Emacs Python IDE, which I'm pretty sure I don't use it to its full extent.

    (use-package elpy
      :ensure t
      :init (elpy-enable))


<a id="orgef9afa4"></a>

## [py-autopep8](https://github.com/paetzke/py-autopep8.el)

Code formatting according to [PEP 8](https://www.python.org/dev/peps/pep-0008/) upon save.

    (use-package py-autopep8
      :ensure t
      :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


<a id="orgbf057a3"></a>

## [Blacken](https://github.com/pythonic-emacs/blacken)

Code formatting according by [black](https://github.com/psf/black).

    (use-package blacken
      :ensure t
      :init 'blacken-mode)


<a id="orge8f83fd"></a>

## [Flycheck](https://www.flycheck.org/en/latest/)

Flycheck is not exclusive to Python, but it is set up only for it since I mainly develop in Python.

    (use-package flycheck
      :ensure t
      :init
      (when (require 'flycheck nil t)
        (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
        (add-hook 'elpy-mode-hook 'flycheck-mode)))


<a id="org1d38dc1"></a>

# Various IDE packages


<a id="org3b6370b"></a>

## [yaml-mode](https://github.com/yoshiki/yaml-mode)

    (use-package yaml-mode :ensure t)


<a id="org589a2f2"></a>

## [Clojure mode](https://github.com/clojure-emacs/clojure-mode/)

    (use-package clojure-mode :ensure t)


<a id="org02d1590"></a>

## [emacs-neotree](https://github.com/jaypei/emacs-neotree)

    (use-package neotree
      :ensure t
      :init
      (global-set-key [f8] 'neotree-toggle)
      (setq-default neo-show-hidden-files t))


<a id="org7f6d63f"></a>

## Terminal emulator

Binds a specific configuration of `ansi-term` to "C-c b". I don't much fancy how the windows are splitted. [emacs-libvterm](https://github.com/akermu/emacs-libvterm) might be a worthy replacement.

    (defun ml/bash ()
      "Start a terminal emulator in a new window"
      (interactive)
      (split-window-sensibly)
      (other-window 1)
      (ansi-term (executable-find "bash")))
    (global-set-key (kbd "C-c b") #'ml/bash)


<a id="org6b0fd59"></a>

# Miscellaneous


<a id="org6e97787"></a>

## Normal copy-pasting in Windows

I believe copy-pasting (in the Emacs sense of it) stopped to working with Emacs 27. The following code block fixes it.

    (if (eq system-type 'windows-nt)
        (progn
          (set-clipboard-coding-system 'utf-16-le)
          (set-selection-coding-system 'utf-16-le))
      (set-selection-coding-system 'utf-8))
    (setq locale-coding-system 'utf-8)
    (set-terminal-coding-system 'utf-8)
    (set-keyboard-coding-system 'utf-8)
    (prefer-coding-system 'utf-8)

