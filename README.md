
# Table of Contents

1.  [Preface](#org457a161)
    1.  [Package infrastructure initialization](#org79479f9)
    2.  [Set-up `use-package`](#orge2cfca9)
2.  [Aesthetics](#org2886fbd)
    1.  [Theme](#org8ee4902)
    2.  [Font](#orgb15e9a8)
    3.  [Rainbow delimiters](#org3f4afcd)
    4.  [Ruler at 80 characters](#org4f229ae)
    5.  [Miscellaneous](#org137d94c)
3.  [Org Mode](#orge51844d)
    1.  [Setup](#org546f64b)
4.  [Python IDE packages](#org9fe734e)
    1.  [Elpy](#org5824cfa)
    2.  [py-autopep8](#orgc9ebab0)
    3.  [Blacken](#org35c32f5)
    4.  [Flycheck](#org94b02a3)
5.  [Various IDE packages](#org9f8e73c)
    1.  [yaml-mode](#org3280ebe)
    2.  [Clojure mode](#orga7186a4)
    3.  [emacs-neotree](#orgf3753b8)
    4.  [Terminal emulator](#org2cfdf58)
6.  [Miscellaneous](#org6dcd0fe)
    1.  [Normal copy-pasting in Windows](#orgfa9d73d)



<a id="org457a161"></a>

# Preface


<a id="org79479f9"></a>

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


<a id="orge2cfca9"></a>

## Set-up `use-package`

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)
    (eval-when-compile (require 'use-package))


<a id="org2886fbd"></a>

# Aesthetics


<a id="org8ee4902"></a>

## Theme

I still haven't been able to figure out when to use `use-package` or `load-theme` in this context

    (use-package material-theme
      :ensure t
      :config (load-theme 'material t))


<a id="orgb15e9a8"></a>

## Font

Absolutely always [Hack](https://sourcefoundry.org/hack/)

    (custom-set-faces
     '(default ((t (:height 160 :family "Hack")))))


<a id="org3f4afcd"></a>

## Rainbow delimiters

    (use-package rainbow-delimiters
      :ensure t
      :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


<a id="org4f229ae"></a>

## Ruler at 80 characters

`display-fill-column-indicator-mode` was introduced with Emacs 27, so the version ought to be checked before adding the hook.

    (if (version< "27.0" emacs-version)
        (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))


<a id="org137d94c"></a>

## Miscellaneous

Disable menu bar

    (setq menu-bar-mode -1)

Disable tool bar

    (setq tool-bar-mode -1)

Hide startup message

    (setq inhibit-startup-message t)

Hide splash screen

    (setq inhibit-splash-screen t)

Enable line numbers globally, format them, and add a solid vertical bar


<a id="orge51844d"></a>

# Org Mode


<a id="org546f64b"></a>

## Setup

    (use-package org
      :ensure t
      :mode ("\\.org$" . org-mode)
      :config
      (setq org-startup-truncated t)
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


<a id="org9fe734e"></a>

# Python IDE packages


<a id="org5824cfa"></a>

## [Elpy](https://github.com/jorgenschaefer/elpy)

Emacs Python IDE, which I'm pretty sure I don't use it to its full extent.

    (use-package elpy
      :ensure t
      :init (elpy-enable))


<a id="orgc9ebab0"></a>

## [py-autopep8](https://github.com/paetzke/py-autopep8.el)

Code formatting according to [PEP 8](https://www.python.org/dev/peps/pep-0008/) upon save

    (use-package py-autopep8
      :ensure t
      :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


<a id="org35c32f5"></a>

## [Blacken](https://github.com/pythonic-emacs/blacken)

Code formatting according by [black](https://github.com/psf/black).

    (use-package blacken
      :ensure t
      :init 'blacken-mode)


<a id="org94b02a3"></a>

## [Flycheck](https://www.flycheck.org/en/latest/)

Flycheck is not exclusive to Python, but it is set up only for it since I mainly develop in Python.

    (use-package flycheck
      :ensure t
      :init
      (when (require 'flycheck nil t)
        (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
        (add-hook 'elpy-mode-hook 'flycheck-mode)))


<a id="org9f8e73c"></a>

# Various IDE packages


<a id="org3280ebe"></a>

## [yaml-mode](https://github.com/yoshiki/yaml-mode)

    (use-package yaml-mode :ensure t)


<a id="orga7186a4"></a>

## [Clojure mode](https://github.com/clojure-emacs/clojure-mode/)

    (use-package clojure-mode :ensure t)


<a id="orgf3753b8"></a>

## [emacs-neotree](https://github.com/jaypei/emacs-neotree)

    (use-package neotree
      :ensure t
      :init
      (global-set-key [f8] 'neotree-toggle)
      (setq-default neo-show-hidden-files t))


<a id="org2cfdf58"></a>

## Terminal emulator

Binds a specific configuration of `ansi-term` to "C-c b". I don't much fancy how the windows are splitted. [emacs-libvterm](https://github.com/akermu/emacs-libvterm) might be a worthy replacement.

    (defun ml/bash ()
      "Start a terminal emulator in a new window"
      (interactive)
      (split-window-sensibly)
      (other-window 1)
      (ansi-term (executable-find "bash")))
    (global-set-key (kbd "C-c b") #'ml/bash)


<a id="org6dcd0fe"></a>

# Miscellaneous


<a id="orgfa9d73d"></a>

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

