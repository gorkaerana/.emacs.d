
# Table of Contents

1.  [Preface](#org61734fa)
    1.  [Package infrastructure initialization](#org59543c7)
    2.  [Set-up `use-package`](#org22a3e4e)
2.  [Aesthetics](#org4348b8e)
    1.  [Theme](#org640a55f)
    2.  [Font](#org851bebc)
    3.  [Rainbow delimiters](#org037d448)
    4.  [Ruler at 80 characters](#orgc84dece)
    5.  [Miscellaneous](#org9330487)
3.  [Org Mode](#org29807a2)
    1.  [Setup](#org03fec51)
4.  [Python IDE packages](#orgf3e6d32)
    1.  [Elpy](#orge32096e)
    2.  [py-autopep8](#org8b3bd60)
    3.  [Blacken](#orge2dcdc7)
    4.  [Flycheck](#org9cc88f9)
5.  [Various IDE packages](#orgf98775b)
    1.  [yaml-mode](#orgc950010)
    2.  [Clojure mode](#orgadcf9c8)
    3.  [emacs-neotree](#orgc63c904)
    4.  [Terminal emulator](#orge0bcd5c)
    5.  [Magit](#org733d088)
6.  [Miscellaneous](#orgd1c4137)
    1.  [Normal copy-pasting in Windows](#orgde07429)
    2.  [toc-org](#org496e855)
    3.  [asd](#orgea9e538)

t#+TITLE: Literate emacs init

Create `init.el` by tangling the contents of the code blocks: "C-c C-v t" (`org-babel-tangle`). Furthermore, it is worth exporting the current file in markdown to create a beautiful README for Github: "C-c C-e m m" (`org-md-export-to-markdown`).


<a id="org61734fa"></a>

# Preface


<a id="org59543c7"></a>

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


<a id="org22a3e4e"></a>

## Set-up `use-package`

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)
    (eval-when-compile (require 'use-package))


<a id="org4348b8e"></a>

# Aesthetics


<a id="org640a55f"></a>

## Theme

I still haven't been able to figure out when to use `use-package` or `load-theme` in this context.

    (use-package material-theme
      :ensure t
      :config (load-theme 'material t))


<a id="org851bebc"></a>

## Font

Absolutely always [Hack](https://sourcefoundry.org/hack/).

    (custom-set-faces
     '(default ((t (:height 160 :family "Hack")))))


<a id="org037d448"></a>

## Rainbow delimiters

    (use-package rainbow-delimiters
      :ensure t
      :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


<a id="orgc84dece"></a>

## Ruler at 80 characters

`display-fill-column-indicator-mode` was introduced with Emacs 27, so the version ought to be checked before adding the hook.

    (if (version< "27.0" emacs-version)
        (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))


<a id="org9330487"></a>

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


<a id="org29807a2"></a>

# Org Mode


<a id="org03fec51"></a>

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


<a id="orgf3e6d32"></a>

# Python IDE packages


<a id="orge32096e"></a>

## [Elpy](https://github.com/jorgenschaefer/elpy)

Emacs Python IDE, which I'm pretty sure I don't use it to its full extent.

    (use-package elpy
      :ensure t
      :init (elpy-enable))


<a id="org8b3bd60"></a>

## [py-autopep8](https://github.com/paetzke/py-autopep8.el)

Code formatting according to [PEP 8](https://www.python.org/dev/peps/pep-0008/) upon save.

    (use-package py-autopep8
      :ensure t
      :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


<a id="orge2dcdc7"></a>

## [Blacken](https://github.com/pythonic-emacs/blacken)

Code formatting according by [black](https://github.com/psf/black).

    (use-package blacken
      :ensure t
      :init 'blacken-mode)


<a id="org9cc88f9"></a>

## [Flycheck](https://www.flycheck.org/en/latest/)

Flycheck is not exclusive to Python, but it is set up only for it since I mainly develop in Python.

    (use-package flycheck
      :ensure t
      :init
      (when (require 'flycheck nil t)
        (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
        (add-hook 'elpy-mode-hook 'flycheck-mode)))


<a id="orgf98775b"></a>

# Various IDE packages


<a id="orgc950010"></a>

## [yaml-mode](https://github.com/yoshiki/yaml-mode)

    (use-package yaml-mode :ensure t)


<a id="orgadcf9c8"></a>

## [Clojure mode](https://github.com/clojure-emacs/clojure-mode/)

    (use-package clojure-mode :ensure t)


<a id="orgc63c904"></a>

## [emacs-neotree](https://github.com/jaypei/emacs-neotree)

    (use-package neotree
      :ensure t
      :init
      (global-set-key [f8] 'neotree-toggle)
      (setq-default neo-show-hidden-files t))


<a id="orge0bcd5c"></a>

## Terminal emulator

Binds a specific configuration of `ansi-term` to "C-c b". I don't much fancy how the windows are splitted. [emacs-libvterm](https://github.com/akermu/emacs-libvterm) might be a worthy replacement.

    (defun ml/bash ()
      "Start a terminal emulator in a new window"
      (interactive)
      (split-window-sensibly)
      (other-window 1)
      (ansi-term (executable-find "bash")))
    (global-set-key (kbd "C-c b") #'ml/bash)


<a id="org733d088"></a>

## [Magit](https://magit.vc/)

    (use-package magit :ensure t)


<a id="orgd1c4137"></a>

# Miscellaneous


<a id="orgde07429"></a>

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


<a id="org496e855"></a>

## [toc-org](https://github.com/snosov1/toc-org)


### TODO. Use it so that the table of contents for the Github README is generated automatically upon save.


<a id="orgea9e538"></a>

## asd

    (setq markdown-fontify-code-blocks-natively t)

