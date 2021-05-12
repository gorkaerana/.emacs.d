
# Table of Contents

1.  [Preface](#orgae93242)
    1.  [Package infrastructure initialization](#orgabe2eaf)
    2.  [Set-up `use-package`](#org2614240)
2.  [Aesthetics](#orgf6c691b)
    1.  [Theme](#org31877f3)
    2.  [Font](#org3fd30e4)
    3.  [Rainbow delimiters](#org45f3737)
    4.  [Ruler at 80 characters](#orgb937552)
    5.  [Miscellaneous](#org9cd3106)
3.  [Org Mode](#orge88aceb)
    1.  [Setup](#org6b0eba7)
4.  [Python IDE packages](#org1fec9e4)
    1.  [Elpy](#org7fac7fd)
    2.  [py-autopep8](#org5ca29d9)
    3.  [Blacken](#org2efb684)
    4.  [Flycheck](#org05b55c7)
5.  [Various IDE packages](#org466b3bb)
    1.  [yaml-mode](#org6fcb4da)
    2.  [Clojure mode](#org708ac57)
    3.  [emacs-neotree](#org83972de)
    4.  [Terminal emulator](#orgcb4e5b5)
    5.  [Magit](#orga031c5e)
6.  [Miscellaneous](#orgdddf662)
    1.  [Normal copy-pasting in Windows](#orge97e81a)
    2.  [toc-org](#orgbc9a35c)

Create `init.el` by tangling the contents of the code blocks: "C-c C-v t" (`org-babel-tangle`). Furthermore, it is worth exporting the current file in markdown to create a beautiful README for Github: "C-c C-e m m" (`org-md-export-to-markdown`).


<a id="orgae93242"></a>

# Preface


<a id="orgabe2eaf"></a>

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


<a id="org2614240"></a>

## Set-up `use-package`

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)
    (eval-when-compile (require 'use-package))


<a id="orgf6c691b"></a>

# Aesthetics


<a id="org31877f3"></a>

## Theme

I still haven't been able to figure out when to use `use-package` or `load-theme` in this context.

    (use-package material-theme
      :ensure t
      :config (load-theme 'material t))


<a id="org3fd30e4"></a>

## Font

Absolutely always [Hack](https://sourcefoundry.org/hack/).

    (custom-set-faces
     '(default ((t (:height 160 :family "Hack")))))


<a id="org45f3737"></a>

## Rainbow delimiters

    (use-package rainbow-delimiters
      :ensure t
      :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


<a id="orgb937552"></a>

## Ruler at 80 characters

`display-fill-column-indicator-mode` was introduced with Emacs 27, so the version ought to be checked before adding the hook.

    (if (version< "27.0" emacs-version)
        (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))


<a id="org9cd3106"></a>

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

    (global-linum-mode t)
    (setq linum-format "%4d\u2502")


<a id="orge88aceb"></a>

# Org Mode


<a id="org6b0eba7"></a>

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


<a id="org1fec9e4"></a>

# Python IDE packages


<a id="org7fac7fd"></a>

## [Elpy](https://github.com/jorgenschaefer/elpy)

Emacs Python IDE, which I'm pretty sure I don't use it to its full extent.

    (use-package elpy
      :ensure t
      :init (elpy-enable))


<a id="org5ca29d9"></a>

## [py-autopep8](https://github.com/paetzke/py-autopep8.el)

Code formatting according to [PEP 8](https://www.python.org/dev/peps/pep-0008/) upon save.

    (use-package py-autopep8
      :ensure t
      :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


<a id="org2efb684"></a>

## [Blacken](https://github.com/pythonic-emacs/blacken)

Code formatting according by [black](https://github.com/psf/black).

    (use-package blacken
      :ensure t
      :init 'blacken-mode)


<a id="org05b55c7"></a>

## [Flycheck](https://www.flycheck.org/en/latest/)

Flycheck is not exclusive to Python, but it is set up only for it since I mainly develop in Python.

    (use-package flycheck
      :ensure t
      :init
      (when (require 'flycheck nil t)
        (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
        (add-hook 'elpy-mode-hook 'flycheck-mode)))


<a id="org466b3bb"></a>

# Various IDE packages


<a id="org6fcb4da"></a>

## [yaml-mode](https://github.com/yoshiki/yaml-mode)

    (use-package yaml-mode :ensure t)


<a id="org708ac57"></a>

## [Clojure mode](https://github.com/clojure-emacs/clojure-mode/)

    (use-package clojure-mode :ensure t)


<a id="org83972de"></a>

## [emacs-neotree](https://github.com/jaypei/emacs-neotree)

    (use-package neotree
      :ensure t
      :init
      (global-set-key [f8] 'neotree-toggle)
      (setq-default neo-show-hidden-files t))


<a id="orgcb4e5b5"></a>

## Terminal emulator

Binds a specific configuration of `ansi-term` to "C-c b". I don't much fancy how the windows are splitted. [emacs-libvterm](https://github.com/akermu/emacs-libvterm) might be a worthy replacement.

    (defun ml/bash ()
      "Start a terminal emulator in a new window"
      (interactive)
      (split-window-sensibly)
      (other-window 1)
      (ansi-term (executable-find "bash")))
    (global-set-key (kbd "C-c b") #'ml/bash)


<a id="orga031c5e"></a>

## [Magit](https://magit.vc/)

    (use-package magit :ensure t)


<a id="orgdddf662"></a>

# Miscellaneous


<a id="orge97e81a"></a>

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


<a id="orgbc9a35c"></a>

## [toc-org](https://github.com/snosov1/toc-org)


### TODO: use it so that the table of contents for the Github README is generated automatically upon save.

