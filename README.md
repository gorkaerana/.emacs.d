
# Table of Contents

1.  [Preface](#org133a5ae)
    1.  [Package infrastructure initialization](#orgdbc41be)
    2.  [Set-up `use-package`](#org45e773e)
2.  [Aesthetics](#org479f604)
    1.  [Theme](#org2e68896)
    2.  [Font](#orgb03990b)
    3.  [Rainbow delimiters](#org4bd9e08)
    4.  [Ruler at 80 characters](#orgc002489)
    5.  [Miscellaneous](#org5094247)
3.  [Org Mode](#orgf94dd04)
    1.  [Setup](#org89ff038)
4.  [Python IDE packages](#orga81fba7)
    1.  [Elpy](#orgfed0de4)
    2.  [py-autopep8](#org95d1907)
    3.  [Blacken](#orgb9c6198)
    4.  [Flycheck](#org2a9b7e5)
5.  [Various IDE packages](#org7e3fc84)
    1.  [yaml-mode](#org7f33bf8)
    2.  [Clojure mode](#orgffad379)
    3.  [emacs-neotree](#orge88d210)
    4.  [Terminal emulator](#orgb8f91ed)
    5.  [Magit](#orge279852)
6.  [Miscellaneous](#org60e3175)
    1.  [Normal copy-pasting in Windows](#org8565243)
    2.  [toc-org](#orge01904b)

:TOC<sub>2</sub><sub>gh</sub>:

Create `init.el` by tangling the contents of the code blocks: "C-c C-v t" (`org-babel-tangle`). Furthermore, it is worth exporting the current file in markdown to create a beautiful README for Github: "C-c C-e m m" (`org-md-export-to-markdown`).


<a id="org133a5ae"></a>

# Preface


<a id="orgdbc41be"></a>

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


<a id="org45e773e"></a>

## Set-up `use-package`

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)
    (eval-when-compile (require 'use-package))


<a id="org479f604"></a>

# Aesthetics


<a id="org2e68896"></a>

## Theme

I still haven't been able to figure out when to use `use-package` or `load-theme` in this context.

    (use-package material-theme
      :ensure t
      :config (load-theme 'material t))


<a id="orgb03990b"></a>

## Font

Absolutely always [Hack](https://sourcefoundry.org/hack/).

    (custom-set-faces
     '(default ((t (:height 160 :family "Hack")))))


<a id="org4bd9e08"></a>

## Rainbow delimiters

    (use-package rainbow-delimiters
      :ensure t
      :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


<a id="orgc002489"></a>

## Ruler at 80 characters

`display-fill-column-indicator-mode` was introduced with Emacs 27, so the version ought to be checked before adding the hook.

    (if (version< "27.0" emacs-version)
        (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))


<a id="org5094247"></a>

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


<a id="orgf94dd04"></a>

# Org Mode


<a id="org89ff038"></a>

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


<a id="orga81fba7"></a>

# Python IDE packages


<a id="orgfed0de4"></a>

## [Elpy](https://github.com/jorgenschaefer/elpy)

Emacs Python IDE, which I'm pretty sure I don't use it to its full extent.

    (use-package elpy
      :ensure t
      :init (elpy-enable))


<a id="org95d1907"></a>

## [py-autopep8](https://github.com/paetzke/py-autopep8.el)

Code formatting according to [PEP 8](https://www.python.org/dev/peps/pep-0008/) upon save.

    (use-package py-autopep8
      :ensure t
      :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))


<a id="orgb9c6198"></a>

## [Blacken](https://github.com/pythonic-emacs/blacken)

Code formatting according by [black](https://github.com/psf/black).

    (use-package blacken
      :ensure t
      :init 'blacken-mode)


<a id="org2a9b7e5"></a>

## [Flycheck](https://www.flycheck.org/en/latest/)

Flycheck is not exclusive to Python, but it is set up only for it since I mainly develop in Python.

    (use-package flycheck
      :ensure t
      :init
      (when (require 'flycheck nil t)
        (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
        (add-hook 'elpy-mode-hook 'flycheck-mode)))


<a id="org7e3fc84"></a>

# Various IDE packages


<a id="org7f33bf8"></a>

## [yaml-mode](https://github.com/yoshiki/yaml-mode)

    (use-package yaml-mode :ensure t)


<a id="orgffad379"></a>

## [Clojure mode](https://github.com/clojure-emacs/clojure-mode/)

    (use-package clojure-mode :ensure t)


<a id="orge88d210"></a>

## [emacs-neotree](https://github.com/jaypei/emacs-neotree)

    (use-package neotree
      :ensure t
      :init
      (global-set-key [f8] 'neotree-toggle)
      (setq-default neo-show-hidden-files t))


<a id="orgb8f91ed"></a>

## Terminal emulator

Binds a specific configuration of `ansi-term` to "C-c b". I don't much fancy how the windows are splitted. [emacs-libvterm](https://github.com/akermu/emacs-libvterm) might be a worthy replacement.

    (defun ml/bash ()
      "Start a terminal emulator in a new window"
      (interactive)
      (split-window-sensibly)
      (other-window 1)
      (ansi-term (executable-find "bash")))
    (global-set-key (kbd "C-c b") #'ml/bash)


<a id="orge279852"></a>

## [Magit](https://magit.vc/)

    (use-package magit :ensure t)


<a id="org60e3175"></a>

# Miscellaneous


<a id="org8565243"></a>

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


<a id="orge01904b"></a>

## [toc-org](https://github.com/snosov1/toc-org)

TODO: use it so that the table of contents for the Github README is generated automatically upon save.

    (use-package toc-org
      :ensure t
      :defer t
      :config
      (add-hook 'org-mode-hook 'toc-org-mode)
      ;; enable in markdown, too
      (add-hook 'markdown-mode-hook 'toc-org-mode)
      (define-key markdown-mode-map (kbd "\C-c\C-o") 'toc-org-markdown-follow-thing-at-point)
      )

