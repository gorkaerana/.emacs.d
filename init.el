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

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(eval-when-compile (require 'use-package))

(use-package material-theme
  :ensure t
  :config (load-theme 'material t))

(custom-set-faces
 '(default ((t (:height 160 :family "Hack")))))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(if (version< "27.0" emacs-version)
    (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))

(setq menu-bar-mode -1)

(setq tool-bar-mode -1)

(setq inhibit-startup-message t)

(setq inhibit-splash-screen t)



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

(use-package elpy
  :ensure t
  :init (elpy-enable))

(use-package py-autopep8
  :ensure t
  :init (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package blacken
  :ensure t
  :init 'blacken-mode)

(use-package flycheck
  :ensure t
  :init
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package yaml-mode :ensure t)

(use-package clojure-mode :ensure t)

(use-package neotree
  :ensure t
  :init
  (global-set-key [f8] 'neotree-toggle)
  (setq-default neo-show-hidden-files t))

(defun ml/bash ()
  "Start a terminal emulator in a new window"
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (ansi-term (executable-find "bash")))
(global-set-key (kbd "C-c b") #'ml/bash)

(use-package magit :ensure t)

(if (eq system-type 'windows-nt)
    (progn
      (set-clipboard-coding-system 'utf-16-le)
      (set-selection-coding-system 'utf-16-le))
  (set-selection-coding-system 'utf-8))
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq markdown-fontify-code-blocks-natively t)
