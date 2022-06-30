;; Enables basic package supporting
(require 'package)

;; Add several archives to list of available repositories
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives '("stable-melpa" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))

;; Initialize the package infrastructure
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(eval-when-compile (require 'use-package))

(use-package material-theme
  :ensure t
  :config (load-theme 'material t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 160 :family "Hack")))))

(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package org-bullets 
  :ensure t
  :config 
  (setq org-bullets-bullet-list '("○" "•" "·"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
)

(if (version< "27.0" emacs-version)
    (add-hook 'prog-mode-hook #'display-fill-column-indicator-mode))

(setq-default mode-line-format
  (list
    ;; The buffer name, equals file name
    '(:eval (propertize " %b" 'face 'font-lock-keyword-face 'help-echo (buffer-file-name)))

    " — "
      
    ;; Line and column
    "("
    (propertize "%02l" 'face 'font-lock-face-type)
    ","
    (propertize "%02c" 'face 'font-lock-face-type)
    ") "

    ;; Relative position, size of file
    (propertize "%p" 'face 'font-lock-constant-face)

    ;; Fill with dashes
    " — "

    ;; Major mode of buffer
    "["
    '(:eval (propertize "%m" 'face 'font-lock-string-face 'help-echo buffer-file-coding-system))
    "] "

    ;; Minor modes
    "["
    minor-mode-alist
    "]"

  ))

(menu-bar-mode -1)

(tool-bar-mode -1)

(setq inhibit-startup-message t)

(setq inhibit-splash-screen t)

(global-linum-mode t)
(setq linum-format "%4d\u2502")

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
  (global-set-key (kbd "C-c a") 'org-agenda)
)

(setq org-log-done 'time)

;; (use-package elpy
;;   :ensure t
;;   :init (elpy-enable))

(use-package py-autopep8
  :ensure t
  :config (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

(use-package blacken
  :ensure t
  :config 'blacken-mode)

(use-package flycheck
  :ensure t
  :config
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         (clojure-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)

;; optionally if you want to use debugger
(use-package dap-mode :ensure t)
;; (use-package dap-python :ensure t) ;; to load the dap adapter for your language

;; optional if you want which-key integration
;; (use-package which-key
;;   :config
;;   (which-key-mode))

(use-package markdown-mode :ensure t)

(use-package yaml-mode :ensure t)

(use-package clojure-mode :ensure t)

(use-package neotree
  :ensure t
  :init
  (global-set-key [f8] 'neotree-toggle)
  (setq-default neo-show-hidden-files t))

;; (use-package vterm :ensure t)

;; (use-package magit :ensure t)

(if (eq system-type 'windows-nt)
    (progn
      (set-clipboard-coding-system 'utf-16-le)
      (set-selection-coding-system 'utf-16-le))
  (set-selection-coding-system 'utf-8))
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(use-package org-make-toc :ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(magit vterm dap-python yaml-mode use-package rainbow-delimiters py-autopep8 org-roam org-make-toc org-bullets neotree material-theme git-commit flycheck elpy dap-mode clojure-mode blacken)))
