;;
;; MELPA Package Support
;;

;; Enables basic package supporting
(require 'package)

;; Initialize the package infrastructure
(package-initialize)

;; Add Melpa archive to list of available repositories
(setq package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("stable-melpa" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ; ("marmalade" . "https://marmalade-repo.org/packages/")
                        ))

;;
;; Ensure package usage with use-packages
;; 

;; make sure use-package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(eval-when-compile (require 'use-package))

;; Theme
(use-package zenburn-theme
  :ensure t)

;; Emacs Lisp Python environment
(use-package elpy
  :ensure t
  :init
  (elpy-enable)
  )

;; pep8 formatting on save
(use-package py-autopep8
  :ensure t
  :init
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save))

;; black formatting on save
(use-package blacken
  :ensure t)

;; On the fly syntax check
(use-package flycheck
  :ensure t
  :init
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  )

;; Yaml mode
(use-package yaml-mode
  :ensure t)

;; Neotree
(use-package neotree
  :ensure t
  :init
  (global-set-key [f8] 'neotree-toggle)
  )

;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
	                  "jupyter")


;;
;; Basic customization
;;

(setq inhibit-startup-message t) ; Hide startup message
(global-linum-mode t)            ; Enable line numbers globally
(setq inhibit-splash-screen t)   ; Hide splash screen
(setq linum-format "%4d\u2502")  ; Add solid vertical line after line numbering

; Tell emacs where to where my personal elisp lib dir is
;(add-to-list 'load-path "~/.emacs.d/lisp/")

;;;;;;;;;;;;;;;;;;
;;;; Org mode ;;;;
;;;;;;;;;;;;;;;;;;

(require 'org)

; Make org-mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

; Set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE" "CANCELLED"))
      org-todo-keyword-faces
      '(("TODO" . "red")
      ("IN-PROGRESS" . "orange")
      ("WAITING" . "yellow")
      ("DONE" . "black")
      ("CANCELLED" . "black")))

; Add link shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)
(global-set-key (kbd "C-c a") 'org-agenda)

; Set tag location
;(setq org-tags-column -95)

; Set agenda mode to read all org files
;(setq org-agenda-files (list "~/../../Dropbox/emacs/work/client_projects/"))

;(setq org-agenda-skip-scheduled-if-done t)

; Agenda mode splits vertically
;(defadvice org-agenda (around split-vertically activate)
;  (let ((split-width-threshold 50))  ; or whatever width makes sense for you
;    ad-do-it))

;;;;;;;;;;;;;;;;;;;;
;;;; Appearance ;;;;
;;;;;;;;;;;;;;;;;;;;

; Change default font to 16 point Consolas
;(set-face-font 'default "Hack")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 160 :family "Hack")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode flycheck blacken py-autopep8 elpy zenburn-theme use-package))))
