;; Disable splash-scren
(setq inhibit-splash-screen t)

;; Tell emacs where to where my personal elisp lib dir is
(add-to-list 'load-path "~/.emacs.d/lisp/")

;;;; Org mode configuration
(require 'org)
;; Make org-mde work with files ending in -org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; Set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "+DONE+" "+CANCELLED+"))
      org-todo-keyword-faces
      '(("TODO" . "red")
      ("IN-PROGRESS" . "orange")
      ("WAITING" . "yellow")
      ("DONE" . "green")
      ("CANCELLED" . "blue")))
;; Add link shortcuts
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C-l") 'org-insert-link)

;; Change default font to 16 point Consolas
(custom-set-faces
 '(default ((t (:height 160 :family "Hack")))))

;; Change theme to zenburn
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;; Yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
