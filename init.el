;; Disable splash-scren
(setq inhibit-splash-screen t)

;;;; Org mode configuration
(require 'org)
;; Make org-mde work with files ending in -org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; Set org todo keywords
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE" "CANCELLED")))

;; Change default font to 16 point Hack
(custom-set-faces
 '(default ((t (:height 160 :family "Hack")))))

;; Change theme to zenburn
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)
