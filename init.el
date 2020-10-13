;;;;;;;;;;;;;;;;;;;;;;;
;;;; Miscellaneous ;;;;
;;;;;;;;;;;;;;;;;;;;;;;

; Disable splash-scren

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq inhibit-splash-screen t)

; Tell emacs where to where my personal elisp lib dir is
(add-to-list 'load-path "~/.emacs.d/lisp/")

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
(setq org-tags-column -95)

; Set agenda mode to read all org files
(setq org-agenda-files (list "~/../../Dropbox/emacs/work/client_projects/"))

(setq org-agenda-skip-scheduled-if-done t)

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
 '(default ((t (:height 160 :family "Hack")))))

; Change theme to zenburn
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;
;;;; Yaml mode ;;;;
;;;;;;;;;;;;;;;;;;;

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(put 'narrow-to-region 'disabled nil)
