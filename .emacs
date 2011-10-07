;; set load-path to site-lisp
(setq load-path (cons "~/.emacs.d" load-path))

(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(set-default-font "DejaVu Sans Mono-10")
(global-font-lock-mode 1)

(require 'color-theme)
(require 'color-theme-zenburn)
(color-theme-zenburn)

;; ========== completion ==========

(require 'completion)

;; ;; javascript
;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; recent files 
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


(setq make-backup-files nil) 

;;disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;disable auto save
(setq auto-save-default nil)
(setq load-path (append load-path (list "/home/cezar/.emacs.d")))

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(defun copy-line ()
  "Copy current line in the kill ring"
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position 2))
  (message "Line copied"))

(global-set-key (kbd "C-x l") 'copy-line)

;; python settings
(add-hook 'python-mode-hook 
      (lambda ()
	(local-set-key [(control ?<)] 'python-shift-left)
	(local-set-key [(control ?>)] 'python-shift-right)))


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)

(add-hook 'html-mode-hook 'turn-off-auto-fill)

(setq tab-width 4)
(setq indent-tabs-mode nil)

;;; for Aquamacs Emacs, they set some keys in
;;; the osx-key-mode-map, and make it a more
;;; default keymap than global-map.
(if (boundp 'osx-key-mode-map)
(setq hah-key-map osx-key-mode-map)
(setq hah-key-map global-map))(define-key hah-key-map [home] 'beginning-of-line)
(define-key hah-key-map [end] 'end-of-line)
(define-key hah-key-map [C-home] 'beginning-of-buffer)
(define-key hah-key-map [C-end] 'end-of-buffer)

(load "php-mode.el")

(if (load "mwheel" t)
    (mwheel-install))

;; turn on mouse wheel scrolling
  (defun sd-mousewheel-scroll-up (event)
    "Scroll window under mouse up by five lines."
    (interactive "e")
    (let ((current-window (selected-window)))
      (unwind-protect
          (progn 
            (select-window (posn-window (event-start event)))
            (scroll-up 2))
        (select-window current-window))))
  (defun sd-mousewheel-scroll-down (event)
    "Scroll window under mouse down by five lines."
    (interactive "e")
    (let ((current-window (selected-window)))
      (unwind-protect
          (progn 
            (select-window (posn-window (event-start event)))
            (scroll-down 2))
        (select-window current-window))))
 
  (global-set-key (kbd "<mouse-5>") 'sd-mousewheel-scroll-up)
  (global-set-key (kbd "<mouse-4>") 'sd-mousewheel-scroll-down)

(xterm-mouse-mode)

(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(setq custom-file (concat dotfiles-dir "custom/" system-name ".el"))
(setq per-machine-config-file (concat dotfiles-dir "config/per-machine/" system-name ".el"))

(load custom-file 'noerror)

(if (file-exists-p per-machine-config-file)
    (load per-machine-config-file))