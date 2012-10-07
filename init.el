;; set load-path to site-lisp
(setq load-path (cons "~/.emacs.d" load-path))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize)

(set-default-font "Source Code Pro-14")
(global-font-lock-mode 1)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(require 'color-theme)

;; ido
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".py" ".html" ".emacs"))
(ido-mode 1)


;; less mode

(load "less-css-mode.el")

;; ========== completion ==========

(require 'completion)

;; ;; javascript
;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


(setq package-user-dir "~/.emacs.d/elpa")

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

(defun copy-line ()
  "Copy current line in the kill ring"
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position 2))
  (message "Line copied"))

(global-set-key (kbd "C-x l") 'copy-line)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)

(global-set-key (kbd "<s-left>") 'beginning-of-line)
(global-set-key (kbd "<s-right>") 'end-of-line)

(add-hook 'html-mode-hook 'turn-off-auto-fill)

(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

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

;; (setq custom-file (concat dotfiles-dir "custom/" system-name ".el"))
;; (setq per-machine-config-file (concat dotfiles-dir "config/per-machine/" system-name ".el"))

;; (load custom-file 'noerror)

;; (if (file-exists-p per-machine-config-file)
;;     (load per-machine-config-file))

;; load buffer revert
(load "revbufs.el")

(global-set-key (kbd "A-r") 'revbufs)


(setq default-frame-alist
      '((cursor-type . box)
        (vertical-scroll-bars . right)
        (internal-border-width . 0)
        (modeline . t)
        (fringe)
        (mouse-color . "black")
        (cursor-color . "Red")
        (background-mode . light)
        (tool-bar-lines . 1)
        (menu-bar-lines . 1)
        (right-fringe . 12)
        (left-fringe . 4)
        (background-color . "#fcf4dc")
        (foreground-color . "Black")
        (font-backend ns)
        )
      )

(add-to-list 'load-path "~/.emacs.d/src/emacs-color-theme-solarized/")
(add-to-list 'custom-theme-load-path
             "~/.emacs.d/src/emacs-color-theme-solarized")

;(load-theme 'solarized-light t)

;; (if (window-system) nil
;;   (load-theme 'solarized-dark t)
;;   )

;; Go mode
(load "go-mode.el")

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
     '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(setq mac-option-modifier 'meta)


(add-to-list 'load-path "~/.emacs.d/emacs-for-python/") ;; tell where to load the various files

(load-file "~/.emacs.d/emacs-for-python/epy-init.el") (require 'epy-setup)      ;; It will setup other loads, it is required!
(require 'epy-python)     ;; If you want the python facilities [optional]
(require 'epy-completion) ;; If you want the autocompletion settings [optional]
(require 'epy-editing)    ;; For configurations related to editing [optional]
(require 'epy-bindings)   ;; For my suggested keybindings [optional]
(require 'epy-nose)       ;; For nose integration
(require 'highlight-indentation)
(epy-django-snippets)

(add-hook 'python-mode-hook 'highlight-indentation)
(add-hook 'python-mode-hook
          (lambda () (define-key python-mode-map "\C-m" 'newline-and-indent))) ;;Newline and enter

(add-hook 'ruby-mode-hook 'highlight-indentation)



(add-to-list 'load-path "~/.emacs.d/magit/") ;; tell where to load the various files
(require 'magit)

(tool-bar-mode -1)
(setq-default cursor-type 'bar) 

(setq ring-bell-function 'ignore)

(server-start)

(add-to-list 'load-path "~/.emacs.d/pony-mode/")
(require 'pony-mode)
