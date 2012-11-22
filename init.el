;; Disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Some look and feel
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

;; Turn off gui stuff I don't need
(tool-bar-mode -1)
(setq-default cursor-type 'bar) 

;; Turn off the bell
(setq ring-bell-function 'ignore)

;; Set our prefered font
(set-default-font "Source Code Pro-14")
(global-font-lock-mode 1)

;; ibuffer is a much better buffer switcher
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;; set load-path to site-lisp
(setq load-path (cons "~/.emacs.d" load-path))
(add-to-list 'load-path "~/.emacs.d/emacs-for-python/") ;; tell where to load the various files

;; Setup Melpa Package Archive
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq package-user-dir "~/.emacs.d/elpa")

(package-initialize)
;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package)) 
           (package-install package))))
 '(magit pony-mode multi-term sr-speedbar bookmark+ rainbow-delimiters))

(global-rainbow-delimiters-mode)

;; ido
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-file-extensions-order '(".py" ".html" ".emacs"))
(ido-mode 1)

;; less mode
(load "less-css-mode.el")

;; Completion
(require 'completion)

;; recent files 
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Allows us to copy a single line
(defun copy-line ()
  "Copy current line in the kill ring"
  (interactive)
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position 2))
  (message "Line copied"))
(global-set-key (kbd "C-x l") 'copy-line)

;; Uniquify, keeps buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
(global-set-key (kbd "M-[ 5 d") 'backward-word)
(global-set-key (kbd "M-[ 5 c") 'forward-word)

;; Make sure Super left and right go back and forth on the line
(global-set-key (kbd "<s-left>") 'beginning-of-line)
(global-set-key (kbd "<s-right>") 'end-of-line)

;; Turn off autofill in html mode
(add-hook 'html-mode-hook 'turn-off-auto-fill)

;; Use html mode for html not the hlper
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

;; All important tab settings
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

;; PHP Mode
(load "php-mode.el")

;; load buffer revert
(load "revbufs.el")
(global-set-key (kbd "A-r") 'revbufs)

;; Go mode
(load "go-mode.el")

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
     '(lambda ()
        (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Setup Meta for mac
(setq mac-option-modifier 'meta)

;; Setup python modes
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

;; Really good Django Mode
(require 'pony-mode)

;; Highling indentation for ruby mode
(add-hook 'ruby-mode-hook 'highlight-indentation)

;; Better git support
(require 'magit)

;; Run emacs as server
(server-start)

;; Better window switching
(defun select-next-window ()
  "Switch to the next window" 
  (interactive)
  (select-window (next-window)))

(defun select-previous-window ()
  "Switch to the previous window" 
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "s-]") 'select-next-window)
(global-set-key (kbd "s-[")  'select-previous-window)

;; Disable lock file
(setq create-lockfiles nil)

;; Store other files in temp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Add the speed bar
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)

;; Project management
(require 'projectile)

;; Bookmark+
(require 'bookmark+)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defadvice ansi-term (after advise-ansi-term-coding-system)
    (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
(ad-activate 'ansi-term)
(put 'downcase-region 'disabled nil)

;; Projects
(require 'mk-project)
(global-set-key (kbd "C-c p c") 'project-compile)
(global-set-key (kbd "C-c p l") 'project-load)
(global-set-key (kbd "C-c p a") 'project-ack)
(global-set-key (kbd "C-c p g") 'project-grep)
(global-set-key (kbd "C-c p o") 'project-multi-occur)
(global-set-key (kbd "C-c p u") 'project-unload)
(global-set-key (kbd "C-c p f") 'project-find-file) ; or project-find-file-ido
(global-set-key (kbd "C-c p i") 'project-index)
(global-set-key (kbd "C-c p s") 'project-status)
(global-set-key (kbd "C-c p h") 'project-home)
(global-set-key (kbd "C-c p d") 'project-dired)
(global-set-key (kbd "C-c p t") 'project-tags)

(load "~/.emacs_projects.el")
