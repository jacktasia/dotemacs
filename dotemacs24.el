;;; package --- Summary
;;; Commentary:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ABOUT dotemacs24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; My emacs config. Tries to use package manager as much as possible.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALLATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1) put the following in your .emacs (or .emacs.d/init.el for windows)
;
;        (load "~/path/to/dotemacs24.el")
;
; 2) if desired, override any of the settings defined below at the TOP of your .emacs
;
; NOTE: on your first boot it will be slow due to installing all packages
;   you will see a *Compiled Log* on your first boot
;   once everything is installed add (defvar *refresh-package-list* nil)
;   to the top of your .emacs to speed things up.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(defconst *start-time* (current-time)) ;; record start time to time .emacs load time

(defvar emacsdir (file-name-directory load-file-name))
(add-to-list 'load-path emacsdir)


;; user-emacs-directory
(message "%s is the value of emacsdir" emacsdir)

(require 'cl) ;; gotta have it
(require 'jack-util)
(require 'ido)

(jack-emacs-maximize) ;; only works for linux right now (requires wmctrl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *refresh-package-list* t) ;; by default refresh package list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) LOGICAL DEFAULTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse) ;; or "forward"
(setq tramp-default-method "scpx")

(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(defalias 'yes-or-no-p 'y-or-n-p) ;; don't require full "yes" for confirms
(tool-bar-mode -1)                 ;; get rid of tool bar
(setq inhibit-startup-message t)   ;; git rid of startup page
(menu-bar-mode 0)                  ;; no menu bar
(setq resize-mini-windows t)       ;; let mini buffer resize
(setq make-backup-files nil)       ;; no backup files
(setq-default c-electric-flag nil) ;; do not get fancy with () {} ?
(setq whitespace-line-column 600)  ;; do not turn line purple if "too long"
(blink-cursor-mode 0)              ;; no blinking cursor
(setq initial-scratch-message "")  ;; no scratch message

;;(custom-set-faces '(fringe ((t (:background "black"))))) ;; change border color to match

(setq cua-enable-cua-keys nil)
(cua-mode)

(global-set-key (kbd "\C-c r") 'replace-string) ;; search & replace (file or region)
(global-set-key (kbd "\C-c m") 'count-matches)  ;; count instaces of prompted string
(global-set-key (kbd "\C-c l") 'goto-line)      ;; goes to prompted line number
(global-set-key (kbd "\C-c i") 'ispell-region)  ;; spell check region
(global-set-key "\C-cp" 'beginning-of-buffer)   ;; top of file
(global-set-key "\C-cn" 'end-of-buffer)

;;
;; make good use of arrow keys
;;
(global-set-key (kbd "\C-c <up>") 'text-scale-increase)     ;; text size up
(global-set-key (kbd "\C-c <down>") 'text-scale-decrease)   ;; text size down

(global-set-key  [C-up] 'enlarge-window)                    ;; make window taller
(global-set-key  [C-down] 'shrink-window)                   ;; make window shorter
(global-set-key  [C-right] 'enlarge-window-horizontally)    ;; make window wider
(global-set-key  [C-left] 'shrink-window-horizontally)      ;; make window less wide

;;
;; tab stuff
;;
(setq indent-tabs-mode t)                           ;; Turn on tabs
(setq-default indent-tabs-mode t)
(global-set-key (kbd "TAB") 'self-insert-command)   ;; Bind the TAB key
(setq default-tab-width 4)                          ;; Set the tab width
(setq tab-width 4)                                  ;; display tabs as 4 chars
(setq c-basic-indent 4)                             ;; c tabs length

;;
;; custom functions
;;

(add-hook 'php-mode-hook 'jack-php-setup)

(global-set-key (kbd "<backtab>") 'jack-unindent-block)
(global-set-key [C-tab] 'jack-unindent-block)
(global-set-key (kbd "\C-c <tab>") 'jack-indent-block)           ;; indent selected region
(global-set-key "\C-u" 'jack-backward-kill-line)
(global-set-key (kbd "\C-c j") 'jack-newline-and-indent-no-spaces)
(global-set-key  "\C-c4" 'jack-php-key-to-fetch)
(global-set-key (kbd "\C-c b") 'jack-git-blame-line)
(global-set-key [f8] 'jack-magic-lint)
(global-set-key [f11] 'jack-toggle-fullscreen)
(global-set-key (kbd "C-=") '(lambda()(interactive)(jack-alpha-change)))
(global-set-key (kbd "C--") '(lambda()(interactive)(jack-alpha-change t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

(global-set-key  "\C-cd" 'jack-git-diff)
(global-set-key  "\C-cw" 'whitespace-mode)
(global-set-key  "\C-ce" 'jack-mark-word)

;; In case you want to change the whitespace-space color
;; (custom-set-variables)
;; (custom-set-faces
;;  '(whitespace-space ((t (:background "#3f3f3f" :foreground "#005500")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2) External Packages -- will attempt to auto-install if it can't load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; setup page manager
;;
(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(package-initialize) ;; turn on

(when *refresh-package-list*
	(package-refresh-contents)) ;; slow, hit server for fresh package list. i think it's worth it
;;
;; THEME - auto-install
;;

(jack-require-or-install 'rainbow-mode)

(jack-load-theme 'monokai-theme) ;; zenburn-theme misterioso-theme

(set-face-background 'region "green") ;; make region stick out more
(set-cursor-color "green")
(scroll-bar-mode -1)

;; sudo apt-get install ubuntu-restricted-extras ttf-mscorefonts-installer
;; http://www.fontsquirrel.com/fonts/Droid-Sans-Mono
(set-face-attribute 'default nil :font "Droid Sans Mono-12")

;;
;; packages to install
;;

(setq pkgs-to-install '(auto-complete ace-jump-mode fuzzy-match rainbow-delimiters php-mode go-mode git-gutter web-mode ido-better-flex linum-relative multiple-cursors dash s projectile flycheck)) ;; multiple-cursors

;; install the packages
(jack-require-or-install-all pkgs-to-install)


;;
;; POST PACKAGE INSTALL
;;
(require 'auto-complete-config)
(ac-config-default)


;;(setq ido-enable-flex-matching t)
(setq ido-confirm-unique-completion t)
;;(setq ido-work-directory-list '("~/" "~/public_html"))
;; (ido-case-fold nil) ;; be case sensative

(jack-require-or-install 'ido-ubiquitous)

(ido-mode t)
(ido-better-flex/enable)
(ido-ubiquitous-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; flycheck stuff
;; sudo npm install -g eslint
;; sudo pip install pylint
;; https://github.com/mozilla/123done/issues/94


;; projectile
;(setq projectile-keymap-prefix '(kbd "C-c o"))
;(setq projectile-keymap-prefix (kbd "C-c C-p"))
;(projectile-global-mode)

;;
;;keybindings for packages
;;

;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c .") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c ,") 'mc/edit-lines)

;(define-key global-map (kbd "C-x b") 'helm-mini)
(define-key global-map (kbd "C-c s") 'ace-jump-char-mode)

(global-set-key (quote [M-down]) (quote scroll-up-line)) ;; scroll by one line --
(global-set-key (quote [M-up]) (quote scroll-down-line))

(global-git-gutter-mode t)
(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)

(setq git-gutter:update-threshold 2)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

;; http://stackoverflow.com/questions/2903426/display-path-of-file-in-status-bar
(setq frame-title-format		;show directory and filename on frame top
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;
;; HOOKS
;;
(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode t)
    (setq python-indent 8)
    (setq tab-width 4)))

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'before-save-hook #'gofmt-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DONE - report time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message ".emacs loaded in %s seconds" (mapconcat 'int-to-string (rest (time-since *start-time*)) "."))

(require 'jack-scratch)

(provide 'dotemacs24)
;;; dotemacs24.el ends here
