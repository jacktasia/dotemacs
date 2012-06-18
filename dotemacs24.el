;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ABOUT dotemacs24
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A simple commented emacs24 config. Meant as a starting off point
; for those I can convince to try emacs.
;
; In an effort to keep this small and portable packages
; will be auto-installed if they're missing.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALLATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1) put the following in your .emacs (or .emacs.d/init.el for windows)
;
;		 (load "~/path/to/dotemacs24.el")
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

(defconst *start-time* (current-time)) ;; record start time to time .emacs load time

(defvar emacsdir (file-name-directory load-file-name))
(add-to-list 'load-path emacsdir)

;; user-emacs-directory
(message "%s is the value of emacsdir" emacsdir)

(require 'cl) ;; gotta have it
(require 'jack-util)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *refresh-package-list* t) ;; by default refresh package list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) LOGICAL DEFAULTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias 'yes-or-no-p 'y-or-n-p) ;; don't require full "yes" for confirms
(tool-bar-mode -1) 				  ;; get rid of tool bar
(setq inhibit-startup-message t)  ;; git rid of startup page
(menu-bar-mode 0) 				  ;; no menu bar
(setq resize-mini-windows t) 	  ;; let mini buffer resize
(setq make-backup-files nil) 	  ;; no backup files

(global-set-key (kbd "\C-c r") 'replace-string) 		;; search & replace (file or region)
(global-set-key (kbd "\C-c m") 'count-matches) 			;; count instaces of prompted string
(global-set-key (kbd "\C-c l") 'goto-line) 				;; goes to prompted line number
(global-set-key (kbd "\C-c i") 'ispell-region) 			;; spell check region
(global-set-key "\C-cp" 'beginning-of-buffer)  			;; top of file
(global-set-key "\C-cn" 'end-of-buffer)

;;
;; make good use of arrow keys
;;
(global-set-key (kbd "\C-c <up>") 'text-scale-increase) 	;; text size up
(global-set-key (kbd "\C-c <down>") 'text-scale-decrease)	;; text size down

(global-set-key  [C-up] 'enlarge-window) 					;; make window taller
(global-set-key  [C-down] 'shrink-window) 					;; make window shorter
(global-set-key  [C-right] 'enlarge-window-horizontally) 	;; make window wider
(global-set-key  [C-left] 'shrink-window-horizontally)      ;; make window less wide

;;
;; tab stuff
;;
(setq indent-tabs-mode t) 							;; Turn on tabs
(setq-default indent-tabs-mode t)
(global-set-key (kbd "TAB") 'self-insert-command) 	;; Bind the TAB key
(setq default-tab-width 4) 							;; Set the tab width
(setq tab-width 4)									;; display tabs as 4 chars
(setq c-basic-indent 4)								;; c tabs length

;;
;; custom functions
;;
(global-set-key (kbd "<backtab>") 'jack-unindent-block)
(global-set-key (kbd "\C-c <tab>") 'jack-indent-block)           ;; indent selected region
(global-set-key "\C-u" 'jack-backward-kill-line)
(global-set-key  "\C-c4" 'jack-php-key-to-fetch)
(global-set-key (kbd "\C-c b") 'jack-git-blame-line)
(global-set-key [f8] 'jack-magic-lint)
(global-set-key [f11] 'jack-toggle-fullscreen)
(global-set-key [f12] 'jack-toggle-alpha)

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
;; Maximize emacs window - auto-install
;;
(when (not (require 'maxframe nil t))
         (package-install 'maxframe))

(add-hook 'window-setup-hook 'maximize-frame t)

;;
;; THEME - auto-install
;;
(unless (member 'zenburn (custom-available-themes))
  (package-install 'zenburn-theme))

(load-theme 'zenburn t)

;;
;; auto-complete (within code buffers) -- auto-install
;;
(when (not (require 'auto-complete-config nil t))
         (package-install 'auto-complete))

(require 'auto-complete-config)
(ac-config-default)

;;
;; fuzzy-match for switching buffers - auto-install (MAYBE remove)
;;
(when (not (require 'fuzzy-match nil t))
         (package-install 'fuzzy-match))

;;
;; php-mode - auto-install
;;
(when (not (require 'php-mode nil t))
         (package-install 'php-mode))

;;
;; ido stuff - auto-install
;;
(require 'ido)
(when (not (require 'ido-better-flex nil t))
         (package-install 'ido-better-flex))
;;(setq ido-enable-flex-matching t)
(setq ido-confirm-unique-completion t)
;;(setq ido-work-directory-list '("~/" "~/public_html"))
;; (ido-case-fold nil) ;; be case sensative
(when (not (require 'ido-ubiquitous nil t))
         (package-install 'ido-ubiquitous))

(ido-mode t)
(ido-better-flex/enable)
(ido-ubiquitous-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DONE - report time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(message ".emacs loaded in %s seconds" (mapconcat 'int-to-string (rest (time-since *start-time*)) "."))

(provide 'dotemacs24)
