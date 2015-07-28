;;; package --- Summary
;;; Commentary:

; My Emacs config.  Tries to use package manager as much as possible.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSTALLATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ln -s /path/to/dotemacs24.el ~/.emacs
;
; NOTE: on your first boot it will be slow due to installing all packages
;   you will see a *Compiled Log* on your first boot
;   once everything is installed add (defvar *refresh-package-list* nil)
;   to the top of your .emacs to speed things up.

;; flycheck stuff
;; sudo npm install -g eslint
;; sudo pip install pylint

;; eslint config example
;; https://github.com/mozilla/123done/issues/94

;; pylint config example
;; pylint --generate-rcfile > ~/.pylintrc

;; sudo apt-get install ubuntu-restricted-extras ttf-mscorefonts-installer
;; sudo apt-get install xfonts-terminus
;; mac: http://files.ax86.net/terminus-ttf/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(defconst *start-time* (current-time)) ;; record start time to time .emacs load time


(defvar emacsdir
  (file-name-directory
    (replace-regexp-in-string
       (regexp-quote "\n") ""
       (shell-command-to-string
       "ls -al ~/.emacs | awk '{print $NF}'"))))

;(kill-buffer "*Shell Command Output*")
(load-file (concat (file-name-as-directory emacsdir) "jack-util.el"))

;; OLD WAY
;;(defvar emacsdir (file-name-directory load-file-name))
;;(add-to-list 'load-path emacsdir)


;; user-emacs-directory
(message "%s is the value of emacsdir" emacsdir)

(require 'cl) ;; gotta have it
(require 'ido)
(require 'midnight)
(require 'uniquify)

(unless (string-equal system-type "darwin")
  (jack-emacs-maximize)) ;; only works for linux right now (requires wmctrl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *refresh-package-list* t) ;; by default refresh package list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) LOGICAL DEFAULTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(jack-visible-bell) ;; is effectively (setq visible-bell 1) but less annoying
(setq uniquify-min-dir-content 3)
;; https://github.com/emacs-mirror/emacs/blob/0537943561a37b54467bec19d1b8afbeba8e1e58/lisp/uniquify.el#L107
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ;; or "forward"
(setq tramp-default-method "scpx")
(setq clean-buffer-list-delay-general 7)

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
(electric-indent-mode 0)           ;; no thanks
(global-hl-line-mode 0)
(global-auto-revert-mode 1)        ;; so git branch changes and checkouts update the mode line
(setq auto-revert-check-vc-info nil)
(setq confirm-kill-emacs 'y-or-n-p)

(setq cua-enable-cua-keys nil)
(cua-mode)


(global-linum-mode)
(set-face-attribute 'linum nil :height 120) ; static height
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(linum-format (quote dynamic)))

(global-set-key (kbd "C-c 1") 'linum-mode)      ;; toggle linum mode
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


;; handle s-m as if it is m-m on macs
;; since depending on the keyboard the key is the same place
(when (string-equal system-type "darwin")
  (global-set-key (kbd "s-m") 'back-to-indentation)
  ;;(global-set-key (kbd "s-v") 'cua-scroll-down)
  (global-set-key (kbd "s-w") 'kill-ring-save))

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

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq indent-tabs-mode nil)))

(eval-after-load 'emacs-lisp-mode
                 '(define-key LaTeX-mode-map [(tab)] 'forward-button))


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
(global-set-key  "\C-ct" 'jack-insert-backtick)
(global-set-key (kbd "M-k") 'jack-delete-line-no-kill)

;;; desktop-mode config
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(setq desktop-dirname "~/.emacs.d/data/desktop/")
(setq desktop-path '("~/.emacs.d/data/desktop/"))
(setq desktop-base-file-name "desktop")
(setq desktop-base-lock-name "desktop.lock")
(setq desktop-save t) ;; always
(make-directory "~/.emacs.d/data/desktop/" t) ;; ensure exists


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
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize) ;; turn on

(when *refresh-package-list*
  (package-refresh-contents))

;;
;; THEME - auto-install
;;

(jack-require-or-install 'rainbow-mode)

;(jack-load-theme 'darktooth-theme) ;; monokai-theme ample-theme zenburn-theme misterioso-theme
(jack-load-theme 'zerodark-theme) ;; monokai-theme ample-theme zenburn-theme misterioso-theme

;; force the fringe to match the current theme's bg color
(let ((cur-bg-color (face-attribute 'default :background)))
  (set-face-attribute 'fringe nil :background cur-bg-color))

(let ((my-select-color "#FF8300")) ;; this is orange -- but plain "green" is also nice
  (set-face-background 'region my-select-color) ;; make region stick out more
  (set-cursor-color my-select-color))

;; https://github.com/bbatsov/zenburn-emacs/blob/master/zenburn-theme.el#L132
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(isearch ((t (:background "yellow" :foreground "black" :weight bold))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "saddle brown")))))

(scroll-bar-mode -1)


(set-default-font "Terminus-12")
;; (when (member "Terminus (TTF)" (font-family-list))
;;   (set-face-attribute 'default nil :font "Terminus (TTF)"))


;;
;; packages to install
;;
(let ((pkgs-to-install '(company company-anaconda company-tern ace-jump-mode ace-jump-buffer fuzzy-match rainbow-delimiters php-mode go-mode web-mode multiple-cursors dash s projectile fringe-helper flycheck f ido-sort-mtime flx-ido switch-window anzu git-gutter+ git-gutter-fringe+ smex exec-path-from-shell groovy-mode ag highlight-symbol ws-butler ht smart-mode-line smart-mode-line-powerline-theme imgix fic-mode multi-term ido-vertical-mode dtrt-indent jsx-mode scss-mode helm helm-projectile)))
  ;; install the packages
  (jack-require-or-install-all pkgs-to-install))


;;
;; POST PACKAGE INSTALL
;;

(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(sml/setup)
(sml/apply-theme 'dark)
(ws-butler-global-mode t)
(highlight-symbol-nav-mode 1)
(global-set-key (kbd "M-n") 'highlight-symbol-next)
(global-set-key (kbd "M-p") 'highlight-symbol-prev)

(smex-initialize)
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)

(setq anzu-cons-mode-line-p nil)
(setcar (cdr (assq 'isearch-mode minor-mode-alist))
        '(:eval (anzu--update-mode-line)))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;;;;

;; company config
(setq company-dabbrev-downcase nil) ;; (setq company-dabbrev-downcase nil)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-anaconda)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-to-list 'company-backends 'company-tern)
(setq company-tern-property-marker "")
(setq company-tern-meta-as-single-line t)
(setq company-idle-delay 0.1)

;; ido config
(ido-mode 1)
(ido-everywhere 1)
(ido-vertical-mode 1)
(setq ido-auto-merge-work-directories-length -1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(setq ido-confirm-unique-completion t)
;; (ido-case-fold nil) ;; be case sensative

(jack-require-or-install 'ido-ubiquitous)
(ido-sort-mtime-mode 1)
;(ido-better-flex/enable)
(ido-ubiquitous-mode)

;(add-hook 'after-init-hook #'global-flycheck-mode)

;;
;;keybindings for packages
;;

;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;;old M-x.
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "C-c .") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c ,") 'mc/edit-lines)

;(define-key global-map (kbd "C-x b") 'helm-mini)
;(define-key global-map (kbd "C-x b") 'ace-jump-buffer)
(define-key global-map (kbd "C-c s") 'ace-jump-char-mode)

(global-set-key (quote [M-down]) (quote scroll-up-line)) ;; scroll by one line --
(global-set-key (quote [M-up]) (quote scroll-down-line))


(global-git-gutter+-mode t)
(global-set-key (kbd "C-x g") 'git-gutter+-mode)
(global-set-key (kbd "C-x C-g") 'git-gutter+-mode) ; because i accidentally do this half the time anyway

(define-key global-map (kbd "C-c s") 'ace-jump-char-mode)
(define-key global-map (kbd "C-s") 'isearch-forward)

;; jump to start end of tags in sgml modes (html, etc)
(define-key sgml-mode-map (kbd "C-M-n") 'sgml-skip-tag-forward)
(define-key sgml-mode-map (kbd "C-M-p") 'sgml-skip-tag-backward)

;; sadly this won't work on first install!!
;; (when (fboundp 'git-gutter-fr+-minimal)
;;   (git-gutter-fr+-minimal))


;(setq git-gutter-fr+-side 'right-fringe)
;(git-gutter+-toggle-fringe)


;; (global-git-gutter-mode t)
;; (global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
;; (setq git-gutter:update-threshold 2)
;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

;; http://stackoverflow.com/questions/2903426/display-path-of-file-in-status-bar
(setq frame-title-format  ;show directory and filename on frame top
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))


;;
;; HOOKS
;;

(add-hook 'go-mode-hook 'turn-on-fic-mode)
(add-hook 'python-mode-hook 'turn-on-fic-mode)
(add-hook 'javascript-mode-hook 'turn-on-fic-mode)
(add-hook 'java-mode-hook 'turn-on-fic-mode)
(add-hook 'emacs-lisp-mode-hook 'turn-on-fic-mode)

(add-hook 'mouse-leave-buffer-hook 'jack-stop-using-minibuffer)

(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode t)
    (setq python-indent 8)
    (setq tab-width 4)))

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'before-save-hook #'gofmt-before-save)

(if (string-equal system-type "darwin")
  (setq gofmt-command "gofmt")
  (setq gofmt-command "~/go/bin/gofmt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DONE - report time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; override
(defun display-startup-echo-area-message ()
  (message "Emacs!"))

(server-start)
(message ".emacs loaded in %s seconds" (mapconcat 'int-to-string (rest (time-since *start-time*)) "."))
