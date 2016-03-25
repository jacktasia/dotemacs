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
;; sudo npm install -g eslint jsxhint
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

(load-file (concat (file-name-as-directory emacsdir) "jack-util.el"))

;; user-emacs-directory
;(message "%s is the value of emacsdir" emacsdir)

(require 'cl) ;; gotta have it
(require 'ido)
(require 'midnight)
(require 'uniquify)
(require 'org)

(unless (string-equal system-type "darwin")
  (jack-emacs-maximize)) ;; only works for linux right now (requires wmctrl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *refresh-package-list* t) ;; by default refresh package list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) LOGICAL DEFAULTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 20000000)
(jack-visible-bell) ;; is effectively (setq visible-bell 1) but less annoying
(setq uniquify-min-dir-content 3)
;; https://github.com/emacs-mirror/emacs/blob/0537943561a37b54467bec19d1b8afbeba8e1e58/lisp/uniquify.el#L107
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ;; or "forward"
(setq tramp-default-method "scpx")
(setq clean-buffer-list-delay-general 7)

(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(setq org-log-done t)              ;; show done time when marking a todo done
(defalias 'yes-or-no-p 'y-or-n-p)  ;; don't require full "yes" for confirms
(tool-bar-mode -1)                 ;; get rid of tool bar
(setq inhibit-startup-message t)   ;; git rid of startup page
(menu-bar-mode 0)                  ;; no menu bar
(setq resize-mini-windows t)       ;; let mini buffer resize
(setq make-backup-files nil)       ;; no backup files
(setq-default c-electric-flag nil) ;; do not get fancy with () {} ?
(setq whitespace-line-column 60000)  ;; do not turn line purple if "too long"
(blink-cursor-mode 0)              ;; no blinking cursor
(setq initial-scratch-message "")  ;; no scratch message
(electric-indent-mode 0)           ;; no thanks
(global-hl-line-mode 0)
(global-auto-revert-mode 1)        ;; so git branch changes and checkouts update the mode line
(setq auto-revert-check-vc-info t)
(setq confirm-kill-emacs 'y-or-n-p)
(setq message-log-max t)           ;; If t, log messages but don't truncate the buffer when it becomes large.
(setq-default cursor-in-non-selected-windows nil)
(setq column-number-mode t)

(setq cua-enable-cua-keys nil)
(cua-mode)
(if window-system
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(global-linum-mode)
(set-face-attribute 'linum nil :height 120) ; static height
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Vhl/highlight-zero-width-ranges t)
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-replace-to-string-separator " => ")
 '(anzu-search-threshold 1000)
 '(custom-safe-themes
   (quote
    ("21c149e080d562fe9169c8abda51c2f1f9b0a12c89cc2c7a4d9998a758e1cfbd" "d1dbb3c37e11ae8f986ca2d4b6a9d78bb1915fe66f3a6ffab1397cc746c18cba" "419637b7a8c9cb43f273980f0c9879c0cbadace6b38efac0281e031772c84eb2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "c5a044ba03d43a725bd79700087dea813abcb6beb6be08c7eb3303ed90782482" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(linum-format (quote dynamic))
 '(volatile-highlights-mode t))

(global-set-key (kbd "C-c 1") 'linum-mode)      ;; toggle linum mode
(global-set-key (kbd "C-c r") 'replace-string) ;; search & replace (file or region)
(global-set-key (kbd "C-c m") 'count-matches)  ;; count instaces of prompted string
(global-set-key (kbd "C-c l") 'goto-line)      ;; goes to prompted line number
(global-set-key (kbd "C-c i") 'ispell-region)  ;; spell check region
(global-set-key (kbd "C-c p") 'beginning-of-buffer)   ;; top of file
(global-set-key (kbd "C-c n") 'end-of-buffer)
(global-set-key (kbd "C-x i") 'infer-indentation-style)   ;; infer spaces/tabs

;;
;; make good use of arrow keys
;;

(global-set-key (kbd "C-<up>") 'enlarge-window)                    ;; make window taller
(global-set-key (kbd "C-<down>") 'shrink-window)                   ;; make window shorter
(global-set-key (kbd "C-<right>") 'enlarge-window-horizontally)    ;; make window wider
(global-set-key (kbd "C-<left>") 'shrink-window-horizontally)      ;; make window less wide


;; handle s-m as if it is m-m on macs
;; since depending on the keyboard the key is the same place
(when (string-equal system-type "darwin")
  (global-set-key (kbd "s-m") 'back-to-indentation)
  ;;(global-set-key (kbd "s-v") 'cua-scroll-down)
  (global-set-key (kbd "s-w") 'kill-ring-save))

;;
;; tab stuff
;;

;; (setq indent-tabs-mode t)                           ;; Turn on tabs
;; (setq-default indent-tabs-mode t)
;; (global-set-key (kbd "TAB") 'self-insert-command)   ;; Bind the TAB key

(setq-default indent-tabs-mode nil)
(setq default-tab-width 2)                          ;; Set the tab width
(setq tab-width 2)                                  ;; display tabs as 2 chars
(setq c-basic-indent 2)                             ;; c tabs length
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)

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
(global-set-key (kbd "C-<tab>") 'jack-unindent-block)
(global-set-key (kbd "C-c <tab>") 'jack-indent-block)           ;; indent selected region
(global-set-key (kbd "M-u") 'jack-backward-kill-line)
(global-set-key (kbd "C-c j") 'jack-newline-and-indent-no-spaces)
(global-set-key (kbd "C-c b") 'jack-git-blame-line)
;(global-set-key [f11] 'jack-toggle-fullscreen)
(global-set-key (kbd "C-=") '(lambda()(interactive)(jack-alpha-change)))
(global-set-key (kbd "C--") '(lambda()(interactive)(jack-alpha-change t)))
(global-set-key (kbd "C-0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

(global-set-key (kbd "C-c 5") 'my/eval-buffer)
(global-set-key (kbd "C-c 6") 'emacs-uptime)

(global-set-key (kbd "C-c d")'jack-git-diff)
(global-set-key (kbd "C-c w")'whitespace-mode)
(global-set-key (kbd "C-c e")'er/mark-symbol)
(global-set-key (kbd "M-k") 'jack-delete-line-no-kill)


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

(jack-load-theme 'dracula-theme) ;; material-theme darktooth-theme monokai-theme ample-theme zenburn-theme misterioso-theme

;; force the fringe to match the current theme's bg color
(let ((cur-bg-color (face-attribute 'default :background)))
  (set-face-attribute 'fringe nil :background cur-bg-color))



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
 '(rainbow-delimiters-depth-9-face ((t (:foreground "saddle brown"))))
 '(vhl/default-face ((t (:inherit default :background "yellow2")))))

(scroll-bar-mode -1)

(when (string-equal system-type "darwin")
  (set-default-font "Menlo-14"))

;; (when (member "Terminus (TTF)" (font-family-list))
;;   (set-face-attribute 'default nil :font "Terminus (TTF)"))

;;
;; packages to install
;;
;; package-install (since i always end up searching for this)
(let ((pkgs-to-install '(company company-anaconda company-tern ace-jump-mode ace-jump-buffer fuzzy-match rainbow-delimiters
                         php-mode go-mode multiple-cursors dash s projectile fringe-helper flycheck f ido-sort-mtime flx-ido
                         switch-window anzu git-gutter+ git-gutter-fringe+ smex exec-path-from-shell groovy-mode ag
                         highlight-symbol ws-butler ht smart-mode-line smart-mode-line-powerline-theme imgix fic-mode
                         multi-term ido-vertical-mode dtrt-indent js2-mode scss-mode helm helm-projectile flyspell-lazy request
                         nyan-mode avy emmet-mode default-text-scale expand-region use-package smartscan yaml-mode dumb-jump
                         clojure-mode smooth-scrolling beacon hlinum google-this crux key-chord)))
  ;; install the packages
  (jack-require-or-install-all pkgs-to-install))

;;
;; POST PACKAGE INSTALL
;;

;; fill-column-indicator
;; turn off fci for now. is buggy with company mode
;; (setq fci-rule-color "#6272a4") ; dracula theme comment color
;; (setq fci-dash-pattern 0.5)
;; (setq fci-rule-use-dashes t)
;; (setq fci-rule-column 100)

;; (define-globalized-minor-mode jack-global-fci-mode fci-mode turn-on-fci-mode)
;; (jack-global-fci-mode 1)

(key-chord-mode 1)
(key-chord-define-global "qw" 'undo)
(key-chord-define-global "fj" 'ace-jump-char-mode)


(hlinum-activate)

(beacon-mode 1)
(setq beacon-blink-duration 0.6)
(setq beacon-blink-delay 0.3)
(setq beacon-size 15)
(setq beacon-color "green")


(dumb-jump-mode)
(global-smartscan-mode 1)
;; http://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
(bind-keys*
     ("C-c w" . whitespace-mode))

(global-set-key (kbd "C-\'") 'er/expand-region)

(global-set-key (kbd "C-c g") 'google-this-noconfirm)

;; (global-set-key (kbd "\C-c <up>") 'text-scale-increase)     ;; text size up
;; (global-set-key (kbd "\C-c <down>") 'text-scale-decrease)   ;; text size down
;; using a plugin across all buffers with default-text-scale
(global-set-key (kbd "C-c <up>") 'default-text-scale-increase)     ;; text size up
(global-set-key (kbd "C-c <down>") 'default-text-scale-decrease)   ;; text size down

(global-set-key (kbd "C-c t") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c 3") 'crux-transpose-windows)


(setq avy-all-windows nil)
(setq avy-keys (number-sequence ?a ?z))

(nyan-mode)
;(projectile-global-mode)
(projectile-mode)
(flyspell-lazy-mode 1)
(flyspell-mode 1)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(sml/setup)
(sml/apply-theme 'dark)
(ws-butler-global-mode t)
(highlight-symbol-nav-mode 1)

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

;;emmet mode config

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)

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
(global-set-key (kbd "C-c \$") 'jack-save-word)
(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "C-x v") 'projectile-switch-to-buffer)
(global-set-key (kbd "C-x b") 'ido-switch-buffer)
(global-set-key (kbd "C-x p") 'projectile-switch-project)
(global-set-key (kbd "C-x f") 'projectile-grep)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;;old M-x.
(global-set-key (kbd "C-x o") 'switch-window)
(global-set-key (kbd "M-o") 'switch-window)
(global-set-key (kbd "C-c .") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c ,") 'mc/edit-lines)

(setq ace-jump-mode-scope 'window)
(setq ace-jump-mode-move-keys
      (loop for i from ?a to ?z collect i))
;; (define-key global-map (kbd "C-c s") 'ace-jump-char-mode)


(global-set-key (kbd "s-s") 'avy-goto-subword-0)
(define-key global-map (kbd "C-c s") 'avy-goto-subword-1)
(define-key global-map (kbd "C-z") 'avy-goto-subword-1)


(define-key flyspell-mode-map (kbd "C-c \$") 'jack-save-word)

(global-set-key (kbd "M-<down>") (quote scroll-up-line)) ;; scroll by one line --
(global-set-key (kbd "M-<up>") (quote scroll-down-line))
(global-set-key (kbd "M-n") '(lambda()(interactive)(scroll-up-line)(next-line)))
(global-set-key (kbd "M-p") '(lambda()(interactive)(scroll-down-line)(previous-line)))
;; (global-set-key (kbd "M-n") 'highlight-symbol-next)
;; (global-set-key (kbd "M-p") 'highlight-symbol-prev)


(global-git-gutter+-mode t)
(global-set-key (kbd "C-x g") 'git-gutter+-mode)
(global-set-key (kbd "C-x C-g") 'git-gutter+-mode) ; because i accidentally do this half the time anyway

(define-key global-map (kbd "C-c s") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c a") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-s") 'isearch-forward)
(bind-keys* ("<C-return>" . set-rectangular-region-anchor))


;; jump to start end of tags in sgml modes (html, etc)

(add-hook 'sgml-mode-hook
  (lambda ()
   (define-key sgml-mode-map (kbd "C-M-n") 'sgml-skip-tag-forward)
   (define-key sgml-mode-map (kbd "C-M-p") 'sgml-skip-tag-backward)))

;; http://stackoverflow.com/questions/2903426/display-path-of-file-in-status-bar
(setq frame-title-format  ;show directory and filename on frame top
      (list (format "%s %%S: %%j " (system-name))
        '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;;
;; HOOKS
;;
(add-hook 'after-change-major-mode-hook 'infer-indentation-style)
(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'scss-mode-hook 'xah-syntax-color-hex)

;; from: https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking/
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (js2-mode))
(add-hook 'js2-mode-hook (lambda ()
                          (flycheck-select-checker 'jsxhint-checker)
                          (flycheck-mode)))

(add-hook 'go-mode-hook 'fic-mode)
(add-hook 'python-mode-hook 'fic-mode)
(add-hook 'javascript-mode-hook 'fic-mode)
(add-hook 'web-mode-hook 'fic-mode)
(add-hook 'java-mode-hook 'fic-mode)
(add-hook 'emacs-lisp-mode-hook 'fic-mode)
(add-hook 'web-mode-hook 'fic-mode)
(add-hook 'js2-mode-hook 'fic-mode)

(add-hook 'javascript-mode-hook 'flycheck-mode)

(add-hook 'mouse-leave-buffer-hook 'jack-stop-using-minibuffer)

(add-hook 'python-mode-hook
  (lambda ()
    (setq indent-tabs-mode t)
    (setq python-indent 8)
    (setq tab-width 4)))

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'before-save-hook #'gofmt-before-save)

(add-hook 'kill-emacs-hook 'pmdm-write-opened-files)

(if (string-equal system-type "darwin")
  (setq gofmt-command "gofmt")
  (setq gofmt-command "~/go/bin/gofmt"))

;; cursor tweaks
(let ((my-select-color "#83F52C")) ;; this is a neon green, FF8300 is orange -- but plain "green" is also nice
  ;(set-face-background 'region my-select-color) ;; make region stick out more
  (set-cursor-color my-select-color))

(setq-default cursor-type '(bar . 2))
;; always use a random beacon color
(setq jacks-colors (--reject (s-contains? "gray" it) (defined-colors)))
(add-function :before (symbol-function 'beacon-blink-automated)
              (lambda ()
                (setq beacon-color (nth (random (length jacks-colors)) jacks-colors))
                (setq beacon-size (+ 15 (random 60)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DONE - report time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; override
(defun display-startup-echo-area-message ()
  (message "Emacs!"))

(server-start)
(pmdm-load-files)
(message ".emacs loaded in %s seconds" (mapconcat 'int-to-string (rest (time-since *start-time*)) "."))
(put 'erase-buffer 'disabled nil)
