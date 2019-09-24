
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

;; http://codewinds.com/blog/2015-04-02-emacs-flycheck-eslint-jsx.html
;; npm install -g eslint babel-eslint eslint-plugin-react eslint-plugin-import eslint-config-airbnb eslint-plugin-jsx-a11y

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; START UP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:
;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq gnutls-algorithm-priority nil)
(defconst *start-time* (current-time)) ;; record start time to time .emacs load time

(defvar emacsdir
  (file-name-directory
    (replace-regexp-in-string
       (regexp-quote "\n") ""
       (shell-command-to-string
       "ls -al ~/.emacs | awk '{print $NF}'"))))

(load-file (concat (file-name-as-directory emacsdir) "jack-util.el"))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(chess-default-display (quote (chess-ics1 chess-plain)))
;;  '(custom-safe-themes
;;    (quote
;;     ("2296db63b1de14e65390d0ded8e2b5df4b9e4186f3251af56807026542a58201" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "10e231624707d46f7b2059cc9280c332f7c7a530ebc17dba7e506df34c5332c4" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
;;  '(debug-on-error t)
;;  '(global-fci-mode nil)
;;  '(package-selected-packages
;;    (quote
;;  '(paradox-github-token t))


;; user-emacs-directory
;(message "%s is the value of emacsdir" emacsdir)

(require 'cl) ;; gotta have it
(require 'ido)
(require 'midnight)
(require 'uniquify)
(require 'org)
(require 'color)

;; (unless (string-equal system-type "darwin")
;;   (jack-emacs-maximize)) ;; only works for linux right now (requires wmctrl)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *refresh-package-list* nil) ;; by default refresh package list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1) LOGICAL DEFAULTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 8000000)
;(setq gc-cons-threshold 20000000)
;(jack-visible-bell) ;; is effectively (setq visible-bell 1) but less annoying
(setq visible-bell nil)
(setq ring-bell-function 'ignore)
(setq uniquify-min-dir-content 0) ;; 3
;; https://github.com/emacs-mirror/emacs/blob/0537943561a37b54467bec19d1b8afbeba8e1e58/lisp/uniquify.el#L107
(setq uniquify-buffer-name-style nil) ;; 'post-forward-angle-brackets or "forward"
(setq tramp-default-method "scpx")
(setq clean-buffer-list-delay-general 7)
(show-paren-mode t)
(add-to-list 'auto-mode-alist '("\\.el\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.sh\\'" . shell-script-mode))
(toggle-truncate-lines -1)         ;; do not truncate lines in org mode
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
(global-hl-line-mode 1)
(global-auto-revert-mode 1)        ;; so git branch changes and checkouts update the mode line
(setq auto-revert-check-vc-info nil)
(setq confirm-kill-emacs 'y-or-n-p)
(setq message-log-max t)           ;; If t, log messages but don't truncate the buffer when it becomes large.
(setq-default cursor-in-non-selected-windows nil)
(setq-default bidi-display-reordering nil)
(setq column-number-mode t)
(global-subword-mode t)


(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq create-lockfiles nil)

(setq cua-enable-cua-keys nil)
(cua-mode)
(if window-system
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; http://stackoverflow.com/a/25438277/24998

(global-linum-mode)
(set-face-attribute 'linum nil :height 120) ; static height

;(global-set-key (kbd "C-c 1") 'linum-mode)      ;; toggle linum mode

(global-set-key (kbd "C-c x") 'jack-kill-all-buffers)
;(global-set-key (kbd "C-c 2") 'next-buffer)
;(global-set-key (kbd "C-c r") 'replace-string) ;; search & replace (file or region)
(global-set-key (kbd "C-c r") 'vr/replace) ;; search & replace (file or region)
(global-set-key (kbd "C-c q") 'vr/query-replace)
;(global-set-key (kbd "C-c m") 'count-matches)  ;; count instaces of prompted string
(global-set-key (kbd "C-c m") 'jack-match-above-alignment)
(global-set-key (kbd "C-c l") 'goto-line)      ;; goes to prompted line number
;(global-set-key (kbd "C-c i") 'ispell-region)  ;; spell check region


(global-set-key (kbd "C-x i") 'infer-indentation-style)   ;; infer spaces/tabs


;;
;; make good use of arrow keys
;;

(global-set-key (kbd "C-c <up>") 'enlarge-window)                    ;; make window taller
(global-set-key (kbd "C-c <down>") 'shrink-window)                   ;; make window shorter
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)    ;; make window wider
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)      ;; make window less wide


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

(setq-default js-indent-level 2)
(setq-default indent-tabs-mode nil)
(setq tab-width 2)                                  ;; display tabs as 2 chars
(setq default-tab-width 2)
(setq c-basic-indent 2)                             ;; c tabs length
(setq-default web-mode-code-indent-offset 2)
(setq-default web-mode-markup-indent-offset 2)
(setq web-mode-enable-auto-indentation nil)

;;
;; custom functions
;;
(global-set-key (kbd "<f10>") 'jack-run-python3-buffer)
(global-set-key (kbd "<f12>") 'jack-smart-translate)
(global-set-key (kbd "C-j") 'jack-match-above-indentation)
(add-hook 'php-mode-hook 'jack-php-setup)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (eldoc-mode t)
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
(global-set-key (kbd "C-c =") '(lambda()(interactive)(jack-alpha-change)))
(global-set-key (kbd "C-c -") '(lambda()(interactive)(jack-alpha-change t)))
(global-set-key (kbd "C-c 0") '(lambda()(interactive)
                               (modify-frame-parameters nil `((alpha . 100)))))

(global-set-key (kbd "C-c 5") 'my/eval-buffer)
(global-set-key (kbd "C-c 6") 'jack-search-all-buffers)

(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c e") 'er/mark-symbol)
;(global-set-key (kbd "C-c 4") 'er/expand-region)


(global-set-key (kbd "M-k") 'jack-delete-line-no-kill)
(global-set-key (kbd "C-c k") 'jack-kill-other-buffers)

;(global-set-key (kbd "C-_") '(lambda()(interactive)(message "%s" "Use \"C-x u\"!!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2) External Packages -- will attempt to auto-install if it can't load
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; setup page manager
;;
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; swap above with this if melpa is ever down.
;; (setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
;;                          ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize) ;; turn on

(when *refresh-package-list*
  (package-refresh-contents))

;;
;; THEME - auto-install
;;

;(jack-require-or-install 'rainbow-mode)

;; Loading themes at the very end of the file instead. Seems to work better
; (jack-load-theme 'dracula-theme) ;; material-theme darktooth-theme monokai-theme ample-theme zenburn-theme misterioso-theme
;(jack-load-theme 'gruvbox-theme)
;(jack-load-theme 'zerodark-theme)







;; https://github.com/bbatsov/zenburn-emacs/blob/master/zenburn-theme.el#L132
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "#424450"))))
 '(isearch ((((class color) (min-colors 89)) (:background "#ddbd78" :foreground "#3e4451"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "white"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "yellow"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "dark violet"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "magenta"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "saddle brown"))))
 '(show-paren-match ((((class color) (min-colors 89)) (:foreground "#61afef" :weight bold))))
 '(swiper-line-face ((t (:inherit highlight :background "gray0" :foreground "gray100"))))
 '(vhl/default-face ((t (:inherit default :background "yellow2")))))

(scroll-bar-mode -1)
(toggle-scroll-bar -1)



;; (when (and (string-equal system-type "darwin") (member "Iosevka" (font-family-list)))
;;   (set-default-font "Iosevka"))

;  (set-default-font "Menlo-14"))

;(member "Iosevka" (font-family-list))
;; (when (member "Terminus (TTF)" (font-family-list))
;;   (set-face-attribute 'default nil :font "Terminus (TTF)"))

(setq python-shell-interpreter "python3")
(setq flycheck-python-pycompile-executable "python3")
;;
;; packages to install
;;
;; package-install (since i always end up searching for this)

; smart-mode-line-powerline-theme
(let ((pkgs-to-install
       '(dash s projectile use-package diminish bind-key company company-anaconda
         company-tern company-jedi ace-jump-mode
         ace-jump-buffer rainbow-delimiters php-mode go-mode
         multiple-cursors fringe-helper flycheck f
         ido-sort-mtime flx-ido switch-window anzu smex exec-path-from-shell
         groovy-mode ag highlight-symbol ws-butler ht smart-mode-line
         fic-mode multi-term
         ido-vertical-mode dtrt-indent js2-mode scss-mode helm helm-projectile
         flyspell-correct-ivy request nyan-mode avy emmet-mode default-text-scale
         expand-region yaml-mode clojure-mode
         smooth-scrolling beacon hlinum google-this crux key-chord ace-mc
         persistent-scratch goto-last-change free-keys which-key helm-ag
         auto-dim-other-buffers easy-kill web-mode json-mode helm-swoop
         visual-regexp helm-themes grizzl spotify volume osx-dictionary hy-mode
         swiper delight spaceline web-beautify py-autopep8 undo-tree hydra slime gruvbox-theme zerodark-theme
         git-link smartparens move-text add-node-modules-path visible-mark cider terraform-mode puppet-mode
         vagrant-tramp emamux gh-md google-translate ivy-rich dockerfile-mode golden-ratio blacken
         fill-column-indicator company-tabnine helm-make prettier-js indent-guide origami iregister
         binclock haskell-mode flymd imenu-list typescript-mode deadgrep iflipb format-all ace-window powerthesaurus ts)))
  ;; install the packages
  (jack-require-or-install-all pkgs-to-install))

;;
;; POST PACKAGE INSTALL
;;
;; (setq helm-ag-use-agignore t)

;; format-all is poorly setup for overrides so the quick thing is just edit the code directly
;; (define-format-all-formatter shfmt
;;   (:executable "shfmt")
;;   (:install (macos "brew install shfmt"))
;;   (:modes sh-mode)
;;   (:format
;;    (format-all--buffer-easy
;;     executable
;;     "-i" "2" "-ci" "-ln" (cl-case (and (boundp 'sh-shell) (symbol-value 'sh-shell))
;;             (bash "bash") (mksh "mksh") (t "posix")))))

(global-set-key (kbd "M-h") 'iflipb-next-buffer)
(global-set-key (kbd "M-H") 'iflipb-previous-buffer)

(defun my-flymd-browser-function (url)
   (let ((browse-url-browser-function 'browse-url-firefox))
     (browse-url url)))
 (setq flymd-browser-open-function 'my-flymd-browser-function)

(add-to-list 'company-backends #'company-tabnine)


;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "<return>") #'company-complete-selection))

(setq helm-ag-base-command "ag --mmap --nocolor --nogroup --ignore-case --ignore=*terraform.tfstate.backup*")


(setq fci-rule-column 88)
(setq fci-rule-color "#888800")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
;(global-fci-mode 1)

;(bind-keys* ("C-c u" . global-fci-mode))

(bind-keys* ("<M-return>" . helm-make-projectile))

(indent-guide-global-mode)
;; fix
(global-unset-key (kbd "s-x"))
;(global-unset-key (kbd "C-x r"))
;(global-set-key (kbd "C-x r") 'undo-tree-redo)
(global-set-key (kbd "s-Z") 'undo-tree-redo)
(global-set-key (kbd "C-x u") 'undo-tree-undo)
(global-set-key (kbd "C-x v") 'undo-tree-visualize)
(global-undo-tree-mode t)

(defhydra hydra-go (global-map "C-c g")
  "go from point"
  ("g" google-this-noconfirm "google")
  ("d" define-word-at-point "define"))

(defhydra hydra-origami () ; (global-map "C-c 1")
  "origami"
  ("T" origami-toggle-all-nodes "toggle all" :color blue)
  ("H" origami-close-all-nodes "hide all" :color blue)
  ("S" origami-show-all-nodes "show all" :color blue)
  ("t" origami-toggle-node "toggle all" :color blue)
  ("s" origami-show-node "show node" :color blue)
  ("h" origami-close-node "hide node" :color blue)
  ("m" origami-mode "turn on mode" :color blue))
(global-set-key (kbd "C-c 1") 'hydra-origami/body) ;; so we always see the menu
;; see: https://github.com/abo-abo/hydra/wiki/Binding-Styles

(defhydra hydra-remove-stuff ()
  "remove-stuff"
  ("e" jack-copy-symbol-underpoint "select word under point and copy" :color blue)
  ("r" jack-delete-symbol-underpoint "remove word under point" :color blue)
  ("w" jack-cut-symbol-underpoint "remove word under point and copy" :color blue)
  ("d" jack-special-forward-delete "delete word and white space forward" :color blue))
(global-set-key (kbd "M-c ") 'hydra-remove-stuff/body)

(defhydra hydra-iregister ()
  "iregister"
  ("p" iregister-jump-to-previous-marker "previous" :color blue)
  ("n" iregister-jump-to-next-marker "next" :color blue)
  ("m" iregister-point-to-register "mark" :color blue)
  ("d" iregister-delete-marker-register "delete current" :color blue))
(global-set-key (kbd "C-c 7") 'hydra-iregister/body) ;; so we always see the menu

(defhydra hydra-emacs-utils ()
  "emacs-util"
  ("t" counsel-projectile-find-file "cmd-t" :color blue)
  ("1" emacs-uptime "emacs uptime" :color blue)
  ("2" (lambda () (interactive) (set-default-font "Iosevka-12")) "font to 12" :color blue)
  ("4" (lambda () (interactive) (set-default-font "Iosevka-14")) "font to 14" :color blue)
  ("6" (lambda () (interactive) (set-default-font "Iosevka-16")) "font to 16" :color blue)
  ("8" (lambda () (interactive) (set-default-font "Iosevka-18")) "font to 18" :color blue)
  ("0" (lambda () (interactive) (set-default-font "Iosevka-20")) "font to 20" :color blue))
(global-set-key (kbd "C-c 2") 'hydra-emacs-utils/body)

;(global-set-key (kbd "C-t") emamux:keymap)
;; (defhydra hydra-tmux-memory
;;   (global-map "C-t")
;;   "tmux muscle memeory bindings"
;;   ("e" eval-expression "eval expression"  :exit t)
;;   ("d" jack-debug-symbol-at-point "debug"  :exit t)
;;   ("\"" split-window-below "split below"  :exit t)
;;   ("'" split-window-below "split below"  :exit t)
;;   ("%" split-window-right "split window right"  :exit t)
;;   ("5" split-window-right "split window right"  :exit t)
;;   ("q" switch-window "switch window"  :exit t)
;;   ("o" other-window "other window"  :exit t)
;;   ("w" ivy-switch-buffer "list buffers"  :exit t)
;;   ("C-t" switch-to-prev-buffer "switch to previous buffer"  :exit t)
;;   ("z" delete-other-windows "zoom"  :exit t))

(defhydra hydra-diff
  (global-map "C-c d")
  "diff tools"
  ("g" dumb-diff-git-file "git-diff"  :exit t)
  ("d" dumb-diff "dumb-diff"  :exit t)
  ("1" dumb-diff-set-region-as-buffer1 "inject into diff buf 1"  :exit t)
  ("2" dumb-diff-set-region-as-buffer2 "inject into diff buf 2"  :exit t))


(load-file "/home/jack/code/dumb-java/dumb-java.el")
(defhydra hydra-dumb-java
  (global-map "C-z")
  "dumb java"
  ("d" ej-javadoc-proj "javadoc"  :exit t)
  ;;("i" ej-run-imports-fixer "imports-fix"  :exit t)
  ;;("c" ej-compile-proj "compile-proj"  :exit t)
  ("c" ej-compile-via-server "compile"  :exit t)
  ("j" ej-jar-via-server "jar"  :exit t)
  ("h" ej-info-via-server "info"  :exit t)
  ("i" ej-fix-imports-via-server "imports-fix"  :exit t)
  ("a" ej-import-and-compile "import and compile proj"  :exit t)
  ;("c" ej-clean-proj "compile-clean"  :exit t)
  ("b" ej-clean-and-compile-proj "clean and compile"  :exit t)

  )

(use-package move-text
  :bind (("C-x <up>" . move-text-line-up)
         ("C-x <down>" . move-text-line-down))
  :ensure)

(use-package undo-tree
  :bind (:map undo-tree-map
              ("C-x u" . undo-tree-undo)
              ("s-Z" . undo-tree-redo)
              ("C-x v" . undo-tree-visualize))
  :config (global-undo-tree-mode t))

(use-package visible-mark
  :init (defface visible-mark-active ;; put this before (require 'visible-mark)
             '((((type tty) (class mono)))
               (t (:background "magenta"))) "")
  :config
           (global-visible-mark-mode 1)
           (setq visible-mark-max 1)
           (setq visible-mark-faces `(visible-mark-face1 visible-mark-face2))
  :ensure)

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy dumb-jump-force-searcher 'ag dumb-jump-aggressive nil dumb-jump-debug nil dumb-jump-use-visible-window nil) ;; (setq dumb-jump-selector 'helm)
  ;:config (setq dumb-jump-selector 'ivy dumb-jump-force-searcher 'git-grep-plus-ag dumb-jump-aggressive nil dumb-jump-debug nil dumb-jump-use-visible-window nil) ;; (setq dumb-jump-selector 'helm)
  :ensure)

(use-package smartparens
  :bind (("M-g a" . sp-beginning-of-sexp))
  :bind (("M-g e" . sp-end-of-sexp))
  :ensure)

(use-package magit
  :bind (("C-x m" . magit-status))
  :ensure)

(use-package diff-hl
  :bind (("C-c 8" . diff-hl-previous-hunk)
         ("C-c 9" . diff-hl-next-hunk))
  :config (global-diff-hl-mode 1)
  :ensure)

;; TODO set to keybinding or hydra
;(diff-hl-flydiff-mode)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("M-g b" . swiper-all)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char))
  :ensure)

(use-package counsel-projectile
  :bind (("s-t" . counsel-projectile-find-file)
         ("s-o" . counsel-projectile-find-file)
         ("C-M-t" . jack-counsel-projectile-find-file-clear-cache)
         ("C-t" . counsel-projectile-find-file)
         :map dired-mode-map
         ("C-t" . counsel-projectile-find-file))
  :ensure)

(global-set-key (kbd "s-T") 'jack-counsel-projectile-find-file-clear-cache)
(global-set-key (kbd "s-r") 'revert-buffer)
(global-set-key (kbd "s-<left>") 'move-beginning-of-line)
(global-set-key (kbd "s-<right>") 'move-end-of-line)

;; (use-package writeroom-mode
;;   :config (jack-start-fullscreen)
;;   :ensure)

(require 'spaceline-config)

(spaceline-emacs-theme)

(spaceline-toggle-minor-modes-off)
(spaceline-toggle-major-mode-off)
(spaceline-toggle-buffer-modified-on)
(spaceline-toggle-buffer-size-off)
(spaceline-toggle-version-control-on)
(spaceline-toggle-buffer-encoding-abbrev-off)
(spaceline-toggle-projectile-root-on)
(spaceline-toggle-anzu-on)


(bind-keys* ("C-c f" . jack-reformat-block))

;; https://github.com/angrybacon/dotemacs/blob/master/dotemacs.org#12-mode-line
;; end spaceline config

;; Nice if I am listening to music only
;;(bind-keys* ("C-c `" . spotify-playpause))
;;(bind-keys* ("C-c v" . volume))
(ivy-mode 1)
(ivy-rich-mode t)

(setq ivy-rich-switch-buffer-align-virtual-buffer t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-rich-path-style 'abbrev)
(setq ivy-rich-switch-buffer-name-max-length 40)
(setq ivy-rich-switch-buffer-project-max-length 20)

(setq ivy-use-virtual-buffers nil)
(setq enable-recursive-minibuffers t)



(global-set-key (kbd "C-s") 'swiper)

(projectile-global-mode +1)
(setq projectile-enable-caching t)

; (diminish 'flycheck-mode "HOME")
(diminish 'flycheck-mode "WORK")
(diminish 'company-mode)
(diminish 'projectile-mode)
(diminish 'auto-dim-other-buffers-mode)
(diminish 'emacs-lisp-mode)
(diminish 'ws-butler-mode)
(diminish 'beacon-mode)
(diminish 'undo-tree-mode)
(diminish 'anzu-mode)
(diminish 'eldoc-mode)
(diminish 'ivy-mode)
(diminish 'golden-ratio-mode)

(helm-adaptive-mode)

(define-key company-active-map (kbd "C-n") (lambda () (interactive) (company-complete-common-or-cycle 1)))
(define-key company-active-map (kbd "C-p") (lambda () (interactive) (company-complete-common-or-cycle -1)))

(global-set-key (kbd "M-i") 'helm-swoop)
;(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
;(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-map (kbd "C-r") 'helm-previous-line)
(define-key helm-map (kbd "C-s") 'helm-next-line)

;(global-set-key (kbd "C-x C-f") 'helm-find-files)


;; fill-column-indicator
;; turn off fci for now. is buggy with company mode
;; (setq fci-rule-color "#6272a4") ; dracula theme comment color
;; (setq fci-dash-pattern 0.5)
;; (setq fci-rule-use-dashes t)
;; (setq fci-rule-column 100)

;; (define-globalized-minor-mode jack-global-fci-mode fci-mode turn-on-fci-mode)
;; (jack-global-fci-mode 1)

(global-set-key [remap kill-ring-save] 'easy-kill)

(bind-keys* ("s-p" . highlight-symbol-prev))
(bind-keys* ("s-n" . highlight-symbol-next))
;(bind-keys* ("M-p" . highlight-symbol-prev))
(bind-keys* ("M-p" . jack-special-prev))
;(bind-keys* ("M-n" . highlight-symbol-next))
(bind-keys* ("M-n" . jack-special-next))
(bind-keys* ("C-S-P" . scroll-down-line))
(bind-keys* ("C-S-N" . scroll-up-line))
;(bind-keys* ("C-x p" . helm-projectile-switch-project))
(bind-keys* ("C-x p" . counsel-projectile-switch-project))
(bind-keys* ("C-c p" . beginning-of-buffer))
(bind-keys* ("C-c n" . end-of-buffer))
;(bind-keys* ("C-`" . pop-to-mark-command))
(bind-keys* ("C-`" . pop-to-mark-command))
(bind-keys* ("M-`" . (lambda()(interactive)(push-mark))))

;; TODO: use a hydra for these
(bind-keys* ("C-M-," . jack-new-scratch))
;;(bind-keys* ("M-," . jack-select-scratch))
(bind-keys* ("M-," . jack-select-scratch))
(bind-keys* ("C-c v" . avy-goto-word-0))

(persistent-scratch-setup-default)

(key-chord-mode 1)
;; (key-chord-define-global "qw" 'undo)
;; (key-chord-define-global "gh" 'er/mark-symbol)
(key-chord-define-global "fj" 'jack-avy-goto-char)
;(key-chord-define-global "fj" 'avy-goto-subword-0)
;; (key-chord-define-global "gh" 'avy-goto-line)
;; (key-chord-define-global "dk" 'avy-goto-char-in-line)

; (auto-dim-other-buffers-mode t) ; (auto-dim-other-buffers-mode nil)

(hlinum-activate)

(beacon-mode 1)
(setq beacon-blink-duration 0.6)
(setq beacon-blink-delay 0.3)
(setq beacon-size 15)
(setq beacon-color "green")


;; (defun my-blacken-buffer (blacken-buffer &rest arguments)
;;   "Send t to blacken-buffer to display any issues"
;;   (apply 'blacken-buffer '(t)))

;; (advice-add #'blacken-buffer :around #'my-blacken-buffer)
;; (advice-remove #'blacken-buffer :around)


;(global-smartscan-mode 1)
;; http://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings
(bind-keys* ("C-c w" . whitespace-mode))

(bind-keys* ("C-M-f" . helm-projectile-ag))
;(bind-keys* ("C-M-f" . counsel-ag))
;(bind-keys* ("C-M-;" . jack-helm-projectile-ag-at-point))


;; (global-set-key (kbd "C-0") 'ace-jump-char-mode)
;; (global-set-key (kbd "C--") 'ace-mc-add-multiple-cursors)
;; (global-set-key (kbd "C-=") 'ace-mc-add-single-cursor)
;(define-key global-map (kbd "C-c s") 'avy-goto-char)

(global-set-key (kbd "M-g f") 'jack-helm-projectile-ag-at-point)
(global-set-key (kbd "M-g r") 'jack-smart-deadgrep)
;(global-set-key (kbd "M-g f") 'helm-git-grep-at-point)
(global-set-key (kbd "M-g d") 'osx-dictionary-search-word-at-point)
;(global-set-key (kbd "M-g f") 'jack-counsel-git-grep-at-point)


(defface jack-avy-lead-face-0
  '((t (:foreground "white" :background "red"))) "color override 0")

(defface jack-avy-lead-face-1b
  '((t (:foreground "white" :background "OrangeRed3"))) "color override 1")

(defface jack-avy-lead-face-2
  '((t (:foreground "white" :background "green"))) "color override 2")

(defface jack-avy-lead-face-3
  '((t (:foreground "white" :background "blue"))) "color override 3")

(setq avy-lead-faces '(jack-avy-lead-face-0 jack-avy-lead-face-1b jack-avy-lead-face-2 jack-avy-lead-face-3 jack-avy-lead-face-0 jack-avy-lead-face-1b jack-avy-lead-face-2 jack-avy-lead-face-3))

(setq avy-all-windows nil)
; (setq avy-keys (number-sequence ?a ?z))
(setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?o ?q ?e ?r ?t ?z ?x ?c ?b))

(setq avy-background nil) ; t
(setq avy-case-fold-search nil)

;; (advice-add #'avy-goto-char :after-while #'jack-avy-to-char-post)
; (setq debug-on-error t)
;; (advice-remove #'avy-goto-char  #'jack-avy-to-char-post)

(global-set-key (kbd "M-g h") 'highlight-symbol-at-point)
(global-set-key (kbd "M-g l") 'avy-goto-line)
;(global-set-key (kbd "M-g c") 'avy-goto-char)
(global-set-key (kbd "M-g c") 'avy-goto-char-in-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-0)
(global-set-key (kbd "M-g t") 'avy-goto-char-timer)
;(bind-keys* ("C-z" . avy-goto-char))
;(global-set-key (kbd "C-0") 'avy-goto-char)
;(global-set-key (kbd "C-\\") 'avy-goto-char)
;(global-set-key (kbd "C-\\") 'avy-goto-char)
;(global-set-key (kbd "C-;") 'avy-goto-char)
;(global-set-key (kbd "C-.") 'avy-goto-char)
;(global-set-key (kbd "C-,") 'avy-goto-subword-0)
;(global-set-key (kbd "C-,") ''avy-goto-char)
;(global-set-key (kbd "s-s") 'avy-goto-subword-0)
;(define-key global-map (kbd "C-c s") 'avy-goto-subword-1)
;(define-key global-map (kbd "C-z") 'avy-goto-subword-1)
;(define-key global-map (kbd "C-c s") 'avy-goto-char-timer)
;(define-key global-map (kbd "C-=") 'avy-goto-char-timer)
;(define-key global-map (kbd "C-c a") 'ace-jump-mode-pop-mark)
;; (bind-keys* ("C-," . (lambda () (message "%s" "test"))))
;; (global-set-key (kbd "C-.") (lambda () (message "%s" "test")))
(bind-keys* ("C-," . jack-avy-goto-char))
(bind-keys* ("C-q" . recenter-top-bottom))
(bind-keys* ("C-l" . jack-avy-goto-char))
(bind-keys* ("M-," . jack-avy-goto-char))

(bind-keys* ("C-'" . imenu-list-smart-toggle))
(bind-keys* ("M-r" . counsel-yank-pop))
;(global-set-key (kbd "M-SPC") 'avy-goto-char)


;(string= (char-to-string (c-int-to-char 10)) "\n")

;(global-set-key (kbd "C-\'") 'er/expand-region)

;(global-set-key (kbd "C-c g") 'google-this-noconfirm)

;; (global-set-key (kbd "\C-c <up>") 'text-scale-increase)     ;; text size up
;; (global-set-key (kbd "\C-c <down>") 'text-scale-decrease)   ;; text size down
;; using a plugin across all buffers with default-text-scale
;(global-set-key (kbd "C-c <up>") 'default-text-scale-increase)     ;; text size up
;(global-set-key (kbd "C-c <down>") 'default-text-scale-decrease)   ;; text size down

(global-set-key (kbd "C-c t") 'jack-toggle-chinese-input)
;(global-set-key (kbd "C-c 3") 'crux-transpose-windows)

(global-set-key (kbd "C-c 3") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "C-c 4") 'kmacro-end-or-call-macro)

;(global-set-key (kbd "C-c 7") 'goto-last-change)

(global-set-key (kbd "C-<backspace>") 'jack-special-delete)
(global-set-key (kbd "M-<backspace>") 'jack-special-delete)

; (nyan-mode)
(projectile-mode)
;; (flyspell-lazy-mode 1)
;; (flyspell-mode 1)
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-jsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-jsx-mode))
(sml/setup)
;(sml/apply-theme 'dark)
(sml/apply-theme 'respectful)
(ws-butler-global-mode t)
(highlight-symbol-nav-mode 1)

(smex-initialize)
(global-anzu-mode +1)

;(set-face-attribute 'anzu-mode-line nil :foreground "yellow" :weight 'bold)
(set-face-attribute 'anzu-mode-line nil :foreground "light green" :weight 'bold)

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
(add-hook 'python-mode-hook 'blacken-mode)
(add-to-list 'company-backends 'company-tern)
(setq company-tern-property-marker "")
(setq company-tern-meta-as-single-line t)
(setq company-idle-delay 1.1)

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

(ido-sort-mtime-mode 1)
;(ido-better-flex/enable)
;(ido-ubiquitous-mode)

;(add-hook 'after-init-hook #'global-flycheck-mode)

;;
;;keybindings for packages
;;

;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c \$") 'jack-save-word)
;(global-set-key (kbd "s-t") 'projectile-find-file)

;(global-set-key (kbd "C-x v") 'projectile-switch-to-buffer)
;(global-set-key (kbd "C-x b") 'ido-switch-buffer)
;(global-set-key (kbd "C-x b") 'helm-buffers-list)

(global-set-key (kbd "C-x e") 'eval-expression)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-x C-b") 'ivy-switch-buffer)
;(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x p") 'projectile-switch-project)
(global-set-key (kbd "s-y") 'projectile-switch-project)
(global-set-key (kbd "C-x f") 'projectile-grep)
;(global-set-key (kbd "M-x") 'smex)
;(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command) ;;old M-x.
;(global-set-key (kbd "C-x o") 'switch-window)
;(global-set-key (kbd "C-x o") 'other-window)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-c .") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c ,") 'mc/edit-lines)

;; (setq ace-jump-mode-scope 'window)
;; (setq ace-jump-mode-move-keys
;;       (loop for i from ?a to ?z collect i))
;; (define-key global-map (kbd "C-c s") 'ace-jump-char-mode)





(define-key flyspell-mode-map (kbd "C-c \$") 'jack-save-word)
(define-key flyspell-mode-map (kbd "C-;") 'flyspell-correct-previous-word-generic)

;(global-set-key (kbd "M-<down>") (quote scroll-up-line)) ;; scroll by one line --
;(global-set-key (kbd "M-<up>") (quote scroll-down-line))

(bind-keys* ("M-<down>" . scroll-up-line))
(bind-keys* ("M-<up>" . scroll-down-line))

;(define-key global-map (kbd "C-s") 'isearch-forward)
(bind-keys* ("<C-return>" . set-rectangular-region-anchor))


;; jump to start end of tags in sgml modes (html, etc)

(add-hook 'sgml-mode-hook
  (lambda ()
   (define-key sgml-mode-map (kbd "C-M-n") 'sgml-skip-tag-forward)
   (define-key sgml-mode-map (kbd "C-M-p") 'sgml-skip-tag-backward)))

;; http://stackoverflow.com/questions/2903426/display-path-of-file-in-status-bar
;; (setq frame-title-format  ;show directory and filename on frame top
;;       (list (format "%s %%S: %%j " (system-name))
;;         '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

(setq frame-title-format "%F |  🤯 | %b | %&")

;;
;; HOOKS
;;
(add-hook 'after-change-major-mode-hook 'infer-indentation-style)
(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'scss-mode-hook 'xah-syntax-color-hex)

;; ;; from: https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking/
;; (flycheck-define-checker jsxhint-checker
;;   "A JSX syntax and style checker based on JSXHint."

;;   :command ("jsxhint" source)
;;   :error-patterns
;;   ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
;;   :modes (js2-jsx-mode))
;; (add-hook 'js2-jsx-mode-hook (lambda ()
;;                           (flycheck-select-checker 'jsxhint-checker)
;;                           (flycheck-mode)))

(add-hook 'clojure-mode-hook 'fic-mode)
(add-hook 'go-mode-hook 'fic-mode)
(add-hook 'python-mode-hook 'fic-mode)
(add-hook 'javascript-mode-hook 'fic-mode)
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'js-mode-hook 'fic-mode)
(add-hook 'js-mode-hook 'fic-mode)
(add-hook 'web-mode-hook 'fic-mode)
(add-hook 'java-mode-hook 'fic-mode)
(add-hook 'emacs-lisp-mode-hook 'fic-mode)
(add-hook 'web-mode-hook 'fic-mode)
(add-hook 'js2-mode-hook 'fic-mode)
(add-hook 'terraform-mode-hook 'fic-mode)

(add-hook 'javascript-mode-hook 'flycheck-mode)

(add-hook 'mouse-leave-buffer-hook 'jack-stop-using-minibuffer)

(setq prettify-symbols-alist '(("lambda" . ?λ)))
(setq python--prettify-symbols-alist prettify-symbols-alist)

(add-hook 'python-mode-hook
  (lambda ()
    (prettify-symbols-mode)
    (setq indent-tabs-mode nil)
    (setq python-indent 4)
    (setq tab-width 4)))

(add-hook 'go-mode-hook
  (lambda ()
    (setq tab-width 4)))

;(global-prettify-symbols-mode nil)

(add-hook 'hy-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'before-save-hook #'gofmt-before-save)
(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

(add-hook 'kill-emacs-hook 'pmdm-write-opened-files)

(if (string-equal system-type "darwin")
  (setq gofmt-command "gofmt")
  (setq gofmt-command "~/go/bin/gofmt"))

(setq gofmt-command "/usr/local/go/bin/gofmt")



;; make helm selections easier to see
; (set-face-attribute 'helm-selection nil :background "yellow" :foreground "black")
(set-face-attribute 'helm-selection nil :background "light green" :foreground "black")

;; cursor tweaks
(let ((my-select-color "light green")) ;; #83F52C is a neon green, FF8300 is orange -- but plain "green" is also nice
  (set-face-background 'region my-select-color) ;; make region stick out more
  (set-cursor-color my-select-color))

(setq-default cursor-type '(bar . 2))

;; always use a random beacon color
;; (setq jacks-colors (--reject (s-contains? "gray" it) (defined-colors)))
;; (add-function :before (symbol-function 'beacon-blink-automated)
;;               (lambda ()
;;                 (setq beacon-color (nth (random (length jacks-colors)) jacks-colors))
;;                 (setq beacon-size (+ 15 (random 60)))))

(setq beacon-color "light green")
(setq beacon-size 80)


;;;;;;;;;;;;;;;;;;;;;

; https://www.npmjs.com/package/eslint-plugin-import
;sudo npm install eslint-plugin-import -g
;sudo npm install eslint -g

;; in javascript project root: npm install eslint-plugin-import --save-dev

; https://github.com/flycheck/flycheck/issues/1175

;; use web-mode for .jsx files
;(add-to-list 'auto-mode-alist '("\\.js$" . javascript-mode))

(eval-after-load 'javascript-mode
  '(add-hook 'javascript-mode-hook #'add-node-modules-path))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'javascript-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist go-gofmt go-golint go-vet go-build go-test go-errcheck go-unconvert go-megacheck)))


(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(sh-shellscript sh-bash)))


;;;;;;;;;;;;;;;;;




(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;;; start common lisp stuff

(require 'cl)

(setq inferior-lisp-program "ros -Q run")
(require 'slime)
(slime-setup '(slime-fancy))    ;adds some nice features

;; these give you unicode
(set-language-environment "UTF-8")
(setenv "LC_LOCALE" "en_US.UTF-8")
(setenv "LC_CTYPE" "en_US.UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

;;; end common lisp stuff

; make default font size slightly bigger...
;f(set-face-attribute 'default nil :height 140)

(global-diff-hl-mode 1)

(midnight-mode)

;; make it totally clear which buffers are not saved
(set-face-attribute 'helm-buffer-modified nil
                        :overline  nil
                        :underline  "red"
                        :foreground "red")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DONE - report time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; override
(defun display-startup-echo-area-message ()
  (message "Emacs!"))

(jack-load-theme 'zerodark-theme)
(zerodark-setup-modeline-format)

(use-package kaolin-themes
  :ensure
  :config
  ;(load-theme 'kaolin-valley-dark t)
;  (load-theme 'kaolin-galaxy t) ;; home
  (load-theme 'kaolin-bubblegum t) ;; work
  (kaolin-treemacs-theme))

;; to detect if in `home` mode
; (setq global-mode-string (append global-mode-string "HOME"))

;;

;; force the fringe to match the current theme's bg color
(let ((cur-bg-color (face-attribute 'default :background)))
  (set-face-attribute 'fringe nil :background cur-bg-color)
  (setq auto-dim-other-buffers-face (color-lighten-name cur-bg-color 25)))

;; nova-theme?

;(server-start)
(pmdm-load-files)
(message ".emacs loaded in %s seconds" (mapconcat 'int-to-string (rest (time-since *start-time*)) "."))
(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

; (s-join "\n" (font-family-list))
;; (when (member "Iosevka" (font-family-list))
;;   (set-frame-font "Iosevka-20"))

(when (member "Noto Mono" (font-family-list))
  (set-frame-font "Noto Mono-12"))

(golden-ratio-mode 1)
(golden-ratio-toggle-widescreen)


(when (string-equal system-type "darwin")
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(let* ((custom-file-name "overrides.el")
      (custom-file-path (concat (file-name-as-directory emacsdir) custom-file-name)))
  (if (file-exists-p custom-file-path)
      (load-file custom-file-path)
    (message "No %s file found in %s" custom-file-name emacsdir)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default)))
 '(package-selected-packages
   (quote
    (ts ace-window powerthesaurus mw-thesaurus protobuf-mode sort-words matlab-mode kaolin-themes counsel-projectile counsel diff-hl magit dumb-jump format-all iflipb deadgrep typescript-mode imenu-list flymd haskell-mode binclock iregister origami indent-guide prettier-js helm-make company-tabnine fill-column-indicator blacken golden-ratio dockerfile-mode ivy-rich google-translate gh-md emamux vagrant-tramp puppet-mode terraform-mode cider visible-mark add-node-modules-path move-text smartparens git-link zerodark-theme gruvbox-theme slime hydra undo-tree py-autopep8 web-beautify spaceline delight yaml-mode ws-butler which-key web-mode volume visual-regexp use-package switch-window swiper spotify smooth-scrolling smex smart-mode-line scss-mode request rainbow-delimiters php-mode persistent-scratch osx-dictionary nyan-mode multi-term key-chord json-mode js2-mode ido-vertical-mode ido-sort-mtime hy-mode ht hlinum highlight-symbol helm-themes helm-swoop helm-projectile helm-ag groovy-mode grizzl goto-last-change google-this go-mode fringe-helper free-keys flyspell-correct-ivy flycheck flx-ido fic-mode expand-region exec-path-from-shell emmet-mode easy-kill dtrt-indent diminish default-text-scale crux company-tern company-jedi company-anaconda clojure-mode beacon auto-dim-other-buffers anzu ag ace-mc ace-jump-buffer))))
