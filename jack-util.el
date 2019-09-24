(require 'time-stamp)
(require 'ts)

(defun vlad-to-snake-case ()
  (interactive)
  (replace-regexp "\\([A-Z]\\)" "-\\1" nil (region-beginning) (region-end))
  (downcase-region (region-beginning) (region-end)))

(defun vlad-css-ify ()
  (interactive)
  (let ((beginning (region-beginning)) (end (region-end)))
    (replace-regexp " *[=] *" ": " nil beginning end)
    (replace-regexp "[,]\n" ";\n" nil beginning end)
    (replace-regexp "[`'\"]" "" nil beginning end)
    (to-snake-case)))

(defun jack-enforce-seconds (ts)
  (cond
   ((< ts 7000000000)
    `("seconds" ,ts))
   ((< ts 7000000000000)
    `("milli" ,(/ ts 1000)))
   ((< ts 7000000000000000)
    `("micro" ,(/ ts 1000000)))
   ((t
     ts))))

(defun jack-human-ts ()
  (interactive)
  (let* ((raw-ts (thing-at-point 'number))
         (parsed-ts (jack-enforce-seconds raw-ts))
         (unit-ts (nth 0 parsed-ts))
         (real-ts (nth 1 parsed-ts)))
    (message "%s [%s] is %s" raw-ts unit-ts (ts-format (make-ts :unix real-ts)))))

(defun jack-work-mode ()
  (interactive)
  (diminish 'flycheck-mode "WORK")
  (load-theme 'kaolin-bubblegum t))

(defun jack-home-mode ()
  (interactive)
  (diminish 'flycheck-mode "HOME")
  (load-theme 'kaolin-galaxy t))

(defun jack-run-python3-buffer ()
  (interactive)
  (let* ((buff (buffer-file-name))
         (cmd (format "python3 %s" buff))
         (result (shell-command-to-string cmd)))
    (message "%s" result)))

(defun jack-toggle-chinese-input ()
  (interactive)
  (if (null default-input-method)
      (progn
        ;; hack
        (set-input-method 'chinese-py)
        (set-input-method 'chinese-py))
    (set-input-method nil)))

(defun jack-english-to-chinese ()
  (interactive)
  (google-translate-translate
     "en" "zh-CN" (jack-get-region-or-word)))

(defun jack-chinese-to-english ()
  (interactive)
  (google-translate-translate
     "zh-CN" "en" (jack-get-region-or-word)))

(defun jack-get-region-or-word ()
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (or (and (setq bounds (bounds-of-thing-at-point 'word))
             (buffer-substring-no-properties (car bounds) (cdr bounds)))
        (error "No word at point."))))

;      ((content (buffer-substring-no-properties (region-beginning) (region-end)))
(defun jack-smart-translate ()
  (interactive)
  (let* ((content (jack-get-region-or-word))
         (is-cn (null (string-match-p "[a-z]" content))))
    (if is-cn
        (google-translate-translate "zh-CN" "en" content)
      (google-translate-translate "en" "zh-CN"  content))))

(defun jack-insert-code-next-line (code)
  (move-end-of-line nil)
  (insert "\n")
  (jack-match-above-indentation)
  (insert code))

(defun jack-debug-symbol-at-point ()
  (interactive)
  (let ((cur-symbol (thing-at-point 'symbol))
        (cur-ext (f-ext (buffer-file-name))))
    (cond
     ((string= cur-ext "py")
      (jack-insert-code-next-line (format "print(\'value of `%s`\', %s)" cur-symbol cur-symbol)))
     ((string= cur-ext "js")
      (jack-insert-code-next-line (format "console.log(\'value of `%s`\', %s);" cur-symbol cur-symbol))))))


(defun jack-search-all-buffers ()
  (interactive)
  (multi-occur-in-matching-buffers "." (read-from-minibuffer "Search all buffers for: ")))

(defun jack-delete-symbol-underpoint ()
  (interactive)
  (jack--remove-symbol-underpoint nil))

(defun jack-cut-symbol-underpoint ()
  (interactive)
  (jack--remove-symbol-underpoint t))

(defun jack--remove-symbol-underpoint (do-copy)
  (with-no-warnings
    (call-interactively 'er/mark-symbol)
    (when do-copy
      (kill-ring-save (region-beginning) (region-end) t))
    (call-interactively 'backward-delete-char)
    (when (and (jack-is-prev-char-space?)
               (jack-is-next-char-space?))
      (call-interactively 'delete-char))))

;; (kill-new "random string") to add an arb string to the kill-ring
(defun jack-copy-symbol-underpoint ()
  (interactive)
  (with-no-warnings
    (call-interactively 'er/mark-symbol)
    (kill-ring-save (region-beginning) (region-end) t)))


(defun jack-special-delete ()
  (interactive)
  (let ((start-point (point)))
    (jack-hungry-delete)
    (when (= (point) start-point)
      (jack-backward-delete-word 1))))

(defun jack-special-forward-delete ()
  (interactive)
  (let ((start-point (point)))
    (jack-hungry-forward-delete)
    (when (= (point) start-point)
      (jack-forward-delete-word 1))))


(defun jack-special-next ()
  "Try going to next symbol or avy-got-char."
  (interactive)
  (let ((start-point (point))
        (tmp-sym (thing-at-point 'symbol)))
    (if tmp-sym
        (progn
          (highlight-symbol-next)
          (when (= (point) start-point)
            (call-interactively 'avy-goto-char)))
      (call-interactively 'avy-goto-char))))


(defun jack-special-prev ()
  "Try going to prev symbol or avy-got-char."
  (interactive)
  (let ((start-point (point))
        (tmp-sym (thing-at-point 'symbol)))
    (if tmp-sym
        (progn
          (highlight-symbol-prev)
          (when (= (point) start-point)
            (call-interactively 'avy-goto-char)))
      (call-interactively 'avy-goto-char))))

(defun jack-hungry-delete ()
  "Keep deleting backward as long as it sees whitespace."
  (interactive)
  (let ((cur (char-to-string
              (char-before (point)))))
    (when (or (string-equal " " cur)
              (string-equal "\t" cur))
      (delete-char -1)
      (jack-hungry-delete))))

(defun jack-hungry-forward-delete ()
  "Keep deleting forward as long as it sees whitespace."
  (interactive)
  (let ((cur (char-to-string
              (char-after (point)))))
    (when (or (string-equal " " cur)
              (string-equal "\t" cur))
      (delete-char 1)
      (jack-hungry-forward-delete))))

;; http://stackoverflow.com/a/6133921/24998
;; don't want to add delete word to the kill-ring
(defun jack-backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun jack-forward-delete-word (arg)
  "Delete characters forward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun jack-start-fullscreen ()
  (interactive)
  (require 'writeroom-mode)
  (set-frame-parameter writeroom--frame 'writeroom-fullscreen (frame-parameter writeroom--frame 'fullscreen))
  (set-frame-parameter writeroom--frame 'fullscreen writeroom-fullscreen-effect))

(defun jack-stop-fullscreen ()
  (interactive)
  (set-frame-parameter writeroom--frame 'fullscreen (frame-parameter writeroom--frame 'writeroom-fullscreen))
  (set-frame-parameter writeroom--frame 'writeroom-fullscreen nil))

(defun jack-counsel-projectile-find-file-clear-cache ()
  (interactive)
  (projectile-invalidate-cache nil)
  (counsel-projectile-find-file))

(defun jack-helm-projectile-ag-at-point ()
  (interactive)
  (let ((helm-ag-insert-at-point 'symbol))
    (helm-projectile-ag)))

(defun jack-counsel-git-grep-at-point ()
  (interactive)
  (counsel-git-grep nil (thing-at-point 'symbol)))

(defun jack-counsel-ag-at-point ()
  (interactive)
  (counsel-ag (thing-at-point 'symbol)))

;;TODO: if col is 0 keep going up ALSO go down and if below is higher col than up then use below
(defun jack-match-above-indentation-old ()
  (interactive)
  (let ((above-col (save-excursion
                     (previous-line)
                     (back-to-indentation)
                     (current-column))))
    (move-beginning-of-line nil)
    (delete-horizontal-space)
    (insert-char (string-to-char " ") above-col)))


(defun jack-match-above-indentation ()
  (interactive)
  (let ((above-col (save-excursion
                     (previous-line)
                     (back-to-indentation)
                     (current-column)))
        (below-col (save-excursion
                     (next-line)
                     (back-to-indentation)
                     (current-column))))
    (move-beginning-of-line nil)
    (delete-horizontal-space)
    (insert-char (string-to-char " ") (max below-col above-col))))

(defun jack-is-next-char-space? ()
  "If the next character is a space then t else nil."
  (string= (char-to-string (char-after (point))) " "))

(defun jack-is-next-n-char-boundary? (&optional n)
  "If the next N characters over is a boundary then t else nil."
  (let* ((point-offset (+ (point) (or n 0)))
         (use-char (char-after point-offset)))
    ;(message "~~~~trying to use char: %s" use-char)
    (member use-char (mapcar (lambda (x) (c-int-to-char x)) '(32 10 40 41 93 91 95 34 47)))))

(defun jack-is-prev-char-space? ()
  "If the previous character is a space then t else nil."
  (string= (char-to-string (char-before (point))) " "))

(defun jack-match-above-alignment ()
  "Insert enough spaces to match the whitespace above for the next column."
  (interactive)
  (let* ((cur-col (current-column))
         (to-match (save-excursion
                     (previous-line)
                     (when (not (jack-is-next-char-space?))
                       (while (not (jack-is-next-char-space?))
                         (forward-char 1)))
                     (while (jack-is-next-char-space?)
                       (forward-char 1))
                     (current-column)))
         (delta (- to-match cur-col)))
    (insert-char (string-to-char " ") delta)))

; http://stackoverflow.com/a/3417473/24998 adding confirmation
(defun jack-kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (when (y-or-n-p "Kill all other buffers? ")
      (mapc 'kill-buffer
            (delq (current-buffer)
                  (remove-if-not 'buffer-file-name (buffer-list))))))

(defun jack-kill-all-buffers ()
    "Kill all other buffers."
    (interactive)
    (when (y-or-n-p "Kill ALL buffers (except *scratch*)? ")
      (mapc 'kill-buffer
            (delq (get-buffer "*scratch*") (remove-if-not 'buffer-file-name (buffer-list))))))

(defun jack-kill-current-project-buffers ()
    "Kill all buffers in the current project."
    (interactive)
    (let* ((proj-root (projectile-project-root))
           (proj-buffers
            (-filter
             (lambda (x)
               (string-prefix-p
                proj-root
                (buffer-file-name x)))
             (buffer-list)))
           (proj-buffer-count (length proj-buffers)))
      (if (y-or-n-p (format "Kill ALL buffers in %s (%s bufffers)? " proj-root proj-buffer-count))
        (progn
          (mapc 'kill-buffer proj-buffers)
          (message "Killed %s buffers in %s" proj-buffer-count proj-root))
        (message "Cancelled killing %s buffers in %s" proj-buffer-count proj-root))))


;; http://emacs.stackexchange.com/a/2302
(defun my/eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil)))
  (message "%s eval'd" (buffer-file-name)))

(defun jack-run-ert-tests ()
  (interactive)
  (my/eval-buffer)
  ;(eval-buffer)
  (ert-run-tests-interactively t))

; based off of http://stackoverflow.com/a/10364547/24998
(defun jack-new-scratch ()
  "open up a new scratch buffer"
  (interactive)
  (switch-to-buffer (generate-new-buffer "*scratch*")))

(defun jack-select-scratch ()
  "select *scratch* buffer and insert delimiter if existing content between them"
  (interactive)
  (let* ((buf "*scratch*")
         (buf-ref (get-buffer-create buf))
         (delimiter "\n;;;;;;;;;;;;;;;;;;;;;")
         (contents (with-current-buffer buf-ref
                     (buffer-string)))
         (do-inject (not (string-prefix-p delimiter contents))))
    (when do-inject
      (with-current-buffer buf-ref
        (goto-char (point-min))
        (insert (format "%s - ^^^ %s\n" delimiter (time-stamp-string)))
        (goto-char (point-min))))
    (switch-to-buffer buf)))

(defun jack-insert-backtick ()
  "because i override tilda/backtick for kupfer, but need backticks a lot for markdown"
  (interactive)
  (insert "`"))

;; forked from: https://github.com/bbatsov/prelude/blob/3017a151501d12695541706c717558dfb614f0c4/core/prelude-core.el#L294
(defun jack-sudo-edit (&optional arg)
  "Edit currently visited file as root."
  (interactive "P")
  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))

(defun jack-require-or-install (pkg)
	(when (not (require pkg nil t))
         (package-install pkg)))

(defun jack-require-or-install-all (pkgs)
	(mapcar (lambda (pkg) (funcall 'jack-require-or-install pkg))
			pkgs))

(defun jack-load-theme (the-theme)
	(let ((just-name (intern (car (split-string (symbol-name the-theme) "-")))))
		(unless (member just-name (custom-available-themes))
		  (package-install the-theme))
		(load-theme just-name t)))


;;(jack-url-to-file "http://unarm.org" "/home/jack/unarm.txt")
(defvar php-lint-cmd "php -l %s")

(defun jack-get-random-quote ()
  (let* ((quote-url "http://www.iheartquotes.com/api/v1/random?max_lines=1")
         (content (jack-url-get-contents quote-url)))
    (s-trim (second (split-string content "\n")))))

(defun jack-java-format ()
	(interactive "*")
		(let* ((fileName (car (last (split-string (buffer-file-name) "/"))))
			   (tmpPath (format "/tmp/%s" fileName)))

			(when (file-exists-p tmpPath)
				(delete-file tmpPath))

			(copy-file (buffer-file-name) tmpPath)

			;; TODO: have user settable vars for customizing paths, or a special json file .java-fix-imports with them defined
			(shell-command (format "%s %s %s %s"
				"python" "~/code/java-import-fixer/java_import_fixer.py" tmpPath "~/code/java-import-fixer/test_assets/jars"))

			(message "%s" "Formatting...")
			(shell-command (format "%s %s %s %s %s %s %s %s"
				"~/bin/eclipse/eclipse" "-nosplash" "-application"
				"org.eclipse.jdt.core.JavaCodeFormatter" "-verbose" "-config"
				"~/code/dotemacs24/org.eclipse.jdt.core.prefs" tmpPath))

			(erase-buffer)
			(insert-file-contents tmpPath)

			(message "%s" "Done Formatting")))

;; TODO: break up above and below into reusable sections...
(defun jack-java-fix-imports ()
	(interactive "*")
		(let* ((fileName (car (last (split-string (buffer-file-name) "/"))))
			   (startContent (buffer-string))
			   (tmpPath (format "/tmp/%s" fileName)))

			(when (file-exists-p tmpPath)
				(delete-file tmpPath))

			(with-temp-file tmpPath (insert startContent))

			(message "%s" "Fixing Imports...")
			;; TODO: have user settable vars for customizing paths, or a special json file .java-fix-imports with them defined
			;; ensure we run locally via tramp
			(let ((default-directory  "~"))
				(shell-command (format "%s %s %s %s"
;				"python" "~/code/java-import-fixer/java_import_fixer.py" tmpPath "~/code/java-import-fixer/test_assets/jars"))
				"python" "~/code/java-import-fixer/java_import_fixer.py" tmpPath "/home/jack/code/imgix-storm/build/classes:/home/jack/code/imgix-storm/lib")))

			(erase-buffer)
			(insert-file-contents tmpPath)

			(message "%s" "Done Fixing Imports")))

(defun jack-stick-out ()
  (interactive "*")
	(set-face-background 'region "yellow") ;; make region stick out more
	(set-cursor-color "green"))

(defun jack-newline-and-indent-no-spaces ()
  (interactive "*")
  (delete-horizontal-space t)
  (newline)
  (indent-according-to-mode)
  (let ((cur-point (point)))
	  (beginning-of-line)
		(while (re-search-forward "[ ]+" cur-point t)
		   (replace-match "\t"))
    (end-of-line)))

(defun jack-url-get-contents (url)
  (jack-remove-headers (with-current-buffer (url-retrieve-synchronously url)
	(buffer-string))))

(defun jack-url-to-file (url file)
	(with-temp-file file (insert (jack-url-get-contents url))))

(defun jack-remove-headers (content)
	(defun chomp-headers (hlist)
		(if (= 0 (length (first hlist)))
			hlist
			(chomp-headers (rest hlist))))
	(let ((content-lines (split-string content "\n")))
		(mapconcat 'identity (chomp-headers content-lines) "\n")))

(defun jack-shift-region(numcols)
  "from http://ignaciopp.wordpress.com/2009/06/17/emacs-indentunindent-region-as-a-block-using-the-tab-key/"
  (if (< (point)
		 (mark))
	  (if (not(bolp))
		  (progn
			(beginning-of-line)
			(exchange-point-and-mark)
			(end-of-line)))
	(progn
	  (end-of-line)
	  (exchange-point-and-mark)
	  (beginning-of-line)))
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
	(if (< (point)
		   (mark))
		(exchange-point-and-mark))
	(let ((save-mark (mark)))
	  (indent-rigidly region-start region-finish numcols)
	  (tabify (region-beginning) (region-end)))))

(defun jack-unindent-block()
	(interactive)
	(jack-shift-region (- 4))
	(setq deactivate-mark nil))

(defun jack-indent-block()
	(interactive)
	(jack-shift-region 4)
	(setq deactivate-mark nil))


;; https://emacs.stackexchange.com/a/16793/2738
(defun jack-current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun jack-reformat-next-line ()
       (interactive)
       (with-current-buffer (current-buffer)
         (next-line nil)
         (back-to-indentation)
         (kill-line 0)
         (indent-for-tab-command)))

(defun jack-reformat-block ()
       (interactive)
       (if (null (jack-current-line-empty-p))
           (progn
             (jack-reformat-next-line)
             (jack-reformat-block))
         (message "%s" "Done formatting block")))

(defun jack-backward-kill-line (arg)
  "removes all tabs/spaces on the front of the line regardless of where your cursor is"
  (interactive "p")
  (back-to-indentation)
  ;;(delete-trailing-whitespace)
  (kill-line 0))

(defun jack-delete-line-no-kill ()
  (interactive)
  (delete-region
   (point)
   (save-excursion (move-end-of-line 1) (point)))
  (delete-char 1))

;; put (jack-train-human) at the end of your .emacs file if you want to force yourself
;; to not use the arrow keys or mouse...
(defun jack-train-human ()
	(interactive)
	(global-set-key	 [up] '(lambda()(interactive)(message "%s" "bad human!")))
	(global-set-key	 [down] '(lambda()(interactive)(message "%s" "bad human!")))
	(global-set-key	 [right] '(lambda()(interactive)(message "%s" "bad human!")))
	(global-set-key	 [left] '(lambda()(interactive)(message "%s" "bad human!")))

	(dolist (k '([mouse-1] [down-mouse-1] [drag-mouse-1] [double-mouse-1] [triple-mouse-1]
				 [mouse-2] [down-mouse-2] [drag-mouse-2] [double-mouse-2] [triple-mouse-2]
				 [mouse-3] [down-mouse-3] [drag-mouse-3] [double-mouse-3] [triple-mouse-3]
				 [mouse-4] [down-mouse-4] [drag-mouse-4] [double-mouse-4] [triple-mouse-4]
				 [wheel-down] [wheel-up]
				 [mouse-5] [down-mouse-5] [drag-mouse-5] [double-mouse-5] [triple-mouse-5]))
	  (global-unset-key k)
	  (global-set-key k '(lambda()(interactive)(message "%s" "bad human!")))))

;;
;; calls to command line programs...
;;
(defun jack-magic-lint ()
  (interactive)
  (save-buffer)
  (let ((the_mode (symbol-name
					(with-current-buffer (current-buffer)
					  major-mode)))
		 (lang_cmds '()))

	(add-to-list 'lang_cmds (cons "php-mode"  php-lint-cmd))
	(add-to-list 'lang_cmds (cons "python-mode" "pep8 --ignore=W191,E501 --show-source %s")) ;; W191 = tab chars
	(add-to-list 'lang_cmds (cons "js-mode" "~/bin/jsl-0.3.0/src/Linux_All_DBG.OBJ/jsl -process  %s"))

	(shell-command (format (cdr (assoc the_mode lang_cmds)) (buffer-file-name)))))


(defun jack-git-blame-line ()
  (interactive)
  (let ((cmd_tmpl "git blame -L %s,+3 %s")
        (minibuffer-message-timeout 20)
        (current_line (cadr (split-string (what-line) " "))))
	(minibuffer-message (shell-command-to-string (format cmd_tmpl current_line (buffer-file-name))))))

;;
;; window
;;

(defun jack-toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
	(set-frame-parameter nil 'fullscreen
						 (if (equal 'fullboth current-value)
							 (if (boundp 'old-fullscreen)
								 old-fullscreen
							   nil)
						   (progn
							 (setq old-fullscreen current-value)
							 'fullboth)))))
;; FROM: http://emacs-fu.blogspot.com/2009/02/transparent-emacs.html
(defun jack-alpha-change (&optional dec)
  "modify the transparency of the emacs frame; if DEC is t,
    decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
          (oldalpha (if alpha-or-nil alpha-or-nil 100))
          (newalpha (if dec (- oldalpha 10) (+ oldalpha 10))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(defun jack-emacs-maximize ()
	(cond
		((string-equal system-type "darwin")
			(set-frame-size (selected-frame)  235 70))
		((string-equal system-type "gnu/linux")
			(shell-command "wmctrl -r :ACTIVE: -b toggle,maximized_vert,maximized_horz"))
		(t
			(message "%s" "Did not try to maximize screen. System type not supported."))))

;; search accross open bufers - http://stackoverflow.com/a/3434098/24998
(defun jack-multi-occur-in-matching-buffers (regexp &optional allbufs)
  "Show all lines matching REGEXP in all buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(global-set-key (kbd "M-s /") 'jack-multi-occur-in-matching-buffers)

;; map command-3 (super-3) on macs so i can have fallback of ubuntu terminal keybinding
(when (string= system-type "darwin")
	(global-set-key (kbd "s-3")
		(lambda ()
			(interactive)
      (shell-command "osascript -e 'tell application \"Terminal\" to activate'"))))

;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
(defun jack-stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(defun jack-visible-bell ()
  (setq visible-bell nil)
  (setq ring-bell-function `(lambda ()
  (let ((cur (face-attribute 'default :background)))
    ;;(set-face-background 'default "orange1")
    (set-face-background 'default "#c678dd")
    (set-face-background 'default cur)))))


;; modified from: http://stackoverflow.com/questions/22107182/in-emacs-flyspell-mode-how-to-add-new-word-to-dictionary
(defun jack-save-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (when (y-or-n-p (concat "Add the word " (car word) " to the dictionary"))
        (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location)))))

;;from `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 in current buffer. Version 2015-06-11"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))


(defun jack-avy-to-char-post () ;; needs to match the signature of the `avy-goto-char`
;(defun jack-avy-to-char-post () ;; needs to match the signature of the `avy-goto-char`
  (interactive)
  (when (jack-is-next-n-char-boundary? 1)
    (forward-char 1)))

(defun jack-avy-goto-char ()
  (interactive)
  (call-interactively 'avy-goto-char)
  (jack-avy-to-char-post))



;;
;; lang mood hook setups...
;;

(defun jack-php-setup ()
  (local-set-key (kbd "TAB") 'self-insert-command)
  (local-set-key (kbd "{") 'self-insert-command) ; support for "{" needed
  (local-set-key (kbd "}") 'self-insert-command)
  (local-set-key (kbd ":") 'self-insert-command)
  (local-set-key (kbd ";") 'self-insert-command))

;; (when (file-exists-p "~/code/javaflunky/editors/emacs/javaflunky-fix-imports.el")
;;   (load-file "~/code/javaflunky/editors/emacs/javaflunky-fix-imports.el"))


;; http://www.emacswiki.org/emacs/NoTabs
(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

;; http://www.emacswiki.org/emacs/NoTabs
(defun infer-indentation-style ()
  (interactive)
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (when (> space-count tab-count) (message "%s" "Indention type: spaces") (setq indent-tabs-mode nil))
    (when (> tab-count space-count) (message "%s" "Indention type: tabs") (setq indent-tabs-mode t))))


(defun jack-smart-deadgrep ()
  (interactive)
  (cond
   ((string-equal "java-mode" major-mode)
      (setq-default deadgrep--file-type (cons 'glob "*.java"))
      (call-interactively 'deadgrep))

   ((string-equal "python-mode" major-mode)
      (setq-default deadgrep--file-type (cons 'glob "*.py"))
      (call-interactively 'deadgrep))
   (t
    (setq-default deadgrep--file-type 'all)
    (call-interactively 'deadgrep))))

;; https://bitbucket.org/inigoserna/pmdm.el/src/ab67371889fb5e70d227f863377f8d1f567ec580?at=default
;;
(defvar pmdm-file-name (expand-file-name ".pmdm-files" user-emacs-directory)
  "Location of file to write in opened files.")

;;; Internal functions
;; Edited to make sure file exists first
(defun pmdm--read-files-list ()
  (when (file-exists-p pmdm-file-name)
    (with-temp-buffer
      (insert-file-contents pmdm-file-name)
      (delete-matching-lines "^;; ")
      (read (buffer-substring-no-properties (point-min) (point-max))))))

;;; Public interface
(defun pmdm-write-opened-files()
  "Write a list of currently opened files to the file defined in `pmdm-file-name'."
  (interactive)
  (let ((files (delq nil (mapcar 'buffer-file-name (buffer-list)))))
    (write-region (format ";; PMDM file.\n;; Please do not edit manually.\n%s"
                          (prin1-to-string files))
                  nil
                  pmdm-file-name)))

(defun pmdm-load-files ()
  "Load the files found in file `pmdm-file-name'."
  (interactive)
  (let* ((opened-files (delq nil (mapcar 'buffer-file-name (buffer-list))))
        (raw-files (pmdm--read-files-list))
        (limit-files (-slice raw-files 0 30))
        (files (--filter (file-exists-p it) limit-files))
        (count 0))
    (dolist (file files)
      (when (and (not (member file opened-files)) (file-exists-p file))
        (find-file-noselect file)
        (setq count (1+ count))))
    (message (if (zerop count)
                 "No files opened"
               (format "%d file%s opened" count (if (> count 1) "s" ""))))))

;(provide 'jack-util)
