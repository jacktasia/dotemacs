
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

;; http://stackoverflow.com/a/6133921/24998
;; don't want to add delete word to the kill-ring
(defun jack-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

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
    (when (y-or-n-p "Kill all other buffers? ")
      (mapc 'kill-buffer (buffer-list))))

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

; based off of http://stackoverflow.com/a/10364547/24998
(defun jack-new-scratch ()
  "open up a new scratch buffer"
  (interactive)
  (switch-to-buffer (generate-new-buffer "*scratch*")))

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
		(current_line (cadr (split-string (what-line) " "))))
	(shell-command (format cmd_tmpl current_line (buffer-file-name)))))

(defun jack-git-diff ()
  (interactive)
  (let* ((cmd_tmpl "git diff %s")
         (git_diff_cmd (format cmd_tmpl (buffer-file-name)))
         (file_name (car (last (split-string (buffer-file-name) "/"))))
         (hunk-re "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@")
         (buffer_title (concat "DIFF " file_name " on " (format-time-string "%D @ %H:%M:%S"))))
    (shell-command git_diff_cmd)
    (with-current-buffer (get-buffer "*Shell Command Output*")
      (rename-buffer buffer_title)
      (diff-mode)
      (while (re-search-forward hunk-re nil t)
        (funcall-interactively 'diff-refine-hunk)))))

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
  (let ((opened-files (delq nil (mapcar 'buffer-file-name (buffer-list))))
        (files (pmdm--read-files-list))
        (count 0))
    (dolist (file files)
      (unless (member file opened-files)
        (find-file-noselect file)
        (setq count (1+ count))))
    (message (if (zerop count)
                 "No files opened"
               (format "%d file%s opened" count (if (> count 1) "s" ""))))))


;(provide 'jack-util)
