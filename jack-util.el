
; because i override tilda/backtick for kupfer, but need backticks a lot for markdown
(defun jack-insert-backtick ()
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
  (let ((cmd_tmpl "git diff %s"))
	(shell-command (format cmd_tmpl (buffer-file-name)))
	(with-current-buffer (get-buffer "*Shell Command Output*")
		(diff-mode))))

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

(defun jack-php-key-to-fetch (start end)
	(interactive "r")
	(save-excursion
		(save-restriction
			(narrow-to-region (point-min) end)
			(goto-char start)
			(insert "fetch(")
			(replace-string "[" ", ")
			(replace-string "]" ")"))))

(defun jack-emacs-maximize ()
	(cond
		((string-equal system-type "darwin")
			(set-frame-size (selected-frame)  235 70))
		((string-equal system-type "gnu/linux")
			(shell-command "wmctrl -r :ACTIVE: -b toggle,maximized_vert,maximized_horz"))
		(t
			(message "%s" "Did not try to maximize screen. System type not supported."))))

;; allows you to highligh a word regardless of position within
(defun jack-mark-word ()
	(interactive)
	(forward-word)
	(backward-word)
	(mark-word))

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


;(provide 'jack-util)
