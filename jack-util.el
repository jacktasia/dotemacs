
;;(jack-url-to-file "http://unarm.org" "/home/jack/unarm.txt")

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

;;
;; calls to command line programs...
;;
(defun jack-magic-lint ()
  (interactive)
  (save-buffer)
  (let* ((the_mode (symbol-name
					(with-current-buffer (current-buffer)
					  major-mode)))
		 (lang_cmds '(("php-mode" . "php -l %s")
					  ("python-mode" . "pep8 --ignore=W191 %s") ;; W191 = tab chars
					  ("js2-mode" .
					   "/home/jack/bin/jsl-0.3.0/src/Linux_All_DBG.OBJ/jsl -process  %s")))
		 (the_cmd (cdr (assoc the_mode lang_cmds))))
	(shell-command (format the_cmd (buffer-file-name)))))

(defun jack-git-blame-line () 
  (interactive) 
  (let ((cmd_tmpl "git blame -L %s,+3 %s") 
		(current_line (cadr (split-string (what-line) " "))))
	(shell-command (format cmd_tmpl current_line (buffer-file-name)))))

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

(defun jack-toggle-alpha ()
	(interactive)
	(let ((current_alpha (cadr (assoc 'alpha (frame-parameters)))))
		(if (= current_alpha 70)
			(modify-frame-parameters nil '((alpha 100)))
			(modify-frame-parameters nil '((alpha 70))))))

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
	(when (string-equal system-type "gnu/linux")
		(shell-command "wmctrl -r :ACTIVE: -b toggle,maximized_vert,maximized_horz")))

;;
;; lang mood hook setups...
;;

(defun jack-php-setup ()
  (local-set-key (kbd "TAB") 'self-insert-command)
  (local-set-key (kbd "{") 'self-insert-command) ; support for "{" needed
  (local-set-key (kbd "}") 'self-insert-command)
  (local-set-key (kbd ":") 'self-insert-command)
  (local-set-key (kbd ";") 'self-insert-command))

(provide 'jack-util)
