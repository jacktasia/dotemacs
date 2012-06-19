;; #1 new cmdj-read-from-bigbuffer refactoring
;; #2 php arg tooltip support for project functions

 ;; ((lambda (x)
 ;; 	 (message "%s" x)) "hello world")
;; (setq debug-on-error 1)

;; TODO: bigger input, mini-buffer too small
;; TODO: make it so that other languages can be added widh simole add-to-list? calls
;; TODO: php tooltip thing should work on user defined functions too
;; TODO: if searching inside *scratch* or the like default to elisp files (defun) AND of of course the default propteries. EXTRA: have global list to add-to to define the default lang type and/or projpath
;; TODO: comments about blocks, def customs, interactive (i prefix?), -p, etc.
;; TODO: collapse/expand functions (php and elisp)!!!
;; TODO: git diff (of current) file in own buffer!!
;; TODO: ack-grep wrapper...
;; TODO: make a utitly function that just gives a long list of utilties to run for # to press C-. "u"
;; add-to-list support ?

;;
;; START DEF CUSTOMS
;;

(setq cmdj-debug-mode t)
(setq cmdj-default-project "/home/jack/public_html")

(defcustom cmdj-ack-path "/usr/bin/ack-grep"
  "The path to ack-grep, for installing see http://betterthangrep.com"
  :type 'string
  :group 'cmdj)

(defcustom cmdj-debug-mode nil
  "should debug mode be on"
  :type 'string
  :group 'cmdj)

(defcustom cmdj-cache-file "~/.cmdj_cache"
  "File for cache stuff"
  :type 'string
  :group 'cmdj)

(defcustom cmdj-project-denoter ".project"
  "The file to recongize a folder as a 'project' folder (stops "
  :type 'string
  :group 'cmdj)

(defcustom cmdj-default-project "~"
  "The default project to search code (your 'Projects' folder in ~)"
  :type 'string
  :group 'cmdj)

(defun cmdj-debug-out (out-text) ;; optional non default to message
  (when cmdj-debug-mode
	(message "CMDJ: %s" out-text)))

(defun cmdj-read-from-bigbuffer (buf_title buf_prompt buf_content)
	(if (get-buffer buf_title)
		(switch-to-buffer buf_title)
		(create-file-buffer buf_title))
	(with-current-buffer buf_title
		(erase-buffer)
		(insert (format "%s\n\n" buf_content)))
	(switch-to-buffer buf_title)
	(car (list (read-from-minibuffer buf_prompt) (kill-buffer buf_title))))

(defun cmdj-get-nearest-project ()
  "returns string path of nearest project or default"
  (if (buffer-file-name)
	  (progn
		(let* ((path_list (split-string (buffer-file-name) "/"))
			   (path_root (butlast path_list))
			   (proj_root nil))

		  (while (and (null proj_root)
					  (not (null path_root)))
			(let* ((tmp_root (mapconcat 'identity path_root "/"))
				   (tmp_check (concat tmp_root "/" cmdj-project-denoter)))
			  (if (file-exists-p tmp_check)
				  (set 'proj_root tmp_root)
				(set 'path_root (butlast path_root)))))
		  (if (null proj_root)
			  (progn
				(message "no %s found so using %s" cmdj-project-denoter cmdj-default-project)
				cmdj-default-project)
			(concat proj_root ""))))
	cmdj-default-project))

(defun cmdj-dump-config-file (our_var filename)
  "dump var to file"
  (save-excursion
	(let ((buf (find-file-noselect filename)))
	  (set-buffer buf)
	  (erase-buffer)
	  (print our_var buf)
	  (save-buffer)
	  (kill-buffer))))

(defun cmdj-read-config-file (filename)
  "read var from file"
  (let ((buf (find-file-noselect filename)))
	(with-current-buffer buf
	  (read (buffer-string)))))

(defun cmdj-url-get-contents (url)
  (with-current-buffer (url-retrieve-synchronously url)
	(buffer-string)))

(defun cmdj-cache-get-load ()
  (if (file-exists-p cmdj-cache-file)
	  (cmdj-read-config-file cmdj-cache-file)
	'()))

(defun cmdj-cache-get (cache_key)
  (let ((cache_load (cmdj-cache-get-load)))
	(cmdj-debug-out (format "got %s from cache!" cache_key))
	(cdr (assoc cache_key cache_load))))

(defun cmdj-cache-put (cache_key cache_value)
  (let ((cache_load (cmdj-cache-get-load)))
	(push (cons cache_key cache_value) cache_load)
	(message "%s" cache_load)
	(cmdj-dump-config-file cache_load cmdj-cache-file)))

;;
;; END CMDJ CORE STUFF
;;

;;; ACK-GREP FUNCTIONS (INTERACTIVE)
(defun cmdj-func-at-point ()
  (interactive)
  (let ((search_str (thing-at-point 'symbol)))
	(cmdj-func-in-project search_str)))

(defun cmdj-search-at-point ()
  (interactive)
  (let ((search_str (thing-at-point 'symbol)))
	(cmdj-search-in-project search_str)))

(defun cmdj-search-prompt ()
  (interactive)
  (let* ((search_str (read-from-minibuffer "What should we search? "))
		 (cmd_struct "%s -a '%s' %s")
		 (cmd (format cmd_struct cmdj-ack-path search_str (cmdj-get-nearest-project))))
	(cmdj-search-results cmd)))

(defun cmdj-search-in-project (thesymbol)
  (let* ((cmd_struct "%s -a '%s' %s")
		 (cmd (format cmd_struct cmdj-ack-path thesymbol (cmdj-get-nearest-project))))
	(cmdj-search-results cmd)))

;; NON-INTERACTIVE

(defun cmdj-func-in-project (thesymbol)
  (if (buffer-file-name)
	  (cmdj-search-results (cmdj-func-in-project-cmd thesymbol))
	(if (y-or-n-p "Cannot search func (not in file), normal search? ")
		(cmdj-search-in-project thesymbol))))

(defun cmdj-func-in-project-cmd (thesymbol)
  (let* ((file_type '(("php" . "function ")
					  ("el" . "defun ")
					  (nil . "")))
		 (file_ext (car (last (split-string buffer-file-name "\\."))))
		 (lang_func (cdr (assoc file_ext file_type)))
		 (cmd_struct "%s --%s '%s%s\\s?\\(' %s"))
	(format cmd_struct cmdj-ack-path file_ext lang_func thesymbol (cmdj-get-nearest-project))))

(defun cmdj-search-data (search_str)
  (let* ((ack_response (shell-command-to-string search_str))
		 (result_lines (split-string ack_response "\\\n"))
		 (result_lines (remove (car (last result_lines)) result_lines)))
	(mapcar
	 (lambda (item)
	   (let ((item_parts (split-string item ":")))
		 (list (car item_parts)
			   (cadr item_parts))))
	 result_lines)))

(defun cmdj-search-results (search_str) 
  (let ((found_stuff (cmdj-search-data search_str)))
	(if (= 1 (length found_stuff)) 
		(cmdj-goto-file-location (caar found_stuff) 
							(cadar found_stuff)) 
	  (cmdj-file-pick found_stuff))))

(defun cmdj-search-results-old (search_str)
  (let* ((ack_response (shell-command-to-string search_str))
		 (result_lines (split-string ack_response "\\\n"))
		 (result_lines (remove (car (last result_lines)) result_lines))
		 (found_stuff (mapcar
					   (lambda (item)
						 (let ((item_parts (split-string item ":")))
						   (list (car item_parts)
								 (cadr item_parts))))
					   result_lines)))
	(if (= 1 (length found_stuff))
		(cmdj-goto-file-location (caar found_stuff)
							(cadar found_stuff))
	  (cmdj-file-pick found_stuff))))

(defun cmdj-goto-file-location (thefile theline)
  (find-file thefile)
  (goto-line (string-to-number theline)))

(defun cmdj-file-pick (fileinfo)
  ;; TODO: make below into something like (defun cmdj-build-grep-results
  (let* ((counter 0)
		 (base_prompt_str (mapcar
						   (lambda (item)
							 (format "%d) %s | Line %s" (incf counter)
									 (car item)
									 (cadr item)))
						   fileinfo))
		 (prompt_str (concat (format "%d matches found\n\n" counter)
							 (mapconcat 'identity base_prompt_str "\n") "\n\nOpen #: "))
		 (user_pick (1- (string-to-number (read-from-minibuffer prompt_str)))))
	(when (<= user_pick (length fileinfo))
	  (cmdj-goto-file-location (car (nth user_pick fileinfo))
						  (cadr (nth user_pick fileinfo))))))

;;
;; PHP ARG STUFF
;;


(defun cmdj-php-func-args-tooltip ()
  (interactive)
  (tooltip-show (cmdj-php-args-func-search (thing-at-point 'symbol))))

(defun cmdj-php-args-func-search (php_func)
  "1) local project, 2) web cache 3) web docs"
  (let ((display_args nil));;(cmdj-php-cache-get php_func)))
	(unless display_args
	  (setq display_args (cmdj-php-web-get php_func)))
	(if display_args
		(format	"\n\t%s (%s)\t\n \n" php_func display_args)
	  (format "%s is not a valid function" php_func))))

(defun cmdj-php-cache-get (php_func)
  (cmdj-cache-get (concat "php_args" php_func)))

(defun cmdj-php-cache-put (php_func display_args)
  (cmdj-cache-put (concat "php_args" php_func) display_args))

(defun cmdj-php-web-get (php_func)
  ;; TODO: use string-match-p and split-string to look for [, so can show if optional args
  (let ((php_html_contents (cmdj-url-get-contents (concat "http://us.php.net/" php_func))))
	;; rdfs-comment is a str we know is on every php function doc page
	(when (string-match-p (regexp-quote "rdfs-comment") php_html_contents)
	  (let ((display_args nil)
			(raw_params (car (split-string php_html_contents (regexp-quote "rdfs-comment"))))
			(param_pattern "code class=\"parameter\">\\([^<]+\\)</code>")
			(php_args '()))
		(with-temp-buffer
		  (insert raw_params)
		  (goto-char 1)
		  (while (re-search-forward param_pattern nil t)
			(when (match-string 0) ;; Got a match
			  (cmdj-debug-out (format "got %s from web docs!" php_func))
			  (setq php_args (cons (match-string 1) php_args)))))
		(setq display_args (mapconcat 'identity (reverse php_args) ", "))
		(cmdj-php-cache-put php_func display_args)
		display_args))))

;; ELISP STUFF

(defun cmdj-reload-elisp-file ()
	(interactive)
	(load-file buffer-file-name))

;; TODO: wrap in func to load default keymap
;; (define-prefix-command 'cmdj-key-map)
;; (global-set-key (kbd "C-.") 'cmdj-key-map)

;; (define-key cmdj-key-map (kbd "g") 'cmdj-func-at-point)
;; (define-key cmdj-key-map (kbd "s") 'cmdj-search-at-point)
;; (define-key cmdj-key-map (kbd "d") 'cmdj-search-prompt)
;; (define-key cmdj-key-map (kbd "a") 'cmdj-php-func-args-tooltip)
;; (define-key cmdj-key-map (kbd "r") 'cmdj-reload-elisp-file)


(provide 'cmdj)

