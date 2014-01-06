(defun keystroke-to-cmd-run (k cmd)
	(global-set-key k (lambda () (interactive) (shell-command cmd))))

;; (keystroke-to-cmd-run "\C-c9" "osascript -e 'tell application \"Terminal\" to display dialog \"test\"'")


;;; experimental changes

(define-key global-map (kbd "C-s") 'ace-jump-char-mode)
(define-key global-map (kbd "C-c s") 'isearch-forward)

(provide 'jack-scratch)
