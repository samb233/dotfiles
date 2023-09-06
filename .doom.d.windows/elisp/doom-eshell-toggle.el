;;; elisp/doom-eshell-toggle.el -*- lexical-binding: t; -*-

(defun doom-eshell-toggle-project (arg &optional command)
  "Toggle eshell popup window in project root."
  (interactive "P")
  (let ((eshell-buffer
         (get-buffer-create
          (format "*doom:eshell-popup:%s*"
                  (if (bound-and-true-p persp-mode)
                      (safe-persp-name (get-current-persp))
                    "main"))))
        confirm-kill-processes
        current-prefix-arg)
    (when arg
      (when-let (win (get-buffer-window eshell-buffer))
        (delete-window win))
      (when (buffer-live-p eshell-buffer)
        (with-current-buffer eshell-buffer
          (fundamental-mode)
          (erase-buffer))))
    (if-let (win (get-buffer-window eshell-buffer))
        (let (confirm-kill-processes)
          (delete-window win)
          (ignore-errors (kill-buffer eshell-buffer)))
      (with-current-buffer eshell-buffer
        (setq-local default-directory (or (doom-project-root) default-directory))
        (doom-mark-buffer-as-real-h)
        (if (eq major-mode 'eshell-mode)
            (run-hooks 'eshell-mode-hook)
          (eshell-mode))
        (when command
          (+eshell-run-command command eshell-buffer)))
      (pop-to-buffer eshell-buffer))))

(provide 'doom-eshell-toggle)
;;; doom-eshell-toggle.el ends here
