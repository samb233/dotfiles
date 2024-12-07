;;; elisp/doom-eshell-toggle.el -*- lexical-binding: t; -*-

(defun doom-eshell-toggle-project ()
  "Toggle eshell popup window in project root."
  (interactive)
  (let ((eshell-buffer
         (get-buffer-create
          (format "*doom:eshell-popup:%s*"
                  (or (doom-project-root) default-directory))))
        confirm-kill-processes)
    (if-let (win (get-buffer-window eshell-buffer))
        (let (confirm-kill-processes)
          (delete-window win))
      (with-current-buffer eshell-buffer
        (setq-local default-directory (or (doom-project-root) default-directory))
        (if (eq major-mode 'eshell-mode)
            (run-hooks 'eshell-mode-hook)
          (eshell-mode)))
      (pop-to-buffer eshell-buffer))))

(provide 'doom-eshell-toggle)
;;; doom-eshell-toggle.el ends here
