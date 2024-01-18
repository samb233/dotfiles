;;; elisp/dired-windows-clipboard.el -*- lexical-binding: t; -*-

(defun dired-copy-file-to-windows-clipboard()
  (interactive)
  (if (w32-shell-execute "copy" (dired-get-filename))
      (message "Copied")
    (message "Copy failed")))

(defun dired-file-to-clipboard()
  (interactive)
  (let ((files (dired-get-marked-files))
        (args ""))
    (dolist (file files)
      (setf args (format "%s %s" args (file-name-nondirectory file))))
    (if (w32-shell-execute "open" "file2clip.exe" args 0)
        (message "Copied")
      (message "Copy failed"))))

(defun dired-paste-file-from-windows-clipboard()
  (interactive)
  (if (w32-shell-execute "paste" default-directory)
      (message "Pasted")
    (message "Paste failed")))

(defun dired-open-file-properties-windows()
  (interactive)
  (if (w32-shell-execute "properties" (dired-get-filename))
      (message "Open properties")
    (message "Open properties failed")))

(provide 'dired-windows-clipboard)
;;; dired-windows-clipboard.el ends here
