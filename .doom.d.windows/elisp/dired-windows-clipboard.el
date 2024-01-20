;;; elisp/dired-windows-clipboard.el -*- lexical-binding: t; -*-

(defun dired-copy-file-to-windows-clipboard()
  "Copy file to Windows clipboard using `w32-shell-execute'."
  (interactive)
  (if (w32-shell-execute "copy" (dired-get-filename))
      (message "Copied")
    (message "Copy failed")))

(defun dired-file-to-clipboard()
  "Copy marked files to Windows clipboard using file2clip.exe."
  (interactive)
  (let ((files (dired-get-marked-files))
        (args ""))
    (dolist (file files)
      (setf args (format "%s %s" args (file-name-nondirectory file))))
    (if (w32-shell-execute "open" "file2clip.exe" args 0)
        (message "Copied")
      (message "Copy failed"))))

(defun dired-paste-file-from-windows-clipboard()
  "Paste files to `default-directory' from Windows clipboard."
  (interactive)
  (if (w32-shell-execute "paste" default-directory)
      (message "Pasted")
    (message "Paste failed")))

(defun dired-open-file-properties-windows()
  "Open Windows properties of current file."
  (interactive)
  (if (w32-shell-execute "properties" (dired-get-filename))
      (message "Open properties")
    (message "Open properties failed")))

(provide 'dired-windows-clipboard)
;;; dired-windows-clipboard.el ends here
