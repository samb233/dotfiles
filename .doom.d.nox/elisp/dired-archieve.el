;;; elisp/dired-archieve.el -*- lexical-binding: t; -*-

(defun dired-archieve-add()
  "Compress marked files to FILENAME using file-roller."
  (interactive)
  (let ((files (dired-get-marked-files))
        (args ""))
    (dolist (file files)
      (setf args (format "%s %s" args (shell-quote-argument file))))
    (call-process-shell-command
     (format "file-roller -d %s" args) nil 0)))

(defun dired-archieve-extract(dirname)
  "Extract archieve file to DIRNAME."
  (interactive
   (list (read-from-minibuffer
          "Extract to: "
          (file-name-sans-extension (file-name-nondirectory (dired-get-filename))))))
  (let ((filename (dired-get-filename)))
    (if (> (length dirname))
        (call-process-shell-command
         (format "file-roller -e %s %s"
                 (shell-quote-argument dirname)
                 (shell-quote-argument filename))
         nil 0)
      (call-process-shell-command
       (format "file-roller -h %s" (shell-quote-argument filename)) nil 0))))

(provide 'dired-archieve)
;;; dired-archieve.el ends here
