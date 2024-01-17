;;; elisp/dired-7z.el -*- lexical-binding: t; -*-

(defun dired-7z-get-archieve-default-name()
  (let* ((files (dired-get-marked-files))
         (fnum (length files))
         (thisfile (elt files 0))
         (dirname (file-name-nondirectory
                   (directory-file-name
                    (file-name-directory thisfile)))))
    (if (> fnum 1)
        (format "%s.7z" dirname)
      (format "%s.7z" (file-name-sans-extension (file-name-nondirectory thisfile))))))

(defun dired-7z-compress(filename)
  (interactive
   (list (read-from-minibuffer
          "Compress to: "
          (dired-7z-get-archieve-default-name))))
  (let ((files (dired-get-marked-files))
        (args ""))
    (dolist (file files)
      (setf args (format "%s %s" args (shell-quote-argument file))))
    (async-shell-command
     (format "7z a %s %s" (shell-quote-argument filename) args) "*7z-compress*")))

(defun dired-7z-extract(dirname)
  (interactive
   (list (read-from-minibuffer
          "Extract to: "
          (file-name-sans-extension (file-name-nondirectory (dired-get-filename))))))
  (let ((filename (dired-get-filename)))
    (if (> (length dirname))
        (async-shell-command
         (format "7z x %s -o%s"
                 (shell-quote-argument filename)
                 (shell-quote-argument dirname))
         "*7z-extract*")
      (async-shell-command
       (format "7z x %s" (shell-quote-argument filename)) "*7z-extract*"))))

(provide 'dired-7z)
;;; dired-7z.el ends here
