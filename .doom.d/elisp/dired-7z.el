;;; elisp/dired-7z.el -*- lexical-binding: t; -*-

(defun dired-7z-get-archieve-default-name()
  "Get default archieve file name."
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
  "Compress marked files to FILENAME using 7z."
  (interactive
   (list (read-from-minibuffer
          "Compress to: "
          (dired-7z-get-archieve-default-name))))
  (let ((files (dired-get-marked-files))
        (args ""))
    (dolist (file files)
      (setf args (format "%s %s" args (shell-quote-argument file))))
    (call-process-shell-command
     (format "7zG a %s %s" (shell-quote-argument filename) args) nil 0)))

(defun dired-7z-compress-with-password(filename)
  "Compress marked files to FILENAME using 7z, with password."
  (interactive
   (list (read-from-minibuffer
          "Compress to: "
          (dired-7z-get-archieve-default-name))))
  (let ((files (dired-get-marked-files))
        (args ""))
    (dolist (file files)
      (setf args (format "%s %s" args (shell-quote-argument file))))
    (call-process-shell-command
     (format "7zG a %s %s -p" (shell-quote-argument filename) args) nil 0)))

(defun dired-7z-extract(dirname)
  "Extract archieve file to DIRNAME."
  (interactive
   (list (read-from-minibuffer
          "Extract to: "
          (file-name-sans-extension (file-name-nondirectory (dired-get-filename))))))
  (let ((filename (dired-get-filename)))
    (if (> (length dirname))
        (call-process-shell-command
         (format "7zG x %s -o%s"
                 (shell-quote-argument filename)
                 (shell-quote-argument dirname))
         nil 0)
      (call-process-shell-command
       (format "7zG x %s" (shell-quote-argument filename)) nil 0))))

(provide 'dired-7z)
;;; dired-7z.el ends here
