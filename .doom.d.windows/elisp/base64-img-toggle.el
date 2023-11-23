;;; elisp/base64-img-toggle.el -*- lexical-binding: t; -*-

(defun base64-img-toggle-region (beg end)
  "Decode a base64 string to a image and toggle."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (error "No selection (no active region)")))
  (let* ((content (buffer-substring beg end))
         (base64-pos (string-match-p "base64," content))
         (base64-beg (if base64-pos (+ 7 base64-pos) 0))
         (base64 (substring content base64-beg nil))
         (img (base64-decode-string base64))
         (img-buffer (get-buffer-create "*base64-img-toggle*")))
    (when (buffer-live-p img-buffer)
        (with-current-buffer img-buffer
          (fundamental-mode)
          (erase-buffer)))
    (if-let (win (get-buffer-window img-buffer))
        (delete-window win))
    (with-current-buffer img-buffer
      (insert img)
      (if (eq major-mode 'image-mode)
            (run-hooks 'image-mode-hook)
          (image-mode))
      (image-increase-size 10))
    (pop-to-buffer img-buffer)))

(provide 'base64-img-toggle)
;;; base64-img-toggle.el ends here
