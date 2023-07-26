;;; elisp/doom-lookup-other-window.el -*- lexical-binding: t; -*-

;; https://github.com/doomemacs/doomemacs/issues/3397
;; (dolist (fn '(definition references))
;;   (fset (intern (format "+lookup/%s-other-window" fn))
;;         (lambda (identifier &optional arg)
;;           "lookup informations in other window"
;;           (interactive (list (doom-thing-at-point-or-region)
;;                              current-prefix-arg))
;;           (let ((pt (point)))
;;             (switch-to-buffer-other-window (current-buffer))
;;             (goto-char pt)
;;             (funcall (intern (format "+lookup/%s" fn)) identifier arg)))))

(defun +lookup/definition-other-window (identifier &optional arg)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point) in
other window"
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
   (let ((pt (point)))
            (switch-to-buffer-other-window (current-buffer))
            (goto-char pt)
            (funcall #'+lookup/definition identifier arg)))

(defun +lookup/references-other-window (identifier &optional arg)
  "Jump to the reference of IDENTIFIER (defaults to the symbol at point) in
other window"
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
   (let ((pt (point)))
            (switch-to-buffer-other-window (current-buffer))
            (goto-char pt)
            (funcall #'+lookup/references identifier arg)))

(provide 'doom-lookup-other-window)
;;; doom-looup-other-window.el ends here
