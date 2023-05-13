;;; elisp/tab-bar-helper.el -*- lexical-binding: t; -*-

(defun tab-bar-new-tab-with-name (name)
  "Create the NAME tab if it doesn't exist already."
  (interactive (list (read-from-minibuffer "New tab name: ")))
  (if (not name)
      (tab-bar-new-tab)
    (progn (tab-bar-new-tab)
           (tab-bar-rename-tab name)))
  (message "Create new tab %s" name))

(provide 'tab-bar-helper)
;;; tab-bar-helper.el ends here
