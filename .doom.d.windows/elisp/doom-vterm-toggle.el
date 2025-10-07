;;; elisp/doom-vterm-toggle.el -*- lexical-binding: t; -*-
;;
;; fork of doomemacs's vterm-toggle:
;; https://github.com/doomemacs/doomemacs/blob/master/modules/term/vterm/autoload.el
;; By default, doom's +vterm/toggle toggle a vterm per workspace.
;; I want it to be one vterm per directory.

(defvar doom-vterm--id nil)

(defun doom-vterm-toggle-directory ()
  "Toggles a terminal popup window at current directory.
If prefix ARG is non-nil, recreate vterm buffer in the current directory.
Returns the vterm buffer."
  (interactive)
  (doom-vterm--configure-project-root-and-display
   1
   (lambda ()
     (let ((buffer-name
            (format "*doom:vterm-popup:%s*" (file-truename default-directory)))
           confirm-kill-processes
           current-prefix-arg)
       (if-let (win (get-buffer-window buffer-name))
           (delete-window win)
         (let ((buffer (or (cl-loop for buf in (doom-buffers-in-mode 'vterm-mode)
                                    if (equal (buffer-local-value 'doom-vterm--id buf)
                                              buffer-name)
                                    return buf)
                           (get-buffer-create buffer-name))))
           (pop-to-buffer buffer)
           (with-current-buffer buffer
             (unless (eq major-mode 'vterm-mode)
               (vterm-mode))
             (setq-local doom-vterm--id buffer-name)
             (setq-local vterm-buffer-name-string nil))))
       (get-buffer buffer-name)))))

(defun doom-vterm-toggle-project ()
  "Toggles a terminal popup window at current directory.
If prefix ARG is non-nil, recreate vterm buffer in the current directory.
Returns the vterm buffer."
  (interactive)
  (setq-local project-root (or (doom-project-root) default-directory))
  (doom-vterm--configure-project-root-and-display
   nil
   (lambda ()
     (let ((buffer-name
            (format "*doom:vterm-popup:%s*" (file-truename project-root)))
           confirm-kill-processes
           current-prefix-arg)
       (if-let (win (get-buffer-window buffer-name))
           (delete-window win)
         (let ((buffer (or (cl-loop for buf in (doom-buffers-in-mode 'vterm-mode)
                                    if (equal (buffer-local-value 'doom-vterm--id buf)
                                              buffer-name)
                                    return buf)
                           (get-buffer-create buffer-name))))
           (pop-to-buffer buffer)
           (with-current-buffer buffer
             (unless (eq major-mode 'vterm-mode)
               (vterm-mode))
             (setq-local doom-vterm--id buffer-name)
             (setq-local vterm-buffer-name-string nil))))))))

(defun doom-vterm--configure-project-root-and-display (arg display-fn)
  "Sets the environment variable PROOT and displays a terminal using `display-fn`.
If prefix ARG is non-nil, cd into `default-directory' instead of project root.
Returns the vterm buffer."
  (unless (fboundp 'module-load)
    (user-error "Your build of Emacs lacks dynamic modules support and cannot load vterm"))
  (let* ((project-root (or (doom-project-root) default-directory))
         (default-directory
          (if arg
              default-directory
            project-root)))
    (setenv "PROOT" project-root)
    (funcall display-fn)))

(provide 'doom-vterm-toggle)
;;; doom-vterm-toggle.el ends here
