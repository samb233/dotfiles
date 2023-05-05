;;; elisp/doom-vterm-toggle.el -*- lexical-binding: t; -*-
;;
;; fork of doomemacs's vterm-toggle:
;; https://github.com/doomemacs/doomemacs/blob/master/modules/term/vterm/autoload.el
;; By default, doom's +vterm/toggle toggle a vterm per workspace.
;; I want it to be one vterm per directory.

(defvar doom-vterm--id nil)

(defun doom-vterm-toggle-directory (arg)
  "Toggles a terminal popup window at current directory.

If prefix ARG is non-nil, recreate vterm buffer in the current directory.

Returns the vterm buffer."
  (interactive "P")
  (doom-vterm--configure-project-root-and-display
   1
   (lambda ()
     (let ((buffer-name
            (format "*doom:vterm-popup:%s*"
                    (if (bound-and-true-p persp-mode)
                        (safe-persp-name (get-current-persp))
                      default-directory)))
           confirm-kill-processes
           current-prefix-arg)
       (when arg
         (let ((buffer (get-buffer buffer-name))
               (window (get-buffer-window buffer-name)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (when (window-live-p window)
             (delete-window window))))
       (if-let (win (get-buffer-window buffer-name))
           (delete-window win)
         (let ((buffer (or (cl-loop for buf in (doom-buffers-in-mode 'vterm-mode)
                                    if (equal (buffer-local-value 'doom-vterm--id buf)
                                              buffer-name)
                                    return buf)
                           (get-buffer-create buffer-name))))
           (with-current-buffer buffer
             (setq-local doom-vterm--id buffer-name)
             (unless (eq major-mode 'vterm-mode)
               (vterm-mode)))
           ;; HACK forces vterm to redraw, fixing strange artefacting in the tty.
           (save-window-excursion
             (pop-to-buffer "*scratch*"))
           (pop-to-buffer buffer)))
       (get-buffer buffer-name)))))

(defun doom-vterm-toggle-project (arg)
  "Toggles a terminal popup window at current directory.

If prefix ARG is non-nil, recreate vterm buffer in the current directory.

Returns the vterm buffer."
  (interactive "P")
  (setq-local project-root (or (doom-project-root) default-directory))
  (doom-vterm--configure-project-root-and-display
   nil
   (lambda ()
     (let ((buffer-name
            (format "*doom:vterm-popup:%s*"
                    (if (bound-and-true-p persp-mode)
                        (safe-persp-name (get-current-persp))
                      project-root)))
           confirm-kill-processes
           current-prefix-arg)
       (when arg
         (let ((buffer (get-buffer buffer-name))
               (window (get-buffer-window buffer-name)))
           (when (buffer-live-p buffer)
             (kill-buffer buffer))
           (when (window-live-p window)
             (delete-window window))))
       (if-let (win (get-buffer-window buffer-name))
           (delete-window win)
         (let ((buffer (or (cl-loop for buf in (doom-buffers-in-mode 'vterm-mode)
                                    if (equal (buffer-local-value 'doom-vterm--id buf)
                                              buffer-name)
                                    return buf)
                           (get-buffer-create buffer-name))))
           (with-current-buffer buffer
             (setq-local doom-vterm--id buffer-name)
             (unless (eq major-mode 'vterm-mode)
               (vterm-mode)))
           ;; HACK forces vterm to redraw, fixing strange artefacting in the tty.
           (save-window-excursion
             (pop-to-buffer "*scratch*"))
           (pop-to-buffer buffer)))
       (get-buffer buffer-name)))))

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
