;;; elisp/doom-light-modeline-enhance.el -*- lexical-binding: t; -*-

(def-modeline-var! +modeline-tab-name
                   '(:eval (when-let*
                       ((name (cond
                               ((and (fboundp 'tab-bar-mode)
                                     (length> (frame-parameter nil 'tabs) 1))
                                (let* ((current-tab (tab-bar--current-tab))
                                       (tab-index (tab-bar--current-tab-index))
                                       (explicit-name (alist-get 'explicit-name current-tab))
                                       (tab-name (alist-get 'name current-tab)))
                                  (if explicit-name tab-name (+ 1 tab-index)))))))
                     (unless (string-empty-p name)
                       (propertize (format " %s " name)
                                   'face 'bold)))))

(def-modeline-var! +modeline-flymake
                   '(:eval (flymake--mode-line-counters)))


(def-modeline! :custom
               '(" "
                 +modeline-tab-name
                 +modeline-matches
                 " "
                 +modeline-buffer-identification
                 " "
                 +modeline-flymake
                 +modeline-position)
               `(""
                 mode-line-misc-info
                 +modeline-modes
                 (vc-mode (" "
                           ,(nerd-icons-octicon "nf-oct-git_branch" :v-adjust 0.0)
                           vc-mode " "))
                 " "
                 +modeline-encoding
                 "  "))

(set-modeline! :custom 'default)
(provide 'doom-light-modeline-enhance)
;;; doom-light-modeline-enhance.el ends here
