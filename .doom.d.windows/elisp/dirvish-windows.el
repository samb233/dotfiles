;;; elisp/dirvish-windows.el -*- lexical-binding: t; -*-

;; use call-process-shell-command instead of  start-process on Windows
;; as the max process num is limit to 32 on windows
;; (defadvice! dirvish-find-entry-a-windows (&optional entry)
;;   :override #'dirvish-find-entry-a
;;   "Find ENTRY in current dirvish session.
;; ENTRY can be a filename or a string with format of
;; `dirvish-fd-bufname' used to query or create a `fd' result
;; buffer, it defaults to filename under the cursor when it is nil."
;;   (let* ((entry (or entry (dired-get-filename nil t)))
;;          (buffer (cond ((string-prefix-p "🔍" entry) (dirvish-fd-find entry))
;;                        ((file-directory-p entry) (dired-noselect entry))
;;                        ((string-suffix-p "/" entry)
;;                         (user-error
;;                          (concat entry " is not a valid directory"))))))
;;     (if buffer (switch-to-buffer buffer)
;;       (let* ((ext (downcase (or (file-name-extension entry) "")))
;;              (file (expand-file-name entry))
;;              (process-connection-type nil)
;;              (ex (cl-loop
;;                   for (exts . (cmd . args)) in dirvish-open-with-programs
;;                   thereis (and (not (dirvish-prop :remote))
;;                                (executable-find cmd)
;;                                (member ext exts)
;;                                (append (list cmd) args)))))
;;         (if ex (call-process-shell-command
;;                 (mapconcat #'identity
;;                            (cl-substitute
;;                             (shell-quote-argument file)
;;                             "%f" ex :test 'string=) " ")
;;                 nil 0)
;;           (let* ((dv (dirvish-curr)) (fn (nth 4 (dv-type dv))))
;;             (if fn (funcall fn) (dirvish-kill dv)))
;;           (find-file file))))))

(dirvish-define-preview eza (file)
  "Use `eza' to generate directory preview."
  :require ("eza")
  (when (file-directory-p file)
    `(shell . ("eza" "-al" "--color=always" "--icons=always" "--time-style" "long-iso"
               "--group-directories-first" ,file))))
(add-to-list 'dirvish-preview-dispatchers 'eza)

(dirvish-define-preview pdfinfo (file preview-window)
  "Preview epub files.
Require: `pdfinfo' (executable)"
  :require ("pdfinfo")
  (when (equal ext "pdf")
    `(shell . ("pdfinfo" ,file))))
(add-to-list 'dirvish-preview-dispatchers 'pdfinfo)

(dirvish-define-mode-line pathwin
  "Path of file under the cursor."
  (let* ((directory-abbrev-alist nil) ; TODO: support custom `directory-abbrev-alist'
         (index (dired-current-directory))
         (face (if (dirvish--window-selected-p dv) 'dired-header 'shadow))
         (rmt (dirvish-prop :remote))
         (abvname (if rmt (file-local-name index) (abbreviate-file-name index)))
         (host (propertize (if rmt (concat " " (substring rmt 1)) "")
                           'face 'font-lock-builtin-face))
         (segs (nbutlast (split-string abvname "/")))
         (scope (pcase (car segs)
                  ("~" (dirvish--register-path-seg
                        (nth 0 dirvish-path-separators)
                        (concat rmt "~/") face))
                  ("" (dirvish--register-path-seg
                       (nth 1 dirvish-path-separators)
                       (concat rmt "/") face))
                  ("c:" (dirvish--register-path-seg
                         "  C:"
                         (concat rmt "c:/") face))
                  ("d:" (dirvish--register-path-seg
                         "  D:"
                         (concat rmt "d:/") face))
                  ("e:" (dirvish--register-path-seg
                         "  E:"
                         (concat rmt "e:/") face))
                  ("f:" (dirvish--register-path-seg
                         "  F:"
                         (concat rmt "f:/") face))
                  ("g:" (dirvish--register-path-seg
                         "  G:"
                         (concat rmt "g:/") face))
                  ("h:" (dirvish--register-path-seg
                         "  H:"
                         (concat rmt "h:/") face))
                  ("i:" (dirvish--register-path-seg
                         "  I:"
                         (concat rmt "i:/") face))
                  ("j:" (dirvish--register-path-seg
                         "  J:"
                         (concat rmt "j:/") face))
                  ("k:" (dirvish--register-path-seg
                         "  K:"
                         (concat rmt "k:/") face))
                  ("l:" (dirvish--register-path-seg
                         "  L:"
                         (concat rmt "l:/") face))
                  ("m:" (dirvish--register-path-seg
                         "  M:"
                         (concat rmt "m:/") face))
                  ("n:" (dirvish--register-path-seg
                         "  N:"
                         (concat rmt "n:/") face))
                  ("o:" (dirvish--register-path-seg
                         "  O:"
                         (concat rmt "o:/") face))
                  ("p:" (dirvish--register-path-seg
                         "  P:"
                         (concat rmt "p:/") face))
                  ("q:" (dirvish--register-path-seg
                         "  Q:"
                         (concat rmt "q:/") face))
                  ("r:" (dirvish--register-path-seg
                         "  R:"
                         (concat rmt "r:/") face))
                  ("s:" (dirvish--register-path-seg
                         "  S:"
                         (concat rmt "s:/") face))
                  ("t:" (dirvish--register-path-seg
                         "  T:"
                         (concat rmt "t:/") face))
                  ("u:" (dirvish--register-path-seg
                         "  U:"
                         (concat rmt "u:/") face))
                  ("v:" (dirvish--register-path-seg
                         "  V:"
                         (concat rmt "v:/") face))
                  ("w:" (dirvish--register-path-seg
                         "  W:"
                         (concat rmt "w:/") face))
                  ("x:" (dirvish--register-path-seg
                         "  X:"
                         (concat rmt "x:/") face))
                  ("y:" (dirvish--register-path-seg
                         "  Y:"
                         (concat rmt "y:/") face))
                  ("z:" (dirvish--register-path-seg
                         "  Z:"
                         (concat rmt "z:/") face))))
         (path (cl-loop for idx from 2
                        for sp = (format
                                  "%s%s" (or rmt "")
                                  (mapconcat #'concat (seq-take segs idx) "/"))
                        for s in (cdr segs) concat
                        (format "%s%s" (nth 2 dirvish-path-separators)
                                (dirvish--register-path-seg s sp face)))))
    (replace-regexp-in-string "%" "%%%%" (format "%s%s%s " host scope path))))

(setq dirvish-header-line-format
      '(:left (pathwin) :right (yank sort index " ")))

(provide 'dirvish-windows)
;;; dirvish-windows.el ends here
