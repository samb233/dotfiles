;;; elisp/dirvish-windows.el -*- lexical-binding: t; -*-

(dirvish-define-preview mtn (file ext preview-window)
  "Preview video files.
Require: `mtn' (executable)"
  :require ("mtn")
  (when (member ext dirvish-video-exts)
    (let* ((width (dirvish-media--img-size preview-window))
           (height (dirvish-media--img-size preview-window 'height))
           (cache (dirvish-media--cache-path file (format "images/%s" width) ".jpg"))
           (path (dirvish--get-parent-path cache)))
      (if (file-exists-p cache)
          `(img . ,(create-image cache nil nil :max-width width :max-height height))
        `(cache . ("mtn" "-P" "-i" "-c" "1" "-r" "1" "-O" ,path ,file "-o"
                   ,(format ".%s.jpg" ext) "-w"
                   ,(number-to-string width)))))))
(add-to-list 'dirvish-preview-dispatchers 'mtn)

(dirvish-define-preview ls (file)
  "Use `ls' to generate directory preview."
  :require ("ls")
  (when (file-directory-p file)
    `(shell . ("ls" "-l" "--almost-all" "--human-readable"
               "--group-directories-first" "--no-group" "--time-style=iso",file))))
(add-to-list 'dirvish-preview-dispatchers 'ls)

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
