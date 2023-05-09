;;; emacs/undo/config.el -*- lexical-binding: t; -*-

;;
;;; undo


(use-package! undo-fu-session
  :hook (doom-first-buffer . global-undo-fu-session-mode)
  :custom (undo-fu-session-directory (concat doom-cache-dir "undo-fu-session/"))
  :config
  (setq undo-limit 400000           ; 400kb (default is 160kb)
        undo-strong-limit 3000000   ; 3mb   (default is 240kb)
        undo-outer-limit 48000000)  ; 48mb  (default is 24mb)
  (setq undo-fu-session-incompatible-files '("\\.gpg$" "/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (when (executable-find "zstd")
    ;; There are other algorithms available, but zstd is the fastest, and speed
    ;; is our priority within Emacs
    (setq undo-fu-session-compression 'zst))

  ;; HACK Fix #4993: we've advised `make-backup-file-name-1' to produced SHA1'ed
  ;;      filenames to prevent file paths that are too long, so we force
  ;;      `undo-fu-session--make-file-name' to use it instead of its own
  ;;      home-grown overly-long-filename generator.
  ;; TODO PR this upstream; should be a universal issue
  (defadvice! +undo-fu-make-hashed-session-file-name-a (file)
    :override #'undo-fu-session--make-file-name
    (concat (let ((backup-directory-alist `(("." . ,undo-fu-session-directory))))
              (make-backup-file-name-1 file))
            (undo-fu-session--file-name-ext))))


(use-package! vundo
  :defer t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols
        vundo-compact-display t)
  (define-key vundo-mode-map [remap doom/escape] #'vundo-quit))
