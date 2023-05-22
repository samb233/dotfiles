(setq user-full-name "Jie Samb"
      user-mail-address "samb233@hotmail.com")

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(pushnew! default-frame-alist '(width . 80) '(height . 50))
(toggle-frame-maximized)

;; (add-to-list 'default-frame-alist '(alpha-background . 95))
;; (add-to-list 'default-frame-alist (cons 'alpha 90))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat (format "emacs@%s: " (system-name))
                           (abbreviate-file-name (buffer-file-name)))
                 (format "emacs@%s" (system-name))))))

(remove-hook! 'doom-after-init-hook #'doom-display-benchmark-h)

(setq auth-source-save-behavior nil)

(setq doom-font (font-spec :family "Iosevka" :weight 'medium :size 13.0))

(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "Sarasa Mono SC"))))

(add-hook 'after-setting-font-hook #'my-cjk-font)

(setq doom-theme 'doom-tomorrow-day)

(after! doom-modeline
  (setq doom-modeline-modal nil
        doom-modeline-buffer-encoding t
        doom-modeline-vcs-max-length 20
        doom-modeline-height 30
        doom-modeline-workspace-name nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-buffer-state-icon nil)
  (set-face-attribute 'mode-line-active nil :background "#f4f4f4"))

(setq uniquify-buffer-name-style 'forward)

(custom-set-faces
 '(line-number ((t (:weight medium :slant unspecified))))
 '(line-number-current-line ((t (:weight medium :slant unspecified)))))

(setq scroll-margin 6)
(setq mouse-wheel-scroll-amount '
      (3
       ((shift) . hscroll)
       ((meta))
       ((control) . text-scale)))
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse t) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; (pixel-scroll-precision-mode t)

(evil-define-key 'visual 'global
  (kbd "J") 'drag-stuff-down
  (kbd "K") 'drag-stuff-up)

(map! :n "<mouse-8>" #'better-jumper-jump-backward
      :n "<mouse-9>" #'better-jumper-jump-forward)

(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "Q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)
(evil-ex-define-cmd "W" 'save-buffer)

(evil-define-key 'normal 'global (kbd "C-s") 'consult-line)
(map! "C-s" #'consult-line)

(map! "C-v" #'yank)
(map! "M-v" #'yank)

(evil-define-key 'insert 'global (kbd "C-z") 'undo-only)
(evil-define-key 'normal 'global (kbd "C-z") 'undo-only)
(evil-define-key 'insert 'global (kbd "C-S-z") 'undo-redo)
(evil-define-key 'normal 'global (kbd "C-S-z") 'undo-redo)
(evil-define-key 'normal 'global (kbd "U") 'evil-redo)

(evil-define-key 'normal 'global (kbd "g a") 'avy-goto-char-2)

(evil-define-key 'normal 'global (kbd "] e") 'flymake-goto-next-error)
(evil-define-key 'normal 'global (kbd "[ e") 'flymake-goto-prev-error)

(map! :leader
      :desc "format buffer" "b f" #'+format/buffer
      :desc "toggle format-all" "t f" #'format-all-mode)

(map! :leader
      :desc "bookmark list" "b w" #'list-bookmarks
      :desc "bookmark jump other window" "b o" #'bookmark-jump-other-window)

(evil-define-key 'normal 'global (kbd "g D") 'xref-find-definitions-other-window)

(map! :leader
      "i e" nil
      "f c" nil
      "n d" nil
      "f e" nil
      "f E" nil
      "f p" nil
      "f P" nil
      "s e" nil
      "s t" nil)

(setq undo-no-redo t)
(setq evil-want-fine-undo t)
(setq evil-undo-system 'undo-redo
      evil-undo-function 'undo-only
      evil-redo-function 'undo-redo)

(after! projectile
  (add-to-list 'projectile-project-root-files "go.mod")
  (setq projectile-project-root-functions '(projectile-root-local
                                            projectile-root-marked
                                            projectile-root-top-down
                                            projectile-root-bottom-up
                                            projectile-root-top-down-recurring)))

(defun project-projectile (dir)
  "Return Projectile project of form ('projectile . root-dir) for DIR."
  (let ((root (projectile-project-root dir)))
    (when root
      (cons 'projectile root))))
(setq project-find-functions '(project-projectile project-try-vc))

(after! recentf
  (setq recentf-max-saved-items 1000))

(setq magit-clone-default-directory "~/Codes/Lab/")

(setq eglot-workspace-configuration '(:gopls (:usePlaceholders t)))

(after! eglot
  (setq eglot-events-buffer-size 0)
  (setq eglot-stay-out-of nil)
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (map! :leader
        :desc "LSP start/restart" "c S" #'eglot
        :desc "LSP reconnect" "c R" #'eglot-reconnect
        :desc "LSP rename" "c n" #'eglot-rename
        :desc "Jump to references" "c r" #'+lookup/references)
  (set-popup-rule! "^\\*eglot-help" :size 0.3 :quit t :select nil)
  (set-face-attribute 'eglot-highlight-symbol-face nil :background "#d6d4d4"))

(after! corfu
  (setq corfu-preview-current nil
        corfu-popupinfo-delay nil
        corfu-on-exact-match nil
        corfu-auto-prefix 2
        corfu-auto-delay 0.1
        corfu-popupinfo-max-height 20
        corfu-count 10
        cape-dict-file "~/.doom.d/dict/words")
  (map! :map corfu-map
        :i "C-j" #'corfu-next
        :i "C-k" #'corfu-previous
        :i "C-l" #'corfu-insert-separator
        :i "C-i" #'corfu-info-documentation
        :i "C-g" #'corfu-quit)
  (map! :i "C-S-p" #'cape-file)
  (add-hook! 'evil-insert-state-exit-hook #'corfu-quit)
  (set-face-attribute 'corfu-current nil :background "#cde1f8")
  (use-package! kind-all-the-icons)
  (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))

(use-package! flymake
  :commands (flymake-mode)
  :hook ((prog-mode text-mode) . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-no-changes-timeout 1.0)
  (set-popup-rule! "^\\*format-all-errors*" :size 0.15 :select nil :modeline nil :quit t)
  (set-popup-rule! "^\\*Flymake diagnostics" :size 0.2 :modeline nil :quit t :select nil))

(use-package! flymake-triangle-bitmap
  :after flymake
  :config
  (setq flymake-note-bitmap    '(my-small-left-triangle compilation-info)
        flymake-error-bitmap   '(my-small-left-triangle compilation-error)
        flymake-warning-bitmap '(my-small-left-triangle compilation-warning)))

(after! eldoc
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-use-multiline-p nil)
  (set-popup-rule! "^\\*eldoc*" :size 0.15 :modeline nil :quit t))

(defun my-corfu-frame-visible-h ()
  (and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)))
(add-hook 'yas-keymap-disable-hook #'my-corfu-frame-visible-h)

(use-package dabbrev
  :config
  (setq dabbrev-abbrev-char-regexp "[A-Za-z-_]"))

(use-package! dired
  :commands dired-jump
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-async-mode)
  :init
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-recursive-copies  'always
        dired-recursive-deletes 'always
        dired-create-destination-dirs 'ask
        dired-clean-confirm-killing-deleted-buffers nil)
  :config
  (setq dired-async-skip-fast t)
  (setq dired-omit-files
        (concat "\\`[.][.]?\\'"
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (map! :map dired-mode-map
        :ng "q" #'quit-window )
  (custom-set-faces '(dired-async-message ((t (:inherit success))))))

(use-package! dirvish
  :defer t
  :init (after! dired (dirvish-override-dired-mode))
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("c" "~/Codes/"                    "Codes")
     ("D" "~/Documents/"                "Documents")
     ("w" "~/Works/"                    "Works")
     ("d" "~/Downloads/"                "Downloads")
     ("P" "~/Pictures/"                 "Pictures")
     ("v" "~/Videos/"                   "Videos")
     ("s" "~/Shared/"                   "Shared")
     ("n" "~/Notes/"                    "Notes")
     ("b" "~/Books/"                    "Books")
     ("M" "/mnt/"                       "Drives")
     ("T" "~/.local/share/Trash/files/" "TrashCan")
     ))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  ;; (setq dirvish-reuse-session nil) ; disable session reuse
  ;; (setq dirvish--debouncing-delay 2)
  (setq dirvish-async-listing-threshold 10000)
  (setq dirvish-redisplay-debounce 0.01)
  (setq dirvish-use-mode-line nil)
  ;; (setq dirvish-default-layout '(0 0.5 0.5))
  ;; (setq dirvish-mode-line-format
  ;;       '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-header-line-height '41)
  ;; (setq dirvish-mode-line-height '46)
  (setq dirvish-attributes
        '(file-time all-the-icons file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-all-the-icons-height 0.9)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group --time-style=iso")
  (setq dirvish-fd-default-dir "/home/jiesamb/")
  (setq dirvish-open-with-programs
        `(
          (,dirvish-audio-exts . ("mpv" "%f"))
          (,dirvish-video-exts . ("mpv" "%f"))
          (,dirvish-image-exts . ("eog" "%f"))
          (("doc" "docx") . ("wps" "%f"))
          (("ppt" "pptx") . ("wpp" "%f"))
          (("xls" "xlsx") . ("et" "%f"))
          (("pdf") . ("evince" "%f"))
          (("odt" "ods" "rtf" "odp") . ("libreoffice" "%f"))
          (("epub") . ("koodo-reader" "%f"))
          ))
  (setq dirvish-emerge-groups
  '(("24h" (predicate . recent-files-today))
     ("文档" (extensions "pdf" "epub" "doc" "docx" "xls" "xlsx" "ppt" "pptx"))
     ("视频" (extensions "mp4" "mkv" "webm"))
     ("图片" (extensions "jpg" "png" "svg" "gif"))
     ("音频" (extensions "mp3" "flac" "wav" "ape" "m4a" "ogg"))
     ("压缩包" (extensions "gz" "rar" "zip" "7z" "tar" "z"))))
  (setq dirvish-header-line-format '(:left (path) :right (yank sort index " ")))
  (setq dirvish-path-separators (list "  ~" "  " "/"))
  (setq dirvish-side-display-alist `((side . right) (slot . -1)))
  (setq dirvish-side-width 40)
  (setq dirvish-subtree-file-viewer 'dired-find-file)
  (setq dirvish-side-auto-close t)
  (map! :map dirvish-mode-map
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file
        :n "e" #'dired-create-empty-file
        :n "." #'dired-omit-mode
        :n "C-." #'dirvish-emerge-mode
        :n "q" #'dirvish-quit
        :n "s" #'dirvish-quicksort
        :n "a" #'dirvish-quick-access
        :n "F" #'dirvish-fd
        :n "S" #'dirvish-fd-switches-menu
        :n "y" #'dirvish-yank-menu
        :n "f" #'dirvish-file-info-menu
        :n "H" #'dirvish-history-jump
        :n "TAB" #'dirvish-subtree-toggle
        :n [backtab] #'dirvish-subtree-up
        :n "<mouse-1>" #'dirvish-subtree-toggle
        :n "<mouse-2>" #'dirvish-subtree-toggle
        :n "<mouse-3>" #'dired-find-file
        :n "<mouse-8>" #'dired-up-directory
        :n "<mouse-9>" #'dired-find-file
        :n "<double-mouse-1>" #'dired-find-file
        :n "<double-mouse-3>" #'dired-up-directory
        "M-t" #'dirvish-layout-toggle
        "M-j" #'dirvish-fd-jump
        "M-m" #'dirvish-mark-menu ))

(map! :leader
      :desc "Open dired" "N" #'dired-jump
      :desc "Open dirvish" "V" #'dirvish
      (:prefix ("v" . "my personal bindings")
       :desc "Open dirvish" "v" #'dirvish
       :desc "Open Normal Dired" "n" #'dired-jump
       :desc "Quit dirvish" "q" #'dirvish-quit
       :desc "Toggle dirvish-side" "s" #'dirvish-side
       :desc "Fd in dirvish" "F" #'dirvish-fd
       :desc "Jump using fd" "J" #'dirvish-fd-jump
       :desc "Jump recent dir" "j" #'consult-dir
       :desc "Fd find file in dir" "f" #'+vertico/consult-fd
       :desc "find Item in the buffer" "i" #'consult-imenu
       :desc "open with other coding system" "c" #'revert-buffer-with-coding-system
       :desc "change buffer coding system" "C" #'set-buffer-file-coding-system
       :desc "List processes" "l" #'list-processes
       :desc "VC Refresh state" "r" #'vc-refresh-state))

(setq vterm-always-compile-module t)
(after! vterm
  (setq vterm-max-scrollback 10000)
  (setq vterm-timer-delay 0.01)
  (advice-add #'vterm--redraw :after (lambda (&rest args) (evil-refresh-cursor evil-state)))
  (set-face-attribute 'vterm-color-black nil :background "#a7a7a7"))

(setq-hook! 'vterm-mode-hook
  +popup-margin-width nil
  kill-buffer-query-functions nil)

(use-package! doom-vterm-toggle
  :commands (doom-vterm-toggle-directory
             doom-vterm-toggle-project))

(map! :map vterm-mode-map [f4] nil)
(map! [f4] #'doom-vterm-toggle-directory
      [S-f4] #'+vterm/here
      :leader
      "o t" #'doom-vterm-toggle-project)

(use-package! sis
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  (sis-global-respect-mode t)
  (sis-global-context-mode t))

(add-hook! 'org-mode-hook (setq-local word-wrap nil))

(setq org-directory "~/Notes")
(custom-set-faces
 '(org-level-1 ((t (:height 1.3 :foreground "#4271ae" :weight ultra-bold))))
 '(org-level-2 ((t (:height 1.2 :foreground "#8959a8" :weight extra-bold))))
 '(org-level-3 ((t (:height 1.1 :foreground "#718c00" :weight bold))))
 '(org-level-4 ((t (:height 1.0 :foreground "#eab700" :weight semi-bold))))
 '(org-level-5 ((t (:height 1.0 :foreground "#c82829" :weight normal))))
 '(org-level-6 ((t (:height 1.0 :foreground "#70c0ba" :weight normal))))
 '(org-level-7 ((t (:height 1.0 :foreground "#b77ee0" :weight normal))))
 '(org-level-8 ((t (:height 1.0 :foreground "#9ec400" :weight normal)))))

(after! org
  (setq org-src-preserve-indentation nil)
  (setq org-image-actual-width 500)
  (setq org-hide-emphasis-markers t)
  (map! :map org-mode-map
        :localleader
        "-" #'org-emphasize))

(use-package! org-bullets
  :hook (org-mode . org-bullets-mode)
  :config
  (setq org-bullets-bullet-list '("◉" "○" "✸" "✿" "◈" "◇"))
  (setcdr org-bullets-bullet-map nil))

(use-package! org-appear
  :commands (org-appear-mode)
  :init
  (setq org-appear-autolinks t))

(add-hook 'org-mode-hook #'org-appear-mode)

(setq org-roam-directory "~/Notes/Roam")
(map! :leader
      :desc "Zettelkasten with org-roam" "v z" #'org-roam-node-find
      :desc "org-roam node Insert" "v i" #'org-roam-node-insert)

(after! org-roam
  (setq org-roam-completion-everywhere nil))

(setq org-roam-dailies-directory "~/Notes/Daily")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y>/%<%Y-%m>/%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d %A>\n"))))
(map! :leader
      :desc "my Journal today" "J" #'org-roam-dailies-goto-today
      :desc "org-roam capture" "X" #'org-roam-capture
      :desc "org-roam find node" "Z" #'org-roam-node-find)

(setq org-roam-capture-templates '(
          ("d" "Default" plain "%?"
          :target (file+head "Default/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: \n\n")
          :unnarrowed t)
          ("l" "Learning" plain "%?"
          :target (file+head "Learning/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :learning: \n\n")
          :unarrowed t)
          ("r" "Reading" plain "%?"
          :target (file+head "Reading/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :reading: \n\n")
          :unnarrowed t)
          ("t" "Thinking" plain "%?"
          :target (file+head "Thinking/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :thinking: \n\n")
          :unnarrowed t)
          ("w" "Working" plain "%?"
          :target (file+head "Working/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :working: \n\n")
          :unnarrowed t)
          ("c" "Coding" plain "%?"
          :target (file+head "Coding/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :coding: \n\n")
          :unnarrowed t)))

(custom-set-faces
 '(markdown-code-face ((t (:background "#f5f5f5"))))
 '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.3 :foreground "#4271ae" :weight ultra-bold))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2 :foreground "#8959a8" :weight extra-bold))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.1 :foreground "#b5bd68" :weight bold))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.0 :foreground "#e6c547" :weight semi-bold))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.0 :foreground "#c82829" :weight normal))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0 :foreground "#70c0ba" :weight normal))))
 '(markdown-header-face-7 ((t (:inherit markdown-header-face :height 1.0 :foreground "#b77ee0" :weight normal)))))

(after! markdown-mode
  (setq markdown-fontify-whole-heading-line nil)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-max-image-size '(500 . 500))
  (set-popup-rule! "^\\*edit-indirect" :size 0.42 :quit nil :select t :autosave t :modeline t :ttl nil))

(defun my-eglot-organize-imports ()
  (ignore-errors(call-interactively 'eglot-code-action-organize-imports)))
(defun my-go-mode-init ()
  (setq-local corfu-sort-function nil)
  (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'my-eglot-organize-imports nil t))
(add-hook 'go-mode-hook #'my-go-mode-init)

(after! go-mode
  (map! :map go-mode-map
        :localleader
        "h" nil
        "e" nil
        "i" nil
        (:prefix ("i" . "imports")
                 "i" #'go-goto-imports
                 "a" #'go-import-add
                 "r" #'go-remove-unused-imports)))

(use-package protobuf-mode
  :commands (protobuf-mode)
  :mode("\\.proto\\'" . protobuf-mode))

(after! sh-script
  (set-formatter! 'shfmt
    '("shfmt" "-ci"
      ("-i" "%d" (unless indent-tabs-mode tab-width))
      ("-ln" "%s" (pcase sh-shell (`bash "bash") (`zsh "bash") (`mksh "mksh") (_ "posix"))))))

(add-to-list 'auto-mode-alist '("\\.vpy\\'" . python-mode))

(after! org
  (add-to-list 'org-src-lang-modes '("py" . python-mode)))

(after! markdown-mode
  (add-to-list 'markdown-code-lang-modes '("py" . python-mode)))

(use-package! tab-bar
  :init
  (setq tab-bar-show nil)
  :config
  (tab-bar-rename-tab "Home")
  (use-package! tab-bar-helper
    :commands (tab-bar-new-tab-with-name))
  (map! :leader
        :desc "tab-bar switch tab" "TAB" #'tab-bar-switch-to-tab
        :desc "tab-bar new tab" "v TAB" #'tab-bar-new-tab-with-name))

(use-package! bookmark-view
  :commands (bookmark-view))

(map! :leader
      :desc "bookmark view" "b v" #'bookmark-view)

(use-package! fanyi
  :commands (fanyi-dwim
             fanyi-dwim2)
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; Etymonline
                     ;; fanyi-etymon-provider
                     ;; Longman
                     ;; fanyi-longman-provider
                     )))

(set-popup-rule! "^\\*fanyi*" :size 0.3 :modeline nil :quit t)
(add-hook 'fanyi-mode-hook #'doom-disable-line-numbers-h)
(map! :leader
      :desc "Translate word" "v t" #'fanyi-dwim2)

(after! restclient
  (setq restclient-use-var-regexp
        "{{\([^{ \n]+\)}}$")
  (setq restclient-var-regexp
        (concat "^\\(@[^@= ]+\\)[ \t]*\\(:?\\)=[ \t]*\\(<<[ \t]*\n\\(\\(.*\n\\)*?\\)" restclient-comment-separator "\\|\\([^<].*\\)$\\)"))
  (setq restclient-svar-regexp
        "^\\(@[^@= ]+\\)[ \t]*=[ \t]*\\(.+?\\)$")
  (setq restclient-evar-regexp
        "^\\(@[^@ ]+\\)[ \t]*:=[ \t]*\\(.+?\\)$")
  (setq restclient-mvar-regexp
        "^\\(@[^@ ]+\\)[ \t]*:?=[ \t]*\\(<<\\)[ \t]*$"))

(use-package! texfrag
  :commands (texfrag-mode)
  :init
  (setq texfrag-markdown-preview-image-links nil
        texfrag-subdir ".texfrag"))

(defun my-toggle-texfrag-preview-document()
  (interactive)
  (if (bound-and-true-p texfrag-mode)
      (texfrag-mode -1)
    (progn (texfrag-mode)
           (texfrag-document))))
(map! :map markdown-mode-map :localleader
      :desc "latex preview math" "l" #'my-toggle-texfrag-preview-document)

(setq +org-present-text-scale 3)
(add-hook 'org-tree-slide-play-hook #'doom-disable-line-numbers-h)
(add-hook 'org-tree-slide-stop-hook #'doom-enable-line-numbers-h)
