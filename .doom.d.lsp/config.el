(server-start)

(setq user-full-name "Jie Samb"
      user-mail-address "samb233@hotmail.com")

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(pushnew! default-frame-alist '(width . 80) '(height . 50))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (add-to-list 'default-frame-alist '(alpha-background . 95))
;; (add-to-list 'default-frame-alist (cons 'alpha 90))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq doom-font (font-spec :family "Iosevka Medium" :size 13.0))
;; (setq doom-variable-pitch-font (font-spec :family "Noto Serif CJK SC"))
(setq doom-unicode-font (font-spec :family "Sarasa Mono SC" ))

(setq scroll-margin 9)
(setq mouse-wheel-scroll-amount '
      (3
       ((shift) . hscroll)
       ((meta))
       ((control) . text-scale)))
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; (pixel-scroll-precision-mode t)

(setq doom-theme 'doom-tomorrow-day)

(setq display-line-numbers-type 'relative)

(setq doom-modeline-modal t)
(setq doom-modeline-modal-icon nil)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-height 28)
(setq doom-modeline-buffer-modification-icon nil)
(setq doom-modeline-buffer-state-icon nil)
(after! doom-modeline
  (set-face-attribute 'mode-line-active nil :background "#f4f4f4")
  (setq doom-modeline-buffer-file-name-style 'buffer-name)
  )

(after! persp-mode
  (setq-hook! 'persp-mode-hook uniquify-buffer-name-style 'forward)
  )

(setq highlight-indent-guides-method 'bitmap)
(setq highlight-indent-guides-responsive 'top)

;; (defalias 'evil-insert-state 'evil-emacs-state)
;; (define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)

(evil-define-key 'visual 'global
  (kbd "J") 'drag-stuff-down
  (kbd "K") 'drag-stuff-up)

(evil-define-key 'normal 'global (kbd "C-s") 'consult-line)
(map! "C-s" #'consult-line)
;; (setq consult-line-start-from-top t)

(map! "C-v" #'yank)
(map! "M-v" #'yank)

(evil-define-key 'insert 'global (kbd "C-z") 'undo-fu-only-undo)
(evil-define-key 'normal 'global (kbd "C-z") 'undo-fu-only-undo)
(evil-define-key 'insert 'global (kbd "C-S-z") 'undo-fu-only-redo)
(evil-define-key 'normal 'global (kbd "C-S-z") 'undo-fu-only-redo)

(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)

(map! :map global-map "C-c k" #'+lookup/documentation)

(evil-define-key 'normal 'global (kbd "] e") 'flymake-goto-next-error)
(evil-define-key 'normal 'global (kbd "[ e") 'flymake-goto-prev-error)

(setq undo-no-redo t)
(setq evil-want-fine-undo t)

(after! recentf
  (setq recentf-max-saved-items 1000)
  )

(add-to-list 'auto-mode-alist '("\\.vpy\\'" . python-mode))

(setq auto-revert-check-vc-info t)

(after! evil
  (setq evil-emacs-state-tag "EMACS ")
  (setq evil-insert-state-tag "INSERT")
  (setq evil-motion-state-tag "MOTION")
  (setq evil-normal-state-tag "NORMAL")
  (setq evil-replace-state-tag "REPLACE")
  (setq evil-operator-state-tag "OPERATOR")
  (setq evil-visual-char-tag "VISUAL")
  (setq evil-visual-line-tag "V-LINE")
  (setq evil-visual-block-tag "V-BLOCK")
  (setq evil-visual-screen-line-tag "V-SCREEN")
  )

(setq magit-clone-default-directory "~/Codes/Lab/")

(after! persp-mode
  (map! :leader
        :desc "Switch buffer" "," #'consult-buffer
        :desc "Switch workspace buffer" "<" #'+vertico/switch-workspace-buffer )
  )

(map! :leader
      (:prefix-map ("l" . "LSP")
       :desc "LSP rename" "n" #'eglot-rename
       :desc "LSP find definitions" "f" #'xref-find-definitions
       :desc "LSP find reference" "r" #'xref-find-references
       :desc "LSP restart workspace" "R" #'eglot-reconnect
       ))

(after! eglot
  (set-face-attribute 'eglot-highlight-symbol-face nil :background "#d6d4d4")
  (setq eglot-events-buffer-size 0)
  (setq eglot-stay-out-of '(snippet))
  ;; (add-hook 'eglot-managed-mode-hook (lambda () (eglot-inlay-hints-mode -1)))
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  )

(setq eldoc-echo-area-display-truncation-message nil)
(setq eldoc-echo-area-use-multiline-p nil)
(set-popup-rule! "^\\*eldoc*" :size 0.15 :modeline nil :quit t)

(after! corfu
  (setq corfu-preselect 'prompt)
  ;; (setq corfu-preview-current nil)
  (setq corfu-auto-prefix 1)
  (setq corfu-auto-delay 0.1)
  (setq cape-dict-file "~/.doom.d/dict/words")
  (map! :map corfu-map
        :i "TAB" #'corfu-next
        :i [tab] #'corfu-next
        :i "S-TAB" #'corfu-previous
        :i [backtab] #'corfu-previous
        :i "C-j" #'corfu-next
        :i "C-k" #'corfu-previous
        :i "C-l" #'corfu-insert-separator
        :i "C-i" #'corfu-info-documentation
        :i "C-g" #'corfu-quit
        )
  (map! :map global-map
        :i "C-S-p" #'cape-file)
  )

(add-hook! 'evil-insert-state-exit-hook #'corfu-quit)

(use-package! kind-all-the-icons
  :after corfu
  )

(after! corfu
  (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter)
  )

;; (after! yasnippet
;;   (defun my-corfu-frame-visible-h ()
;;     (and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)))
;;   (add-hook 'yas-keymap-disable-hook #'my-corfu-frame-visible-h)
;;   )

(use-package! dired
  :commands dired-jump
  :init (setq dired-dwim-target t)
  :config
  (setq dired-omit-files
        (concat "\\`[.][.]?\\'"
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (map! :map dired-mode-map :ng "q" #'quit-window)
  )

(use-package! dirvish
  :defer t
  :init (after! dired (dirvish-override-dired-mode))
  :hook (dired-mode . dired-omit-mode)
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
  (setq dirvish-redisplay-debounce 0)
  (setq dirvish-use-mode-line nil)
  ;; (setq dirvish-default-layout '(0 0.4 0.6))
  ;; (setq dirvish-mode-line-format
  ;;       '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-header-line-height '41)
  ;; (setq dirvish-mode-line-height '46)
  (setq dirvish-attributes
        '(file-time all-the-icons file-size collapse subtree-state vc-state git-msg))
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
  (setq dirvish-header-line-format '(:left (path) :right (yank sort index " ")))
  (setq dirvish-path-separators (list "  ~" "  " "/"))
  (setq dirvish-side-display-alist `((side . right) (slot . -1)))
  (setq dirvish-side-width 40)
  (setq dirvish-side-auto-close t)
  (map! :map dirvish-mode-map
        :n "h" #'dired-up-directory
        :n "l" #'dired-find-file
        :n "e" #'dired-create-empty-file
        :n "." #'dired-omit-mode
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
        "M-t" #'dirvish-layout-toggle
        "M-j" #'dirvish-fd-jump
        "M-m" #'dirvish-mark-menu )
  )

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
       :desc "calc mode" "a" #'literate-calc-mode
       ))

(setq delete-window-choose-selected 'pos)

(after! vterm
  (setq vterm-max-scrollback 10000)
  (setq vterm-timer-delay 0.01)
  (advice-add #'vterm--redraw :after (lambda (&rest args) (evil-refresh-cursor evil-state)))
  (set-face-attribute 'vterm-color-black nil :background "#a7a7a7")
  )

(use-package! eshell-git-prompt
  :commands (eshell-git-prompt-use-theme)
  )

(defun my/eshell-use-git-prompt-theme()
  (eshell-git-prompt-use-theme 'git-radar)
  )
(add-hook! 'eshell-prompt-load-hook #'my/eshell-use-git-prompt-theme)

(use-package! sis
  :config
  (sis-ism-lazyman-config "1" "2" 'fcitx5)
  ;; enable the /cursor color/ mode
  ;; (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  ;; (sis-global-inline-mode t)
  )

(add-hook! 'org-mode-hook #'toggle-word-wrap)

(setq org-directory "~/Notes")

(defun my/org-colors-tomorrow-night ()
  (interactive)
  (dolist
      (face
       '((org-level-1 1.3 "#81a2be" ultra-bold)
         (org-level-2 1.2 "#b294bb" extra-bold)
         (org-level-3 1.1 "#b5bd68" bold)
         (org-level-4 1.0 "#e6c547" semi-bold)
         (org-level-5 1.0 "#cc6666" normal)
         (org-level-6 1.0 "#70c0ba" normal)
         (org-level-7 1.0 "#b77ee0" normal)
         (org-level-8 1.0 "#9ec400" normal)))
    (set-face-attribute (nth 0 face) nil :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
  (set-face-attribute 'org-table nil :weight 'normal :height 1.0 :foreground "#bfafdf"))

(defun my/org-colors-tomorrow-day()
  (interactive)
  (dolist
      (face
       '((org-level-1 1.3 "#4271ae" ultra-bold)
         (org-level-2 1.2 "#8959a8" extra-bold)
         (org-level-3 1.1 "#b5bd68" bold)
         (org-level-4 1.0 "#e6c547" semi-bold)
         (org-level-5 1.0 "#c82829" normal)
         (org-level-6 1.0 "#70c0ba" normal)
         (org-level-7 1.0 "#b77ee0" normal)
         (org-level-8 1.0 "#9ec400" normal)))
    (set-face-attribute (nth 0 face) nil :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
  (set-face-attribute 'org-table nil :weight 'normal :height 1.0 :foreground "#bfafdf"))

(after! org
  (my/org-colors-tomorrow-day)
  (setq org-src-preserve-indentation nil)
  (setq org-image-actual-width 500)
  )

(use-package! org-modern
  :commands (org-modern-mode)
  :init
  (setq org-modern-block-name nil)
  (setq org-modern-star '("○" "◉" "✸" "✿" "✳" "▶" "◇" "◈"))
  )

(add-hook 'org-mode-hook #'org-modern-mode)

(use-package! org-appear
  :commands (org-appear-mode)
  :init
  (setq org-appear-autolinks t)
  )

(add-hook 'org-mode-hook 'org-appear-mode)

(setq org-roam-directory "~/Notes/Roam")
(map! :leader
      :desc "Zettelkasten by org-roam" "v z" #'org-roam-node-find
      :desc "org-roam node Insert" "v i" #'org-roam-node-insert
      )

(setq org-roam-dailies-directory "~/Notes/Daily")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y>/%<%Y-%m>/%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d %A>\n"))))
(map! :leader
      :desc "my Journal today" "J" #'org-roam-dailies-goto-today
      )

(setq org-roam-capture-templates '(
          ("d" "Default" plain "%?"
          :target (file+head "Default/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: \n\n")
          :unnarrowed t)
          ("t" "Tech" plain "%?"
          :target (file+head "Tech/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :tech: \n\n")
          :unarrowed t)
          ("b" "Book Notes" plain "%?"
          :target (file+head "Book/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :book: \n\n")
          :unnarrowed t)
          ("w" "Work" plain "%?"
          :target (file+head "Work/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :work: \n\n")
          :unnarrowed t)
          ("a" "Anime" plain "%?"
          :target (file+head "Anime/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :anime: \n\n")
          :unnarrowed t)
          ("v" "Video or VCBs" plain "%?"
          :target (file+head "Video/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :video: \n\n")
          :unnarrowed t)
          ("p" "Project" plain "%?"
          :target (file+head "Project/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :project: \n\n")
          :unnarrowed t)
          ("c" "Coding" plain "%?"
          :target (file+head "Coding/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :coding: \n\n")
          :unnarrowed t)
          ("h" "Thinking" plain "%?"
          :target (file+head "Thinking/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :thinking: \n\n")
          :unnarrowed t)
          ("i" "Inspiration" plain "%?"
          :target (file+head "Inspiration/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :inspiration: \n\n")
          :unnarrowed t)))

(map! :leader
      :desc "org-roam capture" "X" #'org-roam-capture
      :desc "org-roam find node" "Z" #'org-roam-node-find
      )

(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0)))))

(setq deft-extensions '("txt" "tex" "org" "md"))
(setq deft-directory "~/Notes")
(setq deft-recursive t)

(defun my/deft-parse-title (file contents)
  "Parse the given FILE and CONTENTS and determine the title.
If `deft-use-filename-as-title' is nil, the title is taken to
be the first non-empty line of the FILE.  Else the base name of the FILE is
used as title."
  (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
    (if begin
        (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
      (deft-base-filename file))))

(advice-add 'deft-parse-title :override #'my/deft-parse-title)

(setq deft-strip-summary-regexp
      (concat "\\("
              "[\n\t]" ;; blank
              "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
              "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
              "\\)"))

(after! sh-script
  (set-formatter! 'shfmt
    '("shfmt" "-ci"
      ("-i" "%d" (unless indent-tabs-mode tab-width))
      ("-ln" "%s" (pcase sh-shell (`bash "bash") (`zsh "bash") (`mksh "mksh") (_ "posix")))))
  )

(defun lsp-go-install-save-hooks ()
  (defun my-eglot-organize-imports () (interactive)
         (eglot-code-actions nil nil "source.organizeImports" t))
  (add-hook 'before-save-hook 'my-eglot-organize-imports nil t)
  (add-hook 'before-save-hook 'eglot-format-buffer))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package protobuf-mode
  :commands (protobuf-mode)
  :mode("\\.proto\\'" . protobuf-mode)
  )

(set-popup-rule! "^\\*format-all-errors*" :size 0.3 :modeline t :quit t)

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
(set-popup-rule! "^\\*fanyi*" :size 0.3 :modeline t :quit t)
(map! :leader
      :desc "Translate word" "v t" #'fanyi-dwim2
      )
