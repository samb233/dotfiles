(setq user-full-name "Jie Samb"
      user-mail-address "samb233@hotmail.com")

(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(pushnew! default-frame-alist '(width . 80) '(height . 50))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; (add-to-list 'default-frame-alist '(alpha-background . 85))
;; (add-to-list 'default-frame-alist (cons 'alpha 90))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq doom-font (font-spec :family "IBM Plex Mono Medm" :size 11.0))
(setq doom-variable-pitch-font (font-spec :family "BlexMono Nerd Font"))
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

(setq evil-emacs-state-cursor 'bar)

(setq display-line-numbers-type 'relative)

(setq doom-modeline-modal t)
(setq doom-modeline-modal-icon nil)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-height 29)
(setq doom-modeline-buffer-modification-icon nil)
(setq doom-modeline-buffer-state-icon nil)

(defalias 'evil-insert-state 'evil-emacs-state)
(define-key evil-emacs-state-map (kbd "<escape>") 'evil-normal-state)
;; (setq evil-disable-insert-state-bindings t)

(evil-define-key 'visual 'global
  (kbd "J") 'drag-stuff-down
  (kbd "K") 'drag-stuff-up)

(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)

(evil-define-key 'normal 'global
  (kbd "q") nil)

(map! :leader
      :desc "ace-select-window" "w a" #'ace-select-window
      :desc "ace-select-window" "w w" #'ace-select-window
      )

(use-package! lsp-bridge
  :config
  (map! :map acm-mode-map
        "C-j"     #'acm-select-next
        "C-k"     #'acm-select-prev
        :map yas-keymap
        [tab]     #'acm-complete-or-expand-yas-snippet
        [right]   #'yas-next-field-or-maybe-expand
        "C-l"     #'yas-next-field-or-maybe-expand
        )
  (map! :leader
        (:prefix-map ("l" . "LSP")
         :desc "LSP rename" "n" #'lsp-bridge-rename
         :desc "LSP find definitions" "f" #'lsp-bridge-find-def
         :desc "LSP find definitions" "o" #'lsp-bridge-find-def-other-window
         :desc "LSP find reference" "r" #'lsp-bridge-find-references
         :desc "LSP ui doc toggle" "h" #'lsp-bridge-popup-documentation
         :desc "LSP restart server" "R" #'lsp-bridge-restart-process
         :desc "LSP Error list" "e" #'lsp-bridge-diagnostic-list
         :desc "LSP code action" "a" #'lsp-bridge-code-action
         ))
  (evil-define-key 'insert acm-mode-map
    (kbd "C-j") 'acm-select-next
    (kbd "C-k") 'acm-select-prev
    (kbd "C-l") 'acm-complete
    (kbd "RET") 'acm-complete
    (kbd "TAB") 'acm-complete
    )
  (add-hook 'acm-mode-hook #'evil-normalize-keymaps)

  (evil-define-key 'normal lsp-bridge-ref-mode-map
    (kbd "RET") 'lsp-bridge-ref-open-file-and-stay
    (kbd "q") 'lsp-bridge-ref-quit
    )

  (setq lsp-bridge-enable-mode-line nil)
  (setq lsp-bridge--mode-line-format '())
  (setq lsp-bridge-enable-hover-diagnostic t)
  (setq lsp-bridge-diagnostic-max-number 200)
  (global-lsp-bridge-mode))

(map! :after evil-org
      :map evil-org-mode-map
      :i "C-j" nil
      :i "C-k" nil
      :i "RET" nil
      :i [return] nil)

;; (advice-add #'eldoc-mode :override #'(lambda (x) (message "disabled eldoc-mode")))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("c" "~/Codes/"                    "Codes")
     ("t" "~/Codes/Try/"                "Try")
     ("p" "~/Codes/Projects/"           "Projects")
     ("r" "~/Codes/Reading/"            "Reading")
     ("d" "~/Documents/"                "Documents")
     ("w" "~/工作/"                      "工作")
     ("D" "~/Downloads/"                "Downloads")
     ("P" "~/Pictures/"                 "Pictures")
     ("v" "~/Videos/"                   "Videos")
     ("s" "~/Shared/"                   "Shared")
     ("M" "/mnt/"                       "Drives")
     ("T" "~/.local/share/Trash/files/" "TrashCan")
     ))
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  ;; (setq dirvish-reuse-session nil) ; disable session reuse
  ;; (setq dirvish--debouncing-delay 2)
  (setq dirvish-async-listing-threshold 10000)
  (setq dirvish-use-mode-line nil)
  ;; (setq dirvish-default-layout '(0 0.4 0.6))
  ;; (setq dirvish-mode-line-format
  ;;       '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-header-line-height '46)
  ;; (setq dirvish-mode-line-height '46)
  (setq dirvish-attributes
        '(file-time all-the-icons file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (setq dirvish-fd-default-dir "/home/jiesamb/")
  (setq dirvish-open-with-programs
        `(
          (,dirvish-audio-exts . ("mpv" "%f"))
          (,dirvish-video-exts . ("mpv" "%f"))
          (,dirvish-image-exts . ("gwenview" "%f"))
          (("doc" "docx") . ("wps" "%f"))
          (("ppt" "pptx") . ("wpp" "%f"))
          (("xls" "xlsx") . ("et" "%f"))
          (("pdf") . ("evince" "%f"))
          (("odt" "ods" "rtf" "odp") . ("libreoffice" "%f"))
          ))
  ;; (setq dirvish-header-line-format '(:left (path) :right (free-space)))
  (setq dirvish-header-line-format '(:left (path) :right (yank sort index)))
  (setq dirvish-path-separators (list "  ~" "  " "/"))
  (setq dirvish-side-display-alist `((side . right) (slot . -1)))
  (setq dirvish-side-width 40)
  (setq dirvish-side-auto-close t)
  :hook
  (dired-mode . dired-omit-mode)
  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("^"   . dirvish-history-last)
   ("H"   . dirvish-history-jump) ; remapped `describe-mode'
   ("s"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))

(evil-define-key 'normal dirvish-mode-map
  (kbd "e") 'dired-create-empty-file
  (kbd "q") 'dirvish-quit ;; use dirvish would kill the preview buffer
  (kbd "s") 'dirvish-quicksort
  (kbd "a") 'dirvish-quick-access
  (kbd "F") 'dirvish-fd
  (kbd "y") 'dirvish-yank-menu
  (kbd "f") 'dirvish-file-info-menu
  (kbd ".") 'dired-omit-mode
  )

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file)

(map! :leader
      (:prefix ("v" . "dirvish and vertico")
       :desc "Open dirvish" "v" #'dirvish
       :desc "Open Normal Dired" "n" #'dired-jump
       :desc "Quit dirvish" "q" #'dirvish-quit
       :desc "Toggle dirvish-side" "s" #'dirvish-side
       :desc "Fd in dirvish" "F" #'dirvish-fd
       :desc "Jump using fd" "J" #'dirvish-fd-jump
       :desc "Jump recent dir" "j" #'consult-dir
       :desc "Fd find file in dir" "f" #'+vertico/consult-fd
       :desc "Project searching by vertico" "p" #'+vertico/project-search
       :desc "open with other coding system" "c" #'revert-buffer-with-coding-system
       :desc "change buffer coding system" "C" #'set-buffer-file-coding-system
       :desc "toggle tree-sitter-hl-mode" "t" #'tree-sitter-hl-mode
       ))

(after! vterm
 (setq vterm-max-scrollback 10000)
 (remove-hook 'vterm-mode-hook 'hide-mode-line-mode))

(after! eshell
  (eshell-git-prompt-use-theme 'simple)
  )

(setq auto-revert-check-vc-info t)

(use-package docker
  :config
  (set-popup-rule! "^\\* podman " :size 0.8 :modeline t :quit 'other)
  (setq docker-command "podman")
  (setq docker-compose-command "podman-compose")
  (setq docker-pop-to-buffer-action '(display-buffer-same-window))
  (setq docker-run-async-with-buffer-function #'docker-run-async-with-buffer-vterm)
  (setq docker-container-columns
        '(
          (:name "Id" :width 14 :template "{{ json .ID }}" :sort nil :format nil)
          (:name "Names" :width 12 :template "{{ json .Names }}" :sort nil :format nil)
          (:name "Status" :width 14 :template "{{ json .Status }}" :sort nil :format nil)
          (:name "Ports" :width 24 :template "{{ json .Ports }}" :sort nil :format nil)
          (:name "Image" :width 40 :template "{{ json .Image }}" :sort nil :format nil)
          (:name "Created" :width 21 :template "{{ json .CreatedAt }}" :sort nil :format
                 (lambda
                   (x)
                   (format-time-string "%F %T"
                                       (date-to-time x))))
          (:name "Command" :width 20 :template "{{ json .Command }}" :sort nil :format nil)
          )
        )
  )

(map! :leader
      :desc "docker Containers" "o c" #'docker-containers
      )

(use-package sis
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

(add-hook 'evil-emacs-state-exit-hook 'sis-set-english)
(add-hook 'evil-emacs-state-entry-hook 'sis-context t)

(add-hook 'evil-emacs-state-exit-hook 'doom-modeline-update-buffer-file-name)
(add-hook 'evil-emacs-state-exit-hook '+default-disable-delete-selection-mode-h)
(add-hook 'evil-emacs-state-exit-hook 'evil-maybe-expand-abbrev)

(setq org-directory "~/Documents/Notes")
(after! org
  (defun org-colors-tomorrow-night ()
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

  (defun org-colors-tomorrow-day()
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

  (org-colors-tomorrow-day)
  (setq org-src-preserve-indentation nil)
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
(setq deft-directory "~/Documents/Notes")
(setq deft-recursive t)

(use-package sh-script
  :config
  (set-formatter! 'shfmt
    '("shfmt" "-ci"
      ("-i" "%d" (unless indent-tabs-mode tab-width))
      ("-ln" "%s" (pcase sh-shell (`bash "bash") (`zsh "bash") (`mksh "mksh") (_ "posix")))))
  )

(after! go-mode
  (remove-hook 'go-mode-hook #'go-eldoc-setup))

(set-popup-rule! "^\\*format-all-errors*" :size 0.3 :modeline t :quit t)

(setq flycheck-check-syntax-automatically '(save mode-enabled))
