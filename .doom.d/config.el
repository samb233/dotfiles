;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; open at maximaized
(pushnew! default-frame-alist '(width . 80) '(height . 50))
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; (add-to-list 'default-frame-alist '(alpha-background . 85))
;; (add-to-list 'default-frame-alist (cons 'alpha 90))
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jie Samb"
      user-mail-address "samb233@hotmail.com")


;; set fonts
(setq doom-font (font-spec :family "IBM Plex Mono Medm" :size 11.0))
(setq doom-variable-pitch-font (font-spec :family "BlexMono Nerd Font"))
(setq doom-unicode-font (font-spec :family "Sarasa Mono SC" ))

;; (setq doom-unicode-font (font-spec :family "BlexMono Nerd Font" ))

;; (defun +my/better-font()
;;   (interactive)
;;   ;; english font
;;   (if (display-graphic-p)
;;       (progn
;;         (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "IBM Plex Mono Medm" 15)) ;; 11 13 17 19 23S
;;         ;; chinese font
;;         (dolist (charset '(kana han symbol cjk-misc bopomofo))
;;           (set-fontset-font (frame-parameter nil 'font)
;;                             charset
;;                             (font-spec :family "Noto Sans Mono CJK SC")))) ;; 14 16 20 22 28
;;     ))

;; (defun +my|init-font(frame)
;;   (with-selected-frame frame
;;     (if (display-graphic-p)
;;         (+my/better-font))))

;; (if (and (fboundp 'daemonp) (daemonp))
;;     (add-hook 'after-make-frame-functions #'+my|init-font)
;;   (+my/better-font))

;; doom-modeline settings
(setq doom-modeline-modal nil)
(setq doom-modeline-buffer-encoding t)
(setq doom-modeline-vcs-max-length 20)
(setq doom-modeline-height 28)
(setq doom-modeline-buffer-modification-icon nil)
(setq doom-modeline-buffer-state-icon nil)
;; (setq doom-modeline-bar-width 7)
;; (setq doom-modeline-major-mode-icon t)

;; open company tab on go mode
(add-hook 'after-init-hook 'company-tng-mode)

;; mousewheel settings
;; scroll one line at a time (less "jumpy" than defaults)

(setq scroll-margin 9)
;; (pixel-scroll-precision-mode t)
;; (good-scroll-mode 1)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 7))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face

;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-day)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/Notes")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;


;; smart input source switch
(use-package sis
  ;; :hook
  ;; enable the /context/ and /inline region/ mode for specific buffers
  ;; (((text-mode prog-mode) . sis-context-mode)
  ;;  ((text-mode prog-mode) . sis-inline-mode))

  :config
  ;; For Fcitx5
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


;; setting keybinds for vertico project search
(map! :leader
      (:prefix ("p" . "Project")
       :desc "Vertico search in project" "v" #'+vertico/project-search
       )
      )

;; lsp-mode settings
(setq lsp-headerline-breadcrumb-enable t)

;; go-mode settings
(defun lsp-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; Go lsp-mode setting
;; Set up before-save hooks to format buffer and add/delete imports.
;; (add-hook 'go-mode-hook #'lsp-deferred)
(after! go-mode
  (set-docsets! 'go-mode "Go")
  (set-repl-handler! 'go-mode #'gorepl-run)
  (set-lookup-handlers! 'go-mode
    :definition #'go-guru-definition
    :references #'go-guru-referrers
    :documentation #'godoc-at-point)

  ;; Redefines default formatter to *not* use goimports if reformatting a
  ;; region; as it doesn't play well with partial code.
  (set-formatter! 'gofmt
    '(("%s" (if (or +format-region-p
                    (not (executable-find "goimports")))
                "gofmt"
              "goimports"))))

  (if (modulep! +lsp)
      (add-hook 'go-mode-local-vars-hook #'lsp! 'append)
    (add-hook 'go-mode-hook #'lsp-save-hooks))

  (when (modulep! +tree-sitter)
    (add-hook 'go-mode-local-vars-hook #'tree-sitter! 'append))

  (map! :map go-mode-map
        :localleader
        "a" #'go-tag-add
        "d" #'go-tag-remove
        "e" #'+go/play-buffer-or-region
        "i" #'go-goto-imports      ; Go to imports
        (:prefix ("h" . "help")
                 "." #'godoc-at-point     ; Lookup in godoc
                 "d" #'go-guru-describe   ; Describe this
                 "v" #'go-guru-freevars   ; List free variables
                 "i" #'go-guru-implements ; Implements relations for package types
                 "p" #'go-guru-peers      ; List peers for channel
                 "P" #'go-guru-pointsto   ; What does this point to
                 "r" #'go-guru-referrers  ; List references to object
                 "e" #'go-guru-whicherrs  ; Which errors
                 "w" #'go-guru-what       ; What query
                 "c" #'go-guru-callers    ; Show callers of this function
                 "C" #'go-guru-callees)   ; Show callees of this function
        (:prefix ("ri" . "imports")
                 "a" #'go-import-add
                 "r" #'go-remove-unused-imports)
        (:prefix ("b" . "build")
         :desc "go run ." "r" (cmd! (compile "go run ."))
         :desc "go build" "b" (cmd! (compile "go build"))
         :desc "go clean" "c" (cmd! (compile "go clean")))
        (:prefix ("t" . "test")
                 "t" #'+go/test-rerun
                 "a" #'+go/test-all
                 "s" #'+go/test-single
                 "n" #'+go/test-nested
                 "f" #'+go/test-file
                 "g" #'go-gen-test-dwim
                 "G" #'go-gen-test-all
                 "e" #'go-gen-test-exported
                 (:prefix ("b" . "bench")
                          "s" #'+go/bench-single
                          "a" #'+go/bench-all))))


;; Rust lsp-mode setting
;; (setq lsp-rust-server 'rust-analyzer)
;; (setq rustic-lsp-server 'rust-analyzer)


;; lsp-mode keybinding
(map! :leader
      (:prefix ("l" . "LSP")
       :desc "LSP rename" "n" #'lsp-rename
       :desc "LSP find definitions" "f" #'lsp-ui-peek-find-definitions
       :desc "LSP find reference" "r" #'lsp-find-references
       :desc "LSP ui doc toggle" "h" #'lsp-ui-doc-glance
       )
      )



;; some keybindings
(evil-define-key 'insert 'global
  (kbd "C-v") 'yank)

(evil-define-key 'visual 'global
  (kbd "J") 'drag-stuff-down
  (kbd "K") 'drag-stuff-up
  )

;; :q should kill the current buffer rather than quitting emacs entirely
(evil-ex-define-cmd "q" 'kill-this-buffer)
;; Need to type out :quit to close emacs
(evil-ex-define-cmd "quit" 'evil-quit)

;; settings for deft
(setq deft-extensions '("txt" "tex" "org" "md"))
(setq deft-directory "~/Documents/Notes")
(setq deft-recursive t)

;; markdown font size settings
(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0)))))

;; Org mode font config
(after! org
  (defun org-colors-tomorrow-night ()
    "Enable Tomorrow Night colors for Org headers."
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
    "Enable Tomorrow Night colors for Org headers."
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

  (defun yank-with-indent ()
    (interactive)
    (let ((indent
           (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (message indent)
      (yank)
      (save-excursion
        (save-restriction
          (narrow-to-region (mark t) (point))
          (pop-to-mark-command)
          (replace-string "\n" (concat "\n" indent))
          (widen)))))
  )

;; (setq fd-dired-ls-option '(""))

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

;; (evil-define-key 'visual )

(map! :leader
      (:prefix ("v" . "dirvish and vertico")
       :desc "Open dirvish" "v" #'dirvish
       :desc "Quit dirvish" "q" #'dirvish-quit
       :desc "Toggle dirvish-side" "s" #'dirvish-side
       :desc "Fd in dirvish" "F" #'dirvish-fd
       :desc "Jump using fd" "J" #'dirvish-fd-jump
       :desc "Jump recent dir" "j" #'consult-dir
       :desc "Fd find file in dir" "f" #'+vertico/consult-fd
       :desc "Project searching by vertico" "p" #'+vertico/project-search
       :desc "Neotree toggle" "n" #'neotree-toggle
       :desc "fuZzy finder" "z" #'affe-find
       :desc "fuzzy Grep" "g" #'affe-grep
       :desc "select Window" "w" #'ace-select-window
       :desc "fuzzy find Home" "h" (cmd!! #'affe-find "~/")
       :desc "open with other coding system" "c" #'revert-buffer-with-coding-system
       :desc "change buffer coding system" "C" #'set-buffer-file-coding-system
       )
      )

(map! :leader
      :desc "ace-select-window" "w a" #'ace-select-window
      :desc "ace-select-window" "w w" #'ace-select-window
      )

(map! :leader
      :desc "Fuzzy find file in dir" "f z" #'+vertico/consult-fd
      )

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file ; use dired-find-file instead of dired-open.
  )
;; Get file icons in dired


;; With dired-open plugin, you can launch external programs for certain extensions
(setq dired-open-extensions '(("gif" . "eog")
                              ("jpg" . "eog")
                              ("png" . "eog")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")
                              ("doc" . "wps")
                              ("xls" . "et")
                              ("ppt" . "wpp")
                              ("docx" . "wps")
                              ("xlsx" . "et")
                              ("pptx" . "wpp")
                              ))

;; settings for shfmt
;; fix zsh shfmt format
(use-package sh-script
  :config
  (set-formatter! 'shfmt
    '("shfmt" "-ci"
      ("-i" "%d" (unless indent-tabs-mode tab-width))
      ("-ln" "%s" (pcase sh-shell (`bash "bash") (`zsh "bash") (`mksh "mksh") (_ "posix")))))
  )


;; customize eshell
(after! eshell
  (eshell-git-prompt-use-theme 'simple)
  )


;; magit settings
(setq auto-revert-check-vc-info t)

;; vterm settings
(
 after! vterm
 (setq vterm-max-scrollback 10000)
 (remove-hook 'vterm-mode-hook 'hide-mode-line-mode)
 )
;; docker settings
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


;; markdwon preview use grip-mode
(use-package grip-mode)

(map! :map markdown-mode-map
        :localleader
        "g" #'grip-mode
        )

(map! :map org-mode-map
        :localleader
        "G" #'grip-mode
        )
