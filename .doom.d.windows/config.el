(setq user-full-name "Jie Samb"
      user-mail-address "samb233@hotmail.com")

(prefer-coding-system 'utf-8-unix)
(if (eq system-type 'windows-nt)
    (progn
      (set-selection-coding-system 'utf-16le-dos)
      (setq file-name-coding-system 'gb18030)
      (setq tramp-mode nil)
      (setq shell-file-name "bash")
      (setq explicit-shell-file-name "bash")
      (setq default-process-coding-system '(utf-8 . gbk)))
  (set-selection-coding-system 'utf-8))

(setenv "PATH" (concat "d:/Env/msys64/usr/bin;" (getenv "PATH")))
(add-to-list 'exec-path "d:\\Env\\msys64\\usr\\bin")

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat "emacs: "
                           (abbreviate-file-name (buffer-file-name))
                           (if (buffer-modified-p) "*"))
                 "emacs"))))

(setq auth-source-save-behavior nil)

(remove-hook! 'doom-after-init-hook #'doom-display-benchmark-h)

(setq uniquify-buffer-name-style 'forward)

(setq native-comp-jit-compilation nil)

(setq doom-font (font-spec :family "Consolas" :size 11.5))
;; (setq doom-unicode-font (font-spec :family "BlexMono Nerd Font"))
(setq doom-variable-pitch-font (font-spec :family "霞鹜文楷等宽"))

(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "霞鹜文楷等宽"))))

(add-hook 'after-setting-font-hook #'my-cjk-font)

(defun my-set-frame-font (&optional frame)
  "Set font size based on display size and pixel size."
  (let ((mm-size (alist-get 'mm-size (frame-monitor-attributes frame)))
        (pixel-size (nthcdr 2 (frame-monitor-geometry))))

      ;; 4k, 16:10, 16 inch
      (when (and (equal pixel-size '(3840 2400)) (equal mm-size '(344 215)))
        (setq doom-font (font-spec :family "Consolas" :size 11.5))
        (set-face-attribute
         'help-key-binding nil :font (font-spec :family "Consolas" :size 11.5))
        (internal-set-lisp-face-attribute
         'default :font (font-spec :family "Consolas" :size 11.5)))

      ;; 4k, 16:9, 27 inch
      (when (and (equal pixel-size '(3840 2160)) (equal mm-size '(597 336)))
        (setq doom-font (font-spec :family "Consolas" :size 9.0))
        (set-face-attribute
         'help-key-binding nil :font (font-spec :family "Consolas" :size 9.0))
        (internal-set-lisp-face-attribute
         'default :font (font-spec :family "Consolas" :size 9.0)))))

(defun my-frame-moved-monitors (frame)
  "Hook func to set font size based on display size and pixel size."
  (when (frame-size-changed-p)
    (my-set-frame-font frame)))
(push 'my-frame-moved-monitors window-size-change-functions)

(setq doom-theme 'doom-tomorrow-day)

(after! doom-modeline
  (setq doom-modeline-modal nil
        doom-modeline-icon nil
        doom-modeline-lsp nil
        doom-modeline-buffer-state-icon nil
        doom-modeline-buffer-modification-icon nil
        doom-modeline-buffer-encoding t
        doom-modeline-vcs-max-length 20
        doom-modeline-height 32
        doom-modeline-bar-width 6
        doom-modeline-window-width-limit 120))

(after! solaire-mode
 (dolist (face '(mode-line mode-line-inactive))
    (setf (alist-get face solaire-mode-remap-alist) nil)))

(setq word-wrap-by-category t)

(setq mouse-wheel-progressive-speed nil
      scroll-preserve-screen-position nil)
(setq mouse-wheel-scroll-amount
      '(3
        ((shift) . hscroll)
        ((meta))
        ((control) . text-scale)))

(pixel-scroll-precision-mode t)

(map! :n "<mouse-8>" #'better-jumper-jump-backward
      :n "<mouse-9>" #'better-jumper-jump-forward)

(map! :ig  "C-v"  #'yank
      :ig  "M-v"  #'yank
      :v  "J"     #'drag-stuff-down
      :v  "K"     #'drag-stuff-up
      :ni "C-s"   #'consult-line
      :ni "C-z"   #'undo-only
      :ni "C-S-z" #'undo-redo
      :nv "g r"   #'+lookup/references
      :n  "q"     #'doom/escape
      :n  "U"     #'evil-redo
      :n  "g a"   #'avy-goto-char-2
      :n  "] e"   #'flymake-goto-next-error
      :n  "[ e"   #'flymake-goto-prev-error
      :n  "] w"   #'evil-window-right
      :n  "[ w"   #'evil-window-left
      :n  "[ W"   #'evil-window-down
      :n  "] W"   #'evil-window-up
      :v  "<mouse-3>" #'kill-ring-save
      :leader
      :desc "jump to references" "c r" #'+lookup/references
      ;; :desc "consult buffer" "<" #'consult-buffer
      :desc "consult buffer other window" "w ," #'consult-buffer-other-window
      :desc "dired jump" ">" #'dired-jump
      :desc "find-file other window" "w ." #'find-file-other-window
      :desc "format buffer" "b f" #'+format/buffer
      :desc "toggle format-all" "t f" #'format-all-mode
      :desc "bookmark list" "b w" #'list-bookmarks
      :desc "bookmark jump other window" "b o" #'bookmark-jump-other-window)

(map! :map evil-ex-search-keymap
      "C-v" #'yank
      "C-q" #'quoted-insert)

(map! :map vertico-map
      :g "C-<return>" #'exit-minibuffer)

(map! :leader
      (:prefix ("v" . "my personal bindings")
       :desc "Open dirvish" "v" #'dirvish
       :desc "Toggle dirvish-side" "s" #'dirvish-side
       :desc "Fd in dirvish" "F" #'dirvish-fd-ask
       :desc "Jump using fd" "J" #'dirvish-fd-jump
       :desc "Jump recent dir" "j" #'consult-dir
       :desc "Fd find file in dir" "f" #'+vertico/consult-fd
       :desc "find Item in the buffer" "i" #'consult-imenu
       :desc "open with other coding system" "c" #'revert-buffer-with-coding-system
       :desc "change buffer coding system" "C" #'set-buffer-file-coding-system
       :desc "List processes" "l" #'list-processes
       :desc "toggle eldoc buffer" "h" #'eldoc
       :desc "VC Refresh state" "r" #'vc-refresh-state))

(map! :leader
      "i e" nil
      "f c" nil
      "n d" nil
      "f e" nil
      "f E" nil
      "f p" nil
      "f P" nil
      "f u" nil
      "f U" nil
      "b u" nil
      "f l" nil
      "s e" nil
      "s t" nil
      "h g" nil)

(evil-ex-define-cmd "q" 'kill-this-buffer)
(evil-ex-define-cmd "Q" 'kill-this-buffer)
(evil-ex-define-cmd "quit" 'evil-quit)
(evil-ex-define-cmd "W" 'save-buffer)

(setq undo-no-redo t)
(setq evil-want-fine-undo t)
(setq evil-undo-system 'undo-redo
      evil-undo-function 'undo-only
      evil-redo-function 'undo-redo)

(setq undo-limit 400000           ; 400kb (default is 160kb)
      undo-strong-limit 3000000   ; 3mb   (default is 240kb)
      undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

(add-hook! 'doom-first-buffer-hook #'global-undo-fu-session-mode)

(use-package! projectile
  :commands (project-projectile))

;; (defun projectile-root-default-directory (dir)
;;   "Retrieve the root directory of the project at DIR using `default-directory'."
;;   default-directory)

(after! projectile
  (add-to-list 'projectile-project-root-files "go.mod")
  (setq projectile-project-root-functions '(projectile-root-local
                                            projectile-root-marked
                                            projectile-root-top-down
                                            projectile-root-bottom-up
                                            projectile-root-top-down-recurring)))

(setq project-find-functions '(project-projectile project-try-vc))
(setq xref-search-program 'ripgrep)

(after! recentf
  (setq recentf-max-saved-items 1000))

(add-hook 'kill-emacs-hook #'recentf-cleanup -10)

(setq magit-clone-default-directory "D:/Codes/Lab/")

(add-hook! 'better-jumper-post-jump-hook #'recenter)

(defun recenter-advice (&rest args)
  (if (> (count-lines (point) (point-max)) 1)
      (recenter)))

(advice-add #'find-file :after #'recenter-advice)
(advice-add #'evil-goto-line :after #'recenter-advice)
(advice-add #'org-roam-node-find :after #'recenter-advice)

(evil-define-key 'visual 'global
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg
  "N" #'+multiple-cursors/evil-mc-toggle-cursor-here)

(evil-define-key 'normal 'evil-mc-key-map
  "Q" #'evil-mc-undo-all-cursors)

(use-package! doom-lookup-other-window
  :config
  (map! :nv "g D" #'+lookup/definition-other-window
        :nv "g R" #'+lookup/references-other-window
        :leader
        :desc "jump to reference other window" "c R" #'+lookup/references-other-window
        :desc "jump to definition other window" "c D" #'+lookup/definition-other-window))

(defun disable-y-or-n-p (orig-fun &rest args)
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (prompt) t)))
    (apply orig-fun args)))
(advice-add 'ediff-quit :around #'disable-y-or-n-p)

(add-hook! 'ediff-startup-hook #'ediff-next-difference)

(use-package! pixel-scroll)

(defun my-inline-image-pixel-scroll(&rest args)
  (setq-local evil-move-beyond-eol t
              pixel-scroll-precision-mode t))

(defun my-disable-inline-image-pixel-scroll(&rest args)
  (setq-local evil-move-beyond-eol nil
              pixel-scroll-precision-mode nil))

(after! markdown-mode
(advice-add 'markdown-display-inline-images :after #'my-inline-image-pixel-scroll)
(advice-add 'markdown-remove-inline-images :after #'my-disable-inline-image-pixel-scroll))

(after! org
(advice-add 'org-display-inline-images :after #'my-inline-image-pixel-scroll)
(advice-add 'org-remove-inline-images :after #'my-disable-inline-image-pixel-scroll))

(after! eglot
  (setq eglot-events-buffer-size 0)
  (setq eglot-send-changes-idle-time 0.1)
  (setq eglot-stay-out-of '(yasnippet))
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (map! :map eglot-mode-map
        :nv "g D" nil
        :leader
        :desc "LSP start/restart" "c l" #'eglot
        :desc "LSP reconnect" "c L" #'eglot-reconnect
        :desc "LSP rename" "c n" #'eglot-rename)
  (set-popup-rule! "^\\*eglot-help" :size 0.3 :quit t :select nil)
  (set-face-attribute 'eglot-highlight-symbol-face nil :background "#d6d4d4"))

(defun my-remove-eglot-mode-line()
  "Remove `eglot' from mode-line"
  (setq mode-line-misc-info
            (delq (assq 'eglot--managed-mode mode-line-misc-info) mode-line-misc-info)))
(add-hook 'eglot-managed-mode-hook #'my-remove-eglot-mode-line)

(after! corfu-popupinfo
  (setq corfu-popupinfo-delay nil))

(after! corfu
  (setq corfu-preselect 'prompt
        corfu-auto-delay 0.02
        corfu-on-exact-match nil
        corfu-popupinfo-max-height 20
        corfu-separator 32
        corfu-count 10)
  (map! :map corfu-map
        :i "C-j" #'corfu-next
        :i "C-k" #'corfu-previous
        :i "C-i" #'corfu-insert-separator
        :i "C-s" #'corfu-insert-separator
        :i "C-h" #'corfu-info-documentation
        :i "C-l" #'corfu-complete
        :i "C-g" #'corfu-quit)
  (map! :i "C-S-p" #'cape-file)
  (add-hook! 'evil-insert-state-exit-hook #'corfu-quit)
  (set-face-attribute 'corfu-current nil :background "#cde1f8"))

(use-package! flymake
  :commands (flymake-mode)
  :hook ((prog-mode text-mode conf-mode) . flymake-mode)
  :config
  (setq flymake-no-changes-timeout 0.1)
  (setq flymake-fringe-indicator-position 'right-fringe)
  (set-popup-rule! "^\\*format-all-errors*" :size 0.15 :select nil :modeline nil :quit t)
  (set-popup-rule! "^\\*Flymake diagnostics" :size 0.2 :modeline nil :quit t :select nil))

(after! eldoc
  (setq eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-prefer-doc-buffer t)
  (set-face-attribute 'eldoc-highlight-function-argument nil :background "#cde1f8")
  (set-popup-rule! "^\\*eldoc*" :size 0.15 :modeline nil :quit t))

;; (defun my-corfu-frame-visible-h ()
;;   (and (frame-live-p corfu--frame) (frame-visible-p corfu--frame)))
;; (add-hook 'yas-keymap-disable-hook #'my-corfu-frame-visible-h)

(use-package dabbrev
  :config
  (setq dabbrev-abbrev-char-regexp "[A-Za-z-_]"))

(setq completion-ignore-case t)

(after! dired
  (setq dired-dwim-target t
        dired-hide-details-hide-symlink-targets nil
        dired-recursive-copies  'always
        dired-recursive-deletes 'always
        dired-create-destination-dirs 'ask
        delete-by-moving-to-trash t
        dired-clean-confirm-killing-deleted-buffers nil
        dired-kill-when-opening-new-dired-buffer t)
  (setq dired-async-skip-fast t)
  (setq dired-omit-files
        (concat "\\`[.][.]?\\'"
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (setq ls-lisp-dirs-first t
        ls-lisp-verbosity nil
        ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")
        ls-lisp-use-localized-time-format t)
  (map! :map dired-mode-map
        :ng "l" #'dired-find-file
        :ng "h" #'dired-up-directory
        :ng "." #'dired-omit-mode
        :ng "a" #'dirvish-quick-access
        :ng "<mouse-1>" nil
        :ng "<mouse-2>" nil
        :ng "<mouse-3>" #'dired-find-file
        :ng "<mouse-8>" #'dired-up-directory
        :ng "<mouse-9>" #'dired-find-file
        :ng "<double-mouse-1>" #'dired-find-file
        :ng "<double-mouse-3>" #'dired-up-directory)
  (add-hook! 'wdired-mode-hook #'evil-insert-state)
  (custom-set-faces '(dired-async-message ((t (:inherit success))))))

(setq consult-find-args "find . -not ( -wholename \\*/.\\* -prune )")

(use-package! dirvish-quick-access
  :commands (dirvish-quick-access)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                 "Home")
     ("c" "D:/Codes/"          "Codes")
     ("w" "D:/Works/"          "Works")
     ("d" "D:/"                "D")
     ("P" "D:/Pictures/"       "Pictures")
     ("v" "D:/VCBs/"           "Videos")
     ("n" "D:/Notes/"          "Notes")
     ("b" "D:/Books/"          "Books"))))

(defun my-open-explorer()
  (interactive)
  (call-process-shell-command "explorer ." nil 0))

(map! [f9] #'my-open-explorer
      :leader "o e" #'my-open-explorer)

(defun my-open-windows-terminal-project()
  (interactive)
  (call-process-shell-command
   (format "wt -d %s" (or (doom-project-root) default-directory)) nil 0))

(defun my-open-windows-terminal-directory()
  (interactive)
  (call-process-shell-command
   (format "wt -d %s" default-directory) nil 0))

(map! [f4] #'my-open-windows-terminal-project
      [S-f4] #'my-open-windows-terminal-directory
      :leader
      "o t" #'my-open-windows-terminal-project
      "o T" #'my-open-windows-terminal-directory)

(setq org-directory "D:/Notes")
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
  (setq org-src-preserve-indentation nil
        org-image-actual-width 1280
        org-hide-emphasis-markers t
        org-support-shift-select t)
  (map! :map org-mode-map
        :localleader "-" #'org-emphasize))

(after! evil-org
  (map! :map evil-org-mode-map
        :i "C-l" nil
        :i "C-h" nil
        :i "C-j" nil
        :i "C-k" nil))

(use-package! org-modern
  :commands (org-modern-mode)
  :config
  (setq org-modern-block-name nil
        org-modern-table nil)
  (setq org-modern-star '("◉" "○" "✸" "✿" "◈" "◇"))
  (set-face-attribute 'org-modern-label nil :height 1.0))

(add-hook 'org-mode-hook #'org-modern-mode)

(use-package! org-appear
  :commands (org-appear-mode)
  :init
  (setq org-appear-autolinks t))

(add-hook 'org-mode-hook #'org-appear-mode)

(setq org-roam-directory "D:/Notes/Roam")

(after! org-roam
  (setq org-roam-completion-everywhere nil))

(setq org-roam-dailies-directory "D:/Notes/Daily")
(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y>/%<%Y-%m>/%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))
(map! :leader
      :desc "my Journal today" "J" #'org-roam-dailies-goto-today
      :desc "org-roam find node" "Z" #'org-roam-node-find)

(setq org-roam-capture-templates '(
          ("d" "Default" plain "%?"
          :target (file+head "Default/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: \n\n")
          :unnarrowed t)
          ("l" "Learn" plain "%?"
          :target (file+head "Learn/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :learn: \n\n")
          :unarrowed t)
          ("t" "Think" plain "%?"
          :target (file+head "Think/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :think: \n\n")
          :unnarrowed t)
          ("c" "Create" plain "%?"
          :target (file+head "Create/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags: :create: \n\n")
          :unnarrowed t)))

(map! :leader "A" (lambda () (interactive) (org-agenda nil "n")))

(after! org-agenda
  (set-popup-rule! "^\\*Org Agenda" :side 'right :size 0.25 :quit t :select t :modeline nil))

(custom-set-faces
 '(markdown-code-face ((t (:background "#f5f5f5"))))
 '(markdown-header-delimiter-face ((t (:foreground "#616161" :height 0.9))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.3 :foreground "#4271ae" :weight ultra-bold))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.2 :foreground "#8959a8" :weight extra-bold))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.1 :foreground "#718c00" :weight bold))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.0 :foreground "#eab700" :weight semi-bold))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.0 :foreground "#c82829" :weight normal))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0 :foreground "#70c0ba" :weight normal))))
 '(markdown-header-face-7 ((t (:inherit markdown-header-face :height 1.0 :foreground "#b77ee0" :weight normal)))))

(add-hook! 'markdown-mode-hook (setq-local markdown-fontify-code-blocks-natively t))
(after! markdown-mode
  (setq markdown-fontify-whole-heading-line nil)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-max-image-size '(1280 . 960))
  (map! :map markdown-mode-map :n "z i" #'markdown-toggle-inline-images)
  (set-popup-rule! "^\\*edit-indirect" :size 0.42 :quit nil :select t :autosave t :modeline t :ttl nil))

(defun my-eglot-organize-imports ()
  (ignore-errors(call-interactively 'eglot-code-action-organize-imports)))
(defun my-go-mode-init ()
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

(use-package dockerfile-mode
  :commands (dockerfile-mode)
  :mode("\\Dockerfile\\'" . dockerfile-mode))

(after! org
  (add-to-list 'org-src-lang-modes '("py" . python-mode)))

(after! markdown-mode
  (add-to-list 'markdown-code-lang-modes '("py" . python-mode)))

(after! python
  (setq python-shell-interpreter "python")
  (setenv "PYTHONIOENCODING" "utf-8"))

(setq-hook! 'python-mode-hook eglot-workspace-configuration
            '(:python.analysis (:autoSearchPaths t
                                :useLibraryCodeForTypes t
                                :typeCheckingMode "basic"
                                :diagnosticMode "openFilesOnly")))

(after! apheleia
  (push '(ruff . ("ruff" "format"
             "--silent"
             (apheleia-formatters-fill-column "--line-length")
             "--stdin-filename" filepath
             "-"))
      apheleia-formatters)

  (setf (alist-get 'python-mode apheleia-mode-alist)
      '(ruff)))

(add-to-list 'auto-mode-alist '("\\.vpy\\'" . python-mode))

(defun vspreview()
  "Vapoursynth preview this script."
  (interactive)
  (async-shell-command
   (format "D:/Env/vapoursynth/python.exe -m vspreview %s" (shell-quote-argument buffer-file-name))
   "*vspreview*"))

(defun vsbench()
  "Vapoursynth bench this script."
  (interactive)
  (async-shell-command
   (format "D:/Env/vapoursynth/VSPipe.exe -p %s ." (shell-quote-argument buffer-file-name))
   "*vsbench*"))

(map! :map python-mode-map
        :localleader
        "p" #'vspreview
        "b" #'vsbench)

(set-popup-rule! "^\\*vspreview*" :size 0.2 :quit t :select nil)
(set-popup-rule! "^\\*vsbench*" :size 0.2 :quit t :select nil)

(defun my-open-current-file-with-app()
  (interactive)
  (progn
    (dirvish-find-entry-a buffer-file-name)
    (quit-window)))

(map! :map image-mode-map
      :ng "W" #'my-open-current-file-with-app
      "<double-mouse-1>" #'my-open-current-file-with-app)

(after! apheleia
  (setf (alist-get 'rustfmt apheleia-formatters)
      '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2021")))

(setenv "PATH" (concat (getenv "PATH") ";d:/Env/omnisharp/" ))
(add-to-list 'exec-path "d:\\Env\\omnisharp")

(use-package! sis
  :config
  (setq sis-respect-prefix-and-buffer nil)
  (sis-ism-lazyman-config nil t 'w32)
  (add-hook! 'after-init-hook #'sis-set-english)
  (sis-global-respect-mode t))

(use-package! tabspaces
  :hook (doom-init-ui . tabspaces-mode)
  :commands (tabspaces-switch-or-create-workspace
             tabspaces-close-workspace)
  :init
  (setq tab-bar-show nil)
  (tab-rename "Default")
  :custom
  (tabspaces-use-filtered-buffers-as-default nil)
  (tabspaces-default-tab "Default")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*"))
  (tabspaces-session nil)
  (tabspaces-session-auto-restore nil)
  :config
  (map! :leader
        :desc "switch or create tab" "TAB" #'tab-bar-switch-to-tab
        :desc "close current tab" [backtab] #'tab-bar-close-tab))

(defun tabspaces-reset-advice()
  (switch-to-buffer "*scratch*"))

(advice-add #'tabspaces-reset-buffer-list :before #'tabspaces-reset-advice)

(after! consult
  ;; hide full buffer list (still available with "b" prefix)
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :history  'buffer-name-history
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda () (consult--buffer-query
                                :predicate #'tabspaces--local-buffer-p
                                :sort 'visibility
                                :as #'buffer-name)))

    "Set workspace buffer list for consult-buffer.")
  (add-to-list 'consult-buffer-sources 'consult--source-workspace))

(use-package! tab-bookmark
  :commands (tab-bookmark
             tab-bookmark-handler))

(map! :leader
      :desc "Bookmark Tab" "v m" #'tab-bookmark)

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

(defun my-writeroom-mode-on()
  (if (equal major-mode 'org-mode)
      (org-display-inline-images))
  (if (member major-mode '(markdown-mode gfm-mode))
      (markdown-display-inline-images))
  (doom-disable-line-numbers-h))

(defun my-writeroom-mode-off()
  (if (equal major-mode 'org-mode)
      (org-remove-inline-images))
  (if (member major-mode '(markdown-mode gfm-mode))
      (markdown-remove-inline-images))
  (doom-enable-line-numbers-h))

(add-hook 'writeroom-mode-on-hook #'my-writeroom-mode-on)
(add-hook 'writeroom-mode-off-hook #'my-writeroom-mode-off)

(use-package! fringe-scale
  :init
  (set-fringe-mode 16)
  :config
  (fringe-scale-setup))

(setq builtin-bitmaps
      ' ((question-mark [#x3c #x7e #xc3 #xc3 #x0c #x18 #x18 #x00 #x18 #x18])
     (exclamation-mark [#x18 #x18 #x18 #x18 #x18 #x18 #x18 #x00 #x18 #x18])
     (left-arraw [#x18 #x30 #x60 #xfc #xfc #x60 #x30 #x18])
     (right-arrow [#x18 #x0c #x06 #x3f #x3f #x06 #x0c #x18])
     (up-arrow [#x18 #x3c #x7e #xff #x18 #x18 #x18 #x18])
     (down-arrow [#x18 #x18 #x18 #x18 #xff #x7e #x3c #x18])
     (left-curly-arrow [#x3c #x7c #xc0 #xe4 #xfc #x7c #x3c #x7c])
     (right-curly-arrow [#x3c #x3e #x03 #x27 #x3f #x3e #x3c #x3e])
     (left-triangle [#x03 #x0f #x1f #x3f #x3f #x1f #x0f #x03])
     (right-triangle [#xc0 #xf0 #xf8 #xfc #xfc #xf8 #xf0 #xc0])
     (top-left-angle [#xfc #xfc #xc0 #xc0 #xc0 #xc0 #xc0 #x00])
     (top-right-angle [#x3f #x3f #x03 #x03 #x03 #x03 #x03 #x00])
     (bottom-left-angle [#x00 #xc0 #xc0 #xc0 #xc0 #xc0 #xfc #xfc])
     (bottom-right-angle [#x00 #x03 #x03 #x03 #x03 #x03 #x3f #x3f])
     (left-bracket [#xfc #xfc #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xfc #xfc])
     (right-bracket [#x3f #x3f #x03 #x03 #x03 #x03 #x03 #x03 #x3f #x3f])
     (filled-rectangle [#xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe #xfe])
     (hollow-rectangle [#xfe #x82 #x82 #x82 #x82 #x82 #x82 #x82 #x82 #x82 #x82 #x82 #xfe])
     (hollow-square [#x7e #x42 #x42 #x42 #x42 #x7e])
     (filled-square [#x7e #x7e #x7e #x7e #x7e #x7e])
     (vertical-bar [#xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0 #xc0])
     (horizontal-bar [#xfe #xfe])))

(dolist (bitmap builtin-bitmaps)
  (define-fringe-bitmap (car bitmap) (cadr bitmap)))

(use-package! flymake-triangle-bitmap
  :after flymake
  :config
  (setq flymake-note-bitmap    '(my-small-left-triangle compilation-info)
        flymake-error-bitmap   '(my-small-left-triangle compilation-error)
        flymake-warning-bitmap '(my-small-left-triangle compilation-warning)))

(use-package! base64-img-toggle
  :commands (base64-img-toggle-region))

(set-popup-rule! "^\\*base64-img-toggle" :size 0.15 :modeline nil :quit t)
(map! :leader
      :desc "View Base64 img" "v b" #'base64-img-toggle-region)
