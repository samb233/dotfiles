(setq user-full-name "Jie Samb"
      user-mail-address "samb233@hotmail.com")

(prefer-coding-system 'utf-8-unix)

(if (eq system-type 'windows-nt)
    (progn
      (setq tramp-mode nil)
      (setenv "LANG" "zh_CN.UTF-8")
      (setq shell-file-name "bash"
            explicit-shell-file-name "bash")
      ;; (setq default-process-coding-system '(utf-8 . utf-8))

      ;; not needed if use utf-8-beta
      ;; (setq default-process-coding-system '(utf-8 . cp936))
      ;; (setq file-name-coding-system 'cp936)
      (set-selection-coding-system 'utf-16le-dos))
  (set-selection-coding-system 'utf-8))

(setenv "PATH" (concat "d:/Env/msys2/usr/bin;" (getenv "PATH")))
(add-to-list 'exec-path "d:\\Env\\msys2\\usr\\bin")

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat "Editor Macross >> "
                           (abbreviate-file-name (buffer-file-name))
                           (if (buffer-modified-p) " *"))
                 (if (equal major-mode #'dired-mode)
                     (concat "Editor Macross >> " default-directory)
                   "Editor Macross")))))

(setq auth-source-save-behavior nil)

(remove-hook! 'doom-after-init-hook #'doom-display-benchmark-h)

(setq uniquify-buffer-name-style 'forward)

(setq native-comp-jit-compilation nil)

(setq word-wrap-by-category t)

(setq doom-font (font-spec :family "Consolas" :size 11.5))
;; (setq doom-unicode-font (font-spec :family "BlexMono Nerd Font"))
(setq doom-variable-pitch-font (font-spec :family "霞鹜文楷等宽"))

(defun my-cjk-font()
  (dolist (charset '(kana han cjk-misc symbol bopomofo))
    (set-fontset-font t charset (font-spec :family "霞鹜文楷等宽"))))

(add-hook 'after-setting-font-hook #'my-cjk-font)

(setq display-line-numbers-type 'relative)

(setq doom-theme 'doom-tomorrow-day)

(use-package! doom-light-modeline-enhance)
(setq +modeline-height 24
      +modeline-bar-width 4)
(set-face-attribute 'mode-line nil :background "#eef4f9")
(remove-hook '+popup-buffer-mode-hook #'+popup-set-modeline-on-enable-h)

(after! solaire-mode
  (dolist (face '(mode-line mode-line-inactive))
    (setf (alist-get face solaire-mode-remap-alist) nil)))

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

(map! :ig "C-v"       #'yank
      :ig "M-v"       #'yank
      :ig "M-p"       #'yas-insert-snippet
      :nv "C-/"       #'comment-line
      :v  "J"         #'drag-stuff-down
      :v  "K"         #'drag-stuff-up
      :nv "R"         #'query-replace
      :ni "C-s"       #'consult-line
      :ni "C-z"       #'undo-only
      :ni "C-S-z"     #'undo-redo
      :nv "g r"       #'+lookup/references
      :ng "<super>"   #'ignore
      :n  "q"         #'doom/escape
      :n  "U"         #'evil-redo
      :n  "s"         #'avy-goto-char-2
      :n  "] e"       #'flymake-goto-next-error
      :n  "[ e"       #'flymake-goto-prev-error
      :n  "] w"       #'evil-window-next
      :n  "[ w"       #'evil-window-prev
      :v  "<mouse-3>" #'kill-ring-save
      :leader
      :desc "consult buffer other window" "w ," #'consult-buffer-other-window
      :desc "find-file other window"      "w ." #'find-file-other-window
      :desc "vertico project in cwd" "?" #'+vertico/project-search-from-cwd
      :desc "dired jump" ">" #'dired-jump
      :desc "jump to references" "c r" #'+lookup/references
      :desc "format buffer" "b f" #'+format/buffer
      :desc "bookmark list" "b w" #'list-bookmarks
      :desc "start eglot" "c l" #'eglot
      :desc "consult compile errors" "c X" #'consult-compile-error)

(map! :after evil-snipe
      (:map evil-snipe-local-mode-map
       :mn "s" nil
       :mn "S" nil))

(defun avy-goto-char-2-all-window()
  (interactive)
  (let ((avy-all-windows t))
    (call-interactively 'avy-goto-char-2)))

(map! :n "S" #'avy-goto-char-2-all-window)

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
       :desc "Fd find file in dir" "f" #'+vertico/consult-fd-or-find
       :desc "find Item in the buffer" "i" #'consult-imenu
       :desc "open with other coding system" "c" #'revert-buffer-with-coding-system
       :desc "change buffer coding system" "C" #'set-buffer-file-coding-system
       :desc "List processes" "l" #'list-processes
       :desc "toggle eldoc buffer" "h" #'eldoc
       :desc "VC Refresh state" "r" #'vc-refresh-state))

(map! :leader
      "i e" nil
      "n d" nil
      "b u" nil
      "s e" nil
      "s t" nil
      "h g" nil)

(evil-ex-define-cmd "q" 'kill-current-buffer)
(evil-ex-define-cmd "Q" 'kill-current-buffer)
(evil-ex-define-cmd "qa" 'evil-quit)
(evil-ex-define-cmd "W" 'save-buffer)

(use-package! drag-stuff
  :commands (drag-stuff-up
             drag-stuff-down)
  :init
  (map! :v "K"  #'drag-stuff-up
        :v "J"  #'drag-stuff-down))

(setq undo-no-redo t)
(setq evil-want-fine-undo t)
(setq evil-undo-system 'undo-redo
      evil-undo-function 'undo-only
      evil-redo-function 'undo-redo)

(setq undo-limit 400000           ; 400kb (default is 160kb)
      undo-strong-limit 3000000   ; 3mb   (default is 240kb)
      undo-outer-limit 48000000)  ; 48mb  (default is 24mb)

(add-hook! 'doom-first-buffer-hook #'global-undo-fu-session-mode)

(defun projectile-root-default-directory (dir)
  "Retrieve the root directory of the project at DIR using `default-directory'."
  default-directory)

(after! projectile
  (add-to-list 'projectile-project-root-files "go.mod")
  (setq projectile-project-root-functions '(projectile-root-local
                                            projectile-root-marked
                                            projectile-root-top-down
                                            projectile-root-bottom-up
                                            projectile-root-default-directory
                                            projectile-root-top-down-recurring)))

(setq project-find-functions '(project-projectile project-try-vc))
(setq xref-search-program 'ripgrep)

(after! recentf
  (setq recentf-max-saved-items 1000
        recentf-auto-cleanup 'mode)
  (remove-hook 'kill-emacs-hook #'recentf-cleanup))

(setq magit-clone-default-directory "D:/Codes/Lab/")

(add-hook! 'better-jumper-post-jump-hook #'recenter)

(defun recenter-advice (&rest args)
  (if (> (count-lines (point) (point-max)) 1)
      (recenter)))

(advice-add #'find-file :after #'recenter-advice)
(advice-add #'evil-goto-line :after #'recenter-advice)
(advice-add #'org-roam-node-find :after #'recenter-advice)

(map! :n "m" #'evil-mc-make-and-goto-next-match
      :n "M" #'evil-mc-make-and-goto-prev-match
      :v "A" #'evil-mc-make-cursor-in-visual-selection-end
      :v "I" #'evil-mc-make-cursor-in-visual-selection-beg
      :v "N" #'+multiple-cursors/evil-mc-toggle-cursor-here)

(after! evil-mc
  (add-hook 'evil-mc-after-cursors-deleted #'evil-ex-nohighlight))

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

(after! eglot
  (setq eglot-events-buffer-size 0)
  (setq eglot-send-changes-idle-time 0.2)
  (setq eglot-stay-out-of '(yasnippet))
  (map! :map 'eglot-mode-map
        :nv "g D" nil
        :leader
        :desc "LSP start/restart" "c l" #'eglot
        :desc "LSP reconnect" "c L" #'eglot-shutdown
        :desc "LSP rename" "c n" #'eglot-rename)
  (set-popup-rule! "^\\*eglot-help" :size 0.3 :quit t :select nil)
  (push :inlayHintProvider eglot-ignored-server-capabilities)
  (set-face-attribute 'eglot-inlay-hint-face nil :weight 'bold :height 0.9))

(defun my-remove-eglot-mode-line()
  "Remove `eglot' from mode-line"
  (setq mode-line-misc-info
            (delq (assq 'eglot--managed-mode mode-line-misc-info) mode-line-misc-info)))
(add-hook 'eglot-managed-mode-hook #'my-remove-eglot-mode-line)

(setq +lsp-defer-shutdown nil)

(use-package! eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(defvar +eglot-server-num-limit 8)

(after! eglot
  (defun +eglot-get-server-num()
    (let ((servers (cl-loop for servers
                            being hash-values of eglot--servers-by-project
                            append servers))
          (name (lambda (srv)
                  (format "%s %s" (eglot-project-nickname srv)
                          (eglot--major-modes srv)))))
      (mapcar name servers)))

  (defun +eglot-limit-server-num()
    (let ((num (length (+eglot-get-server-num))))
      (when (> num +eglot-server-num-limit)
        (run-at-time 0.1 nil (lambda () (call-interactively #'eglot-shutdown))))))

  (add-hook 'eglot--managed-mode-hook #'+eglot-limit-server-num))

(after! corfu
  (setq corfu-preselect 'prompt
        corfu-auto-delay 0.02
        corfu-auto-prefix 1
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
  (add-hook 'conf-mode-hook #'+corfu-add-cape-file-h)
  (set-face-attribute 'corfu-current nil :background "#cde1f8"))

(after! corfu-popupinfo
  (setq corfu-popupinfo-delay nil))

(setq-hook! 'minibuffer-setup-hook corfu-auto-prefix 2)

(setq thing-at-point-file-name-chars
      (concat thing-at-point-file-name-chars " ・()（）Z-a！+&"))

(after! flymake
  (set-popup-rule! "^\\*format-all-errors*" :size 0.15 :select nil :modeline nil :quit t)
  (set-popup-rule! "^\\*Flymake diagnostics" :size 0.2 :modeline nil :quit t :select nil)
  (setq flymake-no-changes-timeout nil)

  ;; make eglot compatible with flymake-no-changes-timeout=nil
  (cl-defmethod eglot-handle-notification :after
    (_server (_method (eql textDocument/publishDiagnostics)) &key uri
             &allow-other-keys)
    (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
      (with-current-buffer buffer
        (if (and (eq nil flymake-no-changes-timeout)
                 (not (buffer-modified-p)))
            (flymake-start t)))))

  (setq-hook! 'org-src-mode-hook flymake-no-changes-timeout 0.2))

(after! eldoc
  (setq eldoc-echo-area-display-truncation-message nil
        eldoc-echo-area-use-multiline-p nil
        eldoc-echo-area-prefer-doc-buffer t)
  (set-face-attribute 'eldoc-highlight-function-argument nil :background "#cde1f8")
  (set-popup-rule! "^\\*eldoc*" :size 0.15 :modeline nil :quit t))

(use-package dabbrev
  :config
  (setq dabbrev-abbrev-char-regexp "[-_A-Za-z0-9]")
  (setq dabbrev-case-distinction nil)
  (setq dabbrev-case-replace nil))

(setq completion-ignore-case t)

(after! dired
  (setq dired-recursive-deletes 'always
        delete-by-moving-to-trash t)
  (setq ls-lisp-dirs-first t
        ls-lisp-verbosity nil
        ls-lisp-format-time-list '("%Y-%m-%d %H:%M" "%Y-%m-%d %H:%M")
        ls-lisp-use-localized-time-format t)
  (setq dired-omit-files
        (concat "\\`[.][.]?\\'"
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group --time-style=iso"))

(after! dired
  (add-hook 'wdired-mode-hook #'evil-normal-state -10))

(use-package! dirvish
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"                 "Home")
     ("c" "D:/Codes/"          "Codes")
     ("w" "D:/Works/"          "Works")
     ("d" "D:/"                "D")
     ("e" "E:/"                "E")
     ("P" "D:/Pictures/"       "Pictures")
     ("v" "D:/VCBs/"           "Videos")
     ("n" "D:/Notes/"          "Notes")
     ("b" "D:/Books/"          "Books")))
  :config
  ;;(dirvish-side-follow-mode 1)
  (add-to-list 'dirvish-video-exts "m2ts")
  (setq dirvish-side-width 40
        dirvish-side-auto-close t
        dirvish-side-display-alist `((side . right) (slot . -1)))
  (setq dirvish-use-mode-line nil
        dirvish-hide-details '(dirvish-side)
        dirvish-hide-cursor '(dirvish dirvish-side dired)
        dirvish-default-layout '(0 0 0.5)
        dirvish-path-separators (list "  ~" "   " "/")
        dirvish-header-line-format
        '(:left (path) :right (yank sort index " "))
        dirvish-open-with-programs
        `((,dirvish-audio-exts . ("D:/Applications/mpv/mpv.exe" "%f"))
          (,dirvish-video-exts . ("D:/Applications/mpv/mpv.exe" "%f"))
          (,dirvish-image-exts . ("D:/Applications/xnviewmp/xnviewmp.exe" "%f"))
          (("doc" "docx") . ("C:/Program Files/Microsoft Office/root/Office16/WINWORD.EXE" "%f"))
          (("ppt" "pptx") . ("C:/Program Files/Microsoft Office/root/Office16/POWERPNT.EXE" "%f"))
          (("xls" "xlsx") . ("C:/Program Files/Microsoft Office/root/Office16/EXCEL.EXE" "%f"))
          (("pdf") . ("C:/Program Files/SumatraPDF/SumatraPDF.exe" "%f"))
          (("epub") . ("D:/Applications/koodo/Koodo Reader.exe" "%f")))))

(setenv "PATH" (concat "d:/Env/media/poppler/bin/;" (getenv "PATH")))
(add-to-list 'exec-path "d:\\Env\\media\\poppler\\bin")
(setenv "PATH" (concat "d:/Env/media/imagemagick/;" (getenv "PATH")))
(add-to-list 'exec-path "d:\\Env\\media\\imagemagick")
(setenv "PATH" (concat "d:/Env/media/mtn/;" (getenv "PATH")))
(add-to-list 'exec-path "d:\\Env\\media\\mtn")

(after! dirvish
  (use-package! dirvish-windows))

(add-hook! 'dirvish-setup-hook
  (use-package! dirvish-video-mediainfo-enhance))

(after! diff-hl-dired
  (set-face-attribute 'diff-hl-dired-unknown nil :background "#ffffff" :foreground "#ffffff")
  (set-face-attribute 'diff-hl-dired-ignored nil :background "#c0bfbf" :foreground "#c0bfbf")
  (set-face-attribute 'diff-hl-dired-change nil :background "#f2d366")
  (set-face-attribute 'diff-hl-dired-delete nil :background "#c82829")
  (set-face-attribute 'diff-hl-dired-insert nil :background "#a9ba66"))

(if (eq system-type 'windows-nt)
    (after! diff-hl
      (setq diff-hl-update-async nil)
      (remove-hook 'diff-hl-flydiff-mode-hook #'+vc-gutter-init-flydiff-mode-h)
      (remove-hook 'diff-hl-mode-hook #'diff-hl-flydiff-mode)
      (advice-remove 'diff-hl-update #'+vc-gutter--debounce-threads-a)
      (advice-remove 'diff-hl-update-once #'+vc-gutter--only-tick-on-success-a)))

(defun dirvish-unfocus ()
  (interactive)
  (face-remap-add-relative 'dirvish-hl-line '(:background "#d6d4d4")))

(defun dirvish-focus ()
  (interactive)
  (face-remap-add-relative 'dirvish-hl-line '(:background "#4271ae")))

(defun dirvish-focus-change (&rest w)
  (let* ((cb (current-buffer))
         (ow (old-selected-window))
         (ob (window-buffer ow)))
    (progn
      (with-current-buffer cb
        (when (eq major-mode #'dired-mode)
          (dirvish-focus)))
      (with-current-buffer ob
        (when (eq major-mode #'dired-mode)
          (dirvish-unfocus))))))

(add-hook! 'dired-mode-hook
  (add-hook 'window-selection-change-functions #'dirvish-focus-change nil t))

(use-package! dired-7z
  :after dired
  :config
  (map! :map 'dired-mode-map
        :localleader
        "z" #'dired-7z-compress
        "Z" #'dired-7z-compress-with-password
        "e" #'dired-7z-extract))

(use-package! dired-windows-clipboard
  :after dired
  :config
  (map! :map 'dired-mode-map
        :localleader
        "c" #'dired-copy-file-to-windows-clipboard
        "v" #'dired-file-to-clipboard
        "p" #'dired-paste-file-from-windows-clipboard
        "i" #'dired-open-file-properties-windows))

(defun my-open-explorer()
  (interactive)
  (call-process-shell-command "explorer ." nil 0))

(map! [f9] #'my-open-explorer
      :leader "o e" #'my-open-explorer)

(defun dired-open-filename-at-point ()
  "Open `dired' to the filename at point."
  (interactive)
  (let* ((filepath (thing-at-point 'filename t))
         (dir (file-name-directory filepath)))
    (dired dir)))

(map! :leader "v o" #'dired-open-filename-at-point)

(defun dired-region (beg end)
  "Open `dired' according to the selected path within BEG and END."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (error "No selection (no active region)")))
  (let* ((filepath (buffer-substring beg end))
         (dir (file-name-directory filepath)))
    (dired dir)))

(map! :leader "v O" #'dired-region)

(setq eshell-banner-message "")
(use-package! doom-eshell-toggle)
(map! [f4] #'doom-eshell-toggle-project
      [S-f4] #'project-eshell
      :leader
      "o s" #'doom-eshell-toggle-project
      "o S" #'project-eshell)

(defvar +eshell-aliases
  '(("q"  "exit")           ; built-in
    ("f"  "find-file $1")
    ("ff" "find-file-other-window $1")
    ("d"  "dired $1")
    ("bd" "eshell-up $1")
    ("rg" "rg --color=always $*")
    ("l"  "ls -lh $*")
    ("ll" "ls -lah $*")
    ("git" "git --no-pager $*")
    ("gg" "magit-status")
    ("cdp" "cd-to-project")
    ("clear" "clear-scrollback")) ; more sensible than default
  "An alist of default eshell aliases, meant to emulate useful shell utilities")

(after! em-alias
  (setq +eshell--default-aliases eshell-command-aliases-list
        eshell-command-aliases-list
        (append eshell-command-aliases-list
                +eshell-aliases)))

(defun my-open-windows-terminal-project()
  (interactive)
  (call-process-shell-command
   (format "wt -d %s" (shell-quote-argument
                       (or (doom-project-root) default-directory))) nil 0))

(defun my-open-windows-terminal-directory()
  (interactive)
  (call-process-shell-command
   (format "wt -d %s" (shell-quote-argument
                       default-directory)) nil 0))

(map! [f4] #'my-open-windows-terminal-project
      [S-f4] #'my-open-windows-terminal-directory
      :leader
      "o t" #'my-open-windows-terminal-project
      "o T" #'my-open-windows-terminal-directory)

(map! :leader "S" #'shell-command
      :leader "A" #'async-shell-command)

(set-popup-rule! "^\\*Async Shell Command" :size 0.25 :quit 'current :select t :modeline t)

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

(map! :leader "L" (lambda () (interactive) (find-file (concat org-directory "/todo.org"))))

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

(setq eglot--managed-mode nil)
(defun my-eglot-organize-imports ()
  (when eglot--managed-mode
    (ignore-errors(call-interactively 'eglot-code-action-organize-imports))))
(defun my-eglot-format-buffer()
  (when eglot--managed-mode
    (eglot-format-buffer)))

(defun go-eldoc-setup())

(defun my-go-lsp-init ()
  (add-hook 'before-save-hook #'my-eglot-format-buffer -10 t)
  (add-hook 'before-save-hook #'my-eglot-organize-imports nil t))
(add-hook 'go-mode-hook #'my-go-lsp-init)

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

(defun mediainfo-region(beg end)
  "Show mediainfo for selected filename."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (error "No selection (no active region)")))
  (let ((filename (buffer-substring beg end)))
    (async-shell-command (format "mediainfo %s" (shell-quote-argument filename)) "*mediainfo*")))

(defun mediainfo-dired()
  "Show mediainfo for file current line."
  (interactive)
  (let ((filename (dired-get-filename)))
    (async-shell-command (format "mediainfo %s" (shell-quote-argument filename)) "*mediainfo*")))

(map! :map python-mode-map
      :localleader
      "p" #'vspreview
      "b" #'vsbench
      "m" #'mediainfo-region)

(map! :map dired-mode-map
      :localleader
      "m" #'mediainfo-dired)

(set-popup-rule! "^\\*vspreview*" :size 0.2 :quit t :select nil)
(set-popup-rule! "^\\*vsbench*" :size 0.2 :quit t :select nil)
(set-popup-rule! "^\\*mediainfo*" :size 0.4 :quit t :select nil)

(after! apheleia
  (setf (alist-get 'rustfmt apheleia-formatters)
      '("rustfmt" "--quiet" "--emit" "stdout" "--edition" "2021")))

(use-package! sis
  :config
  (setq sis-respect-prefix-and-buffer nil)
  (sis-ism-lazyman-config nil t 'w32)
  (add-hook! 'after-init-hook #'sis-set-english)
  (sis-global-respect-mode t)
  (sis-global-context-mode t))

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
  (map! :n "[ TAB" #'tab-previous
        :n "] TAB" #'tab-next
        :leader
        :desc "switch or create tab" "TAB" #'tab-bar-switch-to-tab
        :desc "close current tab" [backtab] #'tab-bar-close-tab))

(after! tabspaces
    (evil-ex-define-cmd "r" 'tabspaces-remove-current-buffer))

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
  :commands (tab-bookmark-save
             tab-bookmark-handler))

(map! :leader
      :desc "Bookmark Tab" "v m" #'tab-bookmark-save)

(use-package symbol-overlay
  :diminish
  :custom-face
  (symbol-overlay-default-face ((t (:inherit region :background unspecified :foreground unspecified))))
  (symbol-overlay-face-1 ((t (:inherit nerd-icons-blue :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-2 ((t (:inherit nerd-icons-pink :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit nerd-icons-yellow :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit nerd-icons-purple :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit nerd-icons-red :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit nerd-icons-orange :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit nerd-icons-green :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit nerd-icons-cyan :background unspecified :foreground unspecified :inverse-video t))))
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         ("M-N" . symbol-overlay-switch-forward)
         ("M-P" . symbol-overlay-switch-backward)
         ("M-C" . symbol-overlay-remove-all))
  :hook (((prog-mode yaml-mode) . symbol-overlay-mode)
         (iedit-mode            . turn-off-symbol-overlay)
         (iedit-mode-end        . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1))

(use-package! colorful-mode
  :commands (colorful-mode
             global-colorful-mode)
  :config (dolist (mode '(html-mode php-mode help-mode helpful-mode))
            (add-to-list 'global-colorful-modes mode)))

(map! :leader
      :desc "toggle colorful mode" "t s" #'colorful-mode)

(defun my-emacs-use-proxy()
  (interactive)
  (setenv "http_proxy" "http://127.0.0.1:17897")
  (setenv "https_proxy" "http://127.0.0.1:17897")
  (setenv "all_proxy" "socks5://127.0.0.1:17897")
  (message "Use Proxy"))

(defun my-emacs-not-use-proxy()
  (interactive)
  (setenv "http_proxy" "")
  (setenv "https_proxy" "")
  (setenv "all_proxy" "")
  (message "Not use Proxy"))

(map! :leader
      :desc "use proxy" "v p" #'my-emacs-use-proxy
      :desc "use proxy" "v P" #'my-emacs-not-use-proxy)

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

(use-package! base64-img-toggle
  :commands (base64-img-toggle-region))

(set-popup-rule! "^\\*base64-img-toggle" :size 0.15 :modeline nil :quit t)
(map! :leader
      :desc "View Base64 img" "v b" #'base64-img-toggle-region)

(use-package! fringe-scale
  :init
  (set-fringe-mode '(8 . 16))
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
