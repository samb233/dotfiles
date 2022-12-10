;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!
(setq-default buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; open at maximaized
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Jie Samb"
      user-mail-address "samb233@hotmail.com")


;; set fonts
;; (setq doom-font (font-spec :family "Sarasa Term SC" :size 14.0 ))
;; (setq doom-variable-pitch-font (font-spec :family "Sarasa Term SC"))
;; (setq doom-unicode-font (font-spec :family "Sarasa Term SC" ))

;; (setq doom-variable-pitch-font (font-spec :family "Sarasa Mono SC"))
;; (setq doom-font (font-spec :family "JetBrainsMono Nerd Font" :size 13.0 ))
(setq doom-unicode-font (font-spec :family "JetBrainsMono Nerd Font" ))

(defun +my/better-font()
  (interactive)
  ;; english font
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'default nil :font (format "%s:pixelsize=%d" "JetBrainsMono Nerd Font" 17)) ;; 11 13 17 19 23
        ;; chinese font
        (dolist (charset '(kana han symbol cjk-misc bopomofo))
          (set-fontset-font (frame-parameter nil 'font)
                            charset
                            (font-spec :family "Noto Sans Mono CJK SC")))) ;; 14 16 20 22 28
    ))

(defun +my|init-font(frame)
  (with-selected-frame frame
    (if (display-graphic-p)
        (+my/better-font))))

(if (and (fboundp 'daemonp) (daemonp))
    (add-hook 'after-make-frame-functions #'+my|init-font)
  (+my/better-font))


;; doom-modeline settings
(setq doom-modeline-modal nil)
(setq doom-modeline-buffer-encoding t)
;; (setq doom-modeline-major-mode-icon t)

;; open company tab on go mode
(add-hook 'after-init-hook 'company-tng-mode)

;; mousewheel settings
;; scroll one line at a time (less "jumpy" than defaults)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 3))) ;; one line at a time

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
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


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
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )


;; Company mode
;; (setq company-idle-delay 0)
;; (setq company-minimum-prefix-length 1)


;; lsp-mode settings
(setq lsp-headerline-breadcrumb-enable t)

(defun lsp-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; Go lsp-mode setting
;; Set up before-save hooks to format buffer and add/delete imports.
(add-hook 'go-mode-hook #'lsp-save-hooks)
(add-hook 'go-mode-hook #'lsp-deferred)


;; Rust lsp-mode setting
(setq lsp-rust-server 'rust-analyzer)
(setq rustic-lsp-server 'rust-analyzer)


;; lsp-mode keybinding
(evil-define-key 'normal lsp-mode-map
  (kbd "g h") 'lsp-ui-doc-toggle
  (kbd "<leader>rn") 'lsp-rename)


;; some keybindings
(evil-define-key 'insert 'global
  (kbd "C-v") 'yank)

(evil-define-key 'visual 'global
  (kbd "J") 'drag-stuff-down
  (kbd "K") 'drag-stuff-up)

;; :q should kill the current buffer rather than quitting emacs entirely
(evil-ex-define-cmd "q" 'kill-this-buffer)
;; Need to type out :quit to close emacs
(evil-ex-define-cmd "quit" 'evil-quit)



;; markdown font size settings
(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "variable-pitch"))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.1))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0)))))



;; settings to minimap
(setq minimap-window-location 'right)
(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle minimap-mode" "m" #'minimap-mode))


;; settings for treemacs
(setq treemacs-position 'right)
(setq treemacs-filewatch-mode t)
(setq treemacs-file-event-delay 1000)
(setq treemacs-follow-mode t)


;; settings for dirvish and dired
(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  ;; (setq dirvish-reuse-session nil) ; disable session reuse
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes
        '(file-time file-size collapse subtree-state vc-state git-msg))
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (setq dirvish-open-with-programs
        `((,dirvish-audio-exts . ("mpv" "%f"))
          (,dirvish-video-exts . ("mpv" "%f"))
          (("doc" "docx") . ("wps" "%f"))
          (("ppt" "pptx") . ("wpp" "%f"))
          (("xls" "xlsx") . ("et" "%f"))
          ))
  (setq dirvish-side-display-alist `((side . right) (slot . -1)))
  (setq dirvish-side-width 40)
  (setq dirvish-side-auto-close t)
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

(map! :leader
      (:prefix ("d" . "dirvish")
       :desc "Open dirvish" "v" #'dirvish
       :desc "Quit dirvish" "q" #'dirvish-quit
       :desc "Toggle dirvish-side" "s" #'dirvish-side
       :desc "Fd in dirvish" "f" #'dirvish-fd
       )
      )

(evil-define-key 'normal dired-mode-map
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-find-file ; use dired-find-file instead of dired-open.
  )
;; Get file icons in dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)


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
  (eshell-git-prompt-use-theme 'powerline)
  )
