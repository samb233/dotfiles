;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
                                        ;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
                                        ;(unpin! pinned-package)
;; ...or multiple packages
                                        ;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
                                        ;(unpin! t)

;; 一些网上复制粘贴的包
(add-load-path! "elisp")

;; unpin
(unpin! vundo undo-fu-session)

;; 输入法管理
(package! sis)

;; Org-Mode
(package! org-appear)

;; 工具包
(package! fanyi)
(package! drag-stuff)
(package! ultra-scroll
  :recipe (:host github :repo "jdtsmith/ultra-scroll"
           :files ("*")))
(package! eglot-booster
  :recipe (:host github :repo "jdtsmith/eglot-booster"
           :files ("*")))
(package! symbol-overlay)
(package! colorful-mode)

;; Dockerfile-mode
(package! dockerfile-mode)

;; 管理工作区
(package! tabspaces)

;; 不需要这些功能，取消使用
(package! undo-fu :disable t)
(package! evil-escape :disable t)
(package! tide :disable t)
(package! pipenv :disable t)
(package! anaconda-mode :disable t)
(package! flymake-popon :disable t)

;; Windows 下加载 gpg 非常慢
(package! epa :disable t)

;; 过于老旧，取消使用
(package! gorepl-mode :disable t)
(package! ob-go :disable t)
(package! go-guru :disable t)
(package! go-eldoc :disable t)

;; 默认的go-tag在我的配置下会有一些问题
;; 使用我自己的go-tag
(unpin! go-tag)
(package! go-tag
  :recipe (:host github :repo "samb233/emacs-go-tag"
           :files ("*")))
