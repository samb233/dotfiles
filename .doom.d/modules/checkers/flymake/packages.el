;; -*- no-byte-compile: t; -*-
;;; checkers/flymake/packages.el

(when (modulep! +popon)
  ;; NOTE: remove when straight bumped to support nonGnuELPA
  (package! popon :recipe (:repo "https://codeberg.org/akib/emacs-popon"))
  (package! flymake-popon :recipe (:repo "https://codeberg.org/akib/emacs-flymake-popon")))

(when (modulep! +sideline)
  ;; NOTE: remove when straight bumped to support nonGnuELPA
  (package! sideline)
  (package! sideline-flymake)
  )
