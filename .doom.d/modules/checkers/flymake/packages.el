;; -*- no-byte-compile: t; -*-
;;; checkers/flymake/packages.el

(when (modulep! +sideline)
  ;; NOTE: remove when straight bumped to support nonGnuELPA
  (package! sideline)
  (package! sideline-flymake)
  )
