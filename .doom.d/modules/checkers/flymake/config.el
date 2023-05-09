;;; checkers/flymake/config.el -*- lexical-binding: t; -*-

;;
;;; Flymake

(use-package! flymake
  :commands (flymake-mode)
  ;; as flymakes fail silently there is no need to activate it on a per major mode basis
  :hook ((prog-mode text-mode) . flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe)
  (setq flymake-no-changes-timeout 1.0)
  )

(use-package! sideline
  :when (modulep! +sideline)
  :hook (flymake-mode . sideline-mode)
  :init
  (setq sideline-backends-right '(sideline-flymake)
        sideline-display-backend-name t
        sideline-display-backend-type 'left
        )
  :config
  ;; rerender sideline after save
  (add-hook! 'after-save-hook #'sideline-render)
  )

(use-package! sideline-flymake
  :when (modulep! +sideline)
  :after sideline
  :init
  (setq sideline-flymake-display-mode 'line)
  )
