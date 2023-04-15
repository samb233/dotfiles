;;; checkers/flymake/config.el -*- lexical-binding: t; -*-

;;
;;; Flymake

(use-package flymake
  :defer t
  :init
  ;; as flymakes fail silently there is no need to activate it on a per major mode basis
  (add-hook! (prog-mode text-mode) #'flymake-mode)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe))

(use-package flymake-popon
  :when (modulep! +popon)
  :hook (flymake-mode . flymake-popon-mode)
  :config
  (setq flymake-popon-method
        (if (modulep! +childframe)
            'postframe
          'popon)))


(use-package sideline
  :when (modulep! +sideline)
  :init
  (setq sideline-backends-right '(sideline-flymake))
  :hook ((flymake-mode . sideline-mode))
  )

(use-package sideline-flymake
  :when (modulep! +sideline)
  :init (setq sideline-flymake-display-mode 'line)
  (setq sideline-flymake-max-lines 5)
  )
