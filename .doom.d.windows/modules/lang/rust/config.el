;;; lang/rust/config.el -*- lexical-binding: t; -*-

;; DEPRECATED: Remove when projectile is replaced with project.el
(after! projectile
  (add-to-list 'projectile-project-root-files "Cargo.toml"))

;;
;;; Packages

(use-package! rust-mode
  :mode ("\\.rs\\'" . rust-mode))
