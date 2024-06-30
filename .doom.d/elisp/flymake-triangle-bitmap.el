;;; elisp/flymake-triangle-bitmap.el -*- lexical-binding: t; -*-

(defvar my-fringe-left-triangle
  (vector #b00000000
          #b00000000
          #b00000000
          #b00000100
          #b00001100
          #b00011100
          #b00111100
          #b01111100
          #b11111100
          #b01111100
          #b00111100
          #b00011100
          #b00001100
          #b00000100
          #b00000000
          #b00000000
          #b00000000)
  "A left triangle fringe bitmap.")

(define-fringe-bitmap 'my-small-left-triangle
    my-fringe-left-triangle)

(provide 'flymake-triangle-bitmap)
;;; flymake-triangle-bitmap.el ends here
