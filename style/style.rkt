#lang racket
(require scribble/core scribble/latex-properties)

(define right
  (make-style "Sright" (list (make-tex-addition "style.tex"))))

(provide right)
