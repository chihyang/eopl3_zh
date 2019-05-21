#lang racket
(require scribble/core scribble/latex-properties)

(define right
  (make-style "Sright" (list (make-tex-addition "right.tex"))))

(define underline
  (make-style "Sunderline" (list (make-tex-addition "underline.tex"))))

(define question
  (make-style "Squestion" (list (make-tex-addition "question.tex"))))

(provide right underline question)
