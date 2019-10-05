#lang racket
(require scribble/core scribble/latex-properties)

(define right
  (make-style "Sright" (list (make-tex-addition "../style/right.tex"))))

(define underline
  (make-style "Sunderline" (list (make-tex-addition "../style/underline.tex"))))

(define question
  (make-style "Squestion" (list (make-tex-addition "../style/question.tex"))))

(provide right underline question)
