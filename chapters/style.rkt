#lang racket
(require scribble/core scribble/latex-properties scribble/html-properties)

(define question
  (make-style "Squestion" (list (make-tex-addition "../style/question.tex")
                                (make-css-addition "../style/question.css"))))
(define right
  (make-style "Sright" (list (make-tex-addition "../style/right.tex")
                             (make-css-addition "../style/right.css"))))

(define underline
  (make-style "Sunderline" (list (make-tex-addition "../style/underline.tex")
                                 (make-css-addition "../style/underline.css"))))

(define tip
  (make-style "Tip" (list (make-tex-addition "../style/tip.tex")
                          (make-css-addition "../style/tip.css"))))

(define tip-content
  (make-style "TipContent" (list (make-tex-addition "../style/tip-content.tex")
                                 (make-css-addition "../style/tip-content.css"))))

(define exercise
  (make-style "MyExercise" (list (make-tex-addition "../style/exercise.tex"))))

(define figure
  (make-style "Figure" (list (make-tex-addition "../style/figure.tex"))))

(provide (all-defined-out))
