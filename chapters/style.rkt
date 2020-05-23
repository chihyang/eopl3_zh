#lang racket
(require scribble/base
         scribble/core
         scribble/latex-properties
         scribble/html-properties
         scribble/decode
         scriblib/render-cond
         scribble-math)

;;; for title format
(define book-title-style '(toc no-index))
(define part-title-style-numbered '(numbered no-index))
(define part-title-style-unnumbered '(unnumbered no-index))
(define section-title-style-numbered '(no-index))
(define section-title-style-unumbered '(unnumbered no-index))

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

(define eopl-figure
  (make-style "EoplFigure" (list (make-tex-addition "../style/figure.tex"))))

(define eopl-subfigure
  (make-style "EoplSubfigure" (list (make-tex-addition "../style/subfigure.tex"))))

(define two-columns
  (make-style "TwoColumns" (list (make-tex-addition "../style/two-columns.tex"))))

(define samepage
  (make-style "Samepage" (list (make-tex-addition "../style/samepage.tex"))))

(define hangindent
  (make-style "Hangindent" (list (make-tex-addition "../style/hangindent.tex"))))

(define eopl-example
  (make-style "EoplExample" (list (make-tex-addition "../style/example.tex"))))

;;; for exercise
(define exercise-level-mark "{\\star}")

(define (make-level-mark l)
  (define (make-level-mark-iter n str)
    (if (zero? n)
        str
        (make-level-mark-iter (- n 1)
                              (string-append exercise-level-mark str))))
  (make-level-mark-iter l ""))

(define eopl-exercise
  (make-style "EoplExercise" (list (make-tex-addition "../style/exercise.tex"))))

(define (exercise #:level [level 1] #:tag [tag ""] . c)
  (nested #:style eopl-exercise
          ($ "\\textnormal{[}" (make-level-mark level) "\\textnormal{]}")
          (hspace 1)
          (remove-leading-newlines c)
          "\n"))

(define (remove-leading-newlines c)
  (cond [(null? c) c]
        [(and (string? (car c))
              (string=? (car c) "\n"))
         (remove-leading-newlines (cdr c))]
        [else c]))

(define frontmatter
  (make-paragraph (make-style 'pretitle '())
                  (make-element (make-style "frontmatter" '(exact-chars)) '())))

(define mainmatter
  (make-paragraph (make-style 'pretitle '())
                  (make-element (make-style "mainmatter" '(exact-chars)) '())))

(provide (except-out (all-defined-out)
                     remove-leading-newlines))
