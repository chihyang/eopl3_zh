#lang racket
(require scribble/base
         scribble/core
         scribble/latex-properties
         scribble/html-properties
         scribble/decode
         scriblib/render-cond
         scribble-math)

(define book-prefix-and-style
  (make-latex-defaults+replacements
   "../style/prefix.tex"
   "../style/style.tex"
   '()
   (hash "scribble-load-replace.tex"
         "../style/style-load-prefix.tex")))

(define racket-block-offset 6)

;;; for title format
(define book-title-style (make-style #f (list 'toc 'no-index book-prefix-and-style)))
(define part-title-style-numbered '(numbered no-index))
(define part-title-style-unnumbered '(unnumbered no-index))
(define section-title-style-numbered '(numbered no-index))
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

(define normalfont
  (make-style "NormalFont" (list (make-tex-addition "../style/normalfont.tex"))))

(define eopl-example
  (make-style "EoplExample" (list (make-tex-addition "../style/example.tex"))))

(define eopl-example-ref
  (make-style "EoplExampleRef" (list (make-tex-addition "../style/example.tex"))))

(define eopl-exercise
  (make-style "EoplExercise" (list (make-tex-addition "../style/exercise.tex"))))

(define eopl-exercise-ref
  (make-style "EoplExerciseRef" (list (make-tex-addition "../style/exercise.tex"))))

(define eopl-figure-ref
  (make-style "EoplFigureRef" (list (make-tex-addition "../style/figure.tex"))))

(define eopl-definition
  (make-style "EoplDefinition" (list (make-tex-addition "../style/definition.tex"))))

(define eopl-definition-title
  (make-style "EoplDefinitionTitle" (list (make-tex-addition "../style/definition-title.tex"))))

(define eopl-definition-ref
  (make-style "EoplDefinitionRef" (list (make-tex-addition "../style/definition.tex"))))

(define eopl-theorem
  (make-style "EoplTheorem" (list (make-tex-addition "../style/theorem.tex"))))

(define eopl-theorem-ref
  (make-style "EoplTheoremRef" (list (make-tex-addition "../style/theorem.tex"))))

(define small
  (make-style "Small" (list (make-tex-addition "../style/small.tex"))))

(define htt
  (make-style "Shtt" (list (make-tex-addition "../style/htt.tex"))))

;;; for exercise
(define exercise-level-mark "{\\star}")

(define (make-level-mark l)
  (define (make-level-mark-iter n str)
    (if (zero? n)
        str
        (make-level-mark-iter (- n 1)
                              (string-append exercise-level-mark str))))
  (make-level-mark-iter l ""))

(define (make-marker level)
  ($ "\\textnormal{[}" (make-level-mark level) "\\textnormal{]}"))

(define (exercise #:level [level 1] #:tag [tag ""] . c)
  (nested #:style eopl-exercise
          (elemtag tag "")
          (make-marker level)
          (hspace 1)
          (remove-leading-newlines c)))

(define (exercise-ref tag)
  (elem #:style eopl-exercise-ref (countref tag)))

(define (example #:tag [tag ""] . c)
  (nested #:style eopl-example
          (elemtag tag "") c))

(define (example-ref tag)
  (elem #:style eopl-example-ref (countref tag)))

(define (eopl-caption tag . c)
  (nested #:style
          (make-style "caption" (list 'multicommand))
          (remove-leading-newlines c)
          (when tag (elemtag tag ""))))

(define (figure-ref tag)
  (elem #:style eopl-figure-ref (countref tag)))

(define (remove-leading-newlines c)
  (cond [(null? c) c]
        [(and (string? (car c))
              (string=? (car c) "\n"))
         (remove-leading-newlines (cdr c))]
        [else c]))

(define (definition #:title [title #f] #:tag [tag ""] . c)
  (nested #:style eopl-definition
          (elemtag tag "")
          (if title
              (elem #:style eopl-definition-title
                    title)
              (hspace 1))
          (remove-leading-newlines c)))

(define (definition-ref tag)
  (elem #:style eopl-definition-ref (countref tag)))

(define (theorem #:title [title #f] #:tag [tag ""] . c)
  (nested #:style eopl-theorem
          (elemtag tag "")
          (if title
              (elem #:style eopl-definition-title
                    title)
              (hspace 1))
          (remove-leading-newlines c)))

(define (theorem-ref tag)
  (elem #:style eopl-theorem-ref (countref tag)))

(define (exact-elem . c)
  (make-element (make-style #f '(exact-chars)) c))

(define (big-bracket #:title [title #f] . c)
  (nested
   (exact-elem "\\begin{cornerbox}")
   (when title
     (exact-elem "[title=" title "]"))
   (exact-elem "\n")
   c
   (exact-elem "\n\\end{cornerbox}")))

(define frontmatter
  (make-paragraph (make-style 'pretitle '())
                  (make-element (make-style "frontmatter" '(exact-chars)) '())))

(define mainmatter
  (make-paragraph (make-style 'pretitle '())
                  (make-element (make-style "mainmatter" '(exact-chars)) '())))

(provide (except-out (all-defined-out)
                     remove-leading-newlines))
