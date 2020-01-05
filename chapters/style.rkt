#lang racket
(require scribble/base
         scribble/core
         scribble/latex-properties
         scribble/html-properties
         scribble/decode
         scriblib/render-cond)

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

(define figure
  (make-style "Figure" (list (make-tex-addition "../style/figure.tex"))))

(define exer
  (make-style "MyExercise" (list (make-tex-addition "../style/exercise.tex")
                                 (make-css-addition "../style/exercise.css"))))

(define (remove-leading-newlines c)
  (cond [(null? c) c]
        [(and (string? (car c))
              (string=? (car c) "\n"))
         (remove-leading-newlines (cdr c))]
        [else c]))

(define-syntax (add-style-property-inner stx)
  (syntax-case stx ()
    [(_ type content)
     (syntax-case
         (datum->syntax #'type
                        (string->symbol
                         (format "~a-style" (syntax->datum #'type))))
         ()
       [type #'(let ((sty (type content)))
                 (make-style (style-name sty)
                             (cons 'never-indents (style-properties sty))))])]))

(define (add-style-property block)
  (cond
   [(paragraph? block)
    (make-paragraph
     (add-style-property-inner paragraph block)
     (paragraph-content block))]
   [(table? block)
    (make-table
     (add-style-property-inner table block)
     (table-blockss block))]
   [(itemization? block)
    (make-itemization
     (add-style-property-inner itemization block)
     (itemization-blockss block))]
   [(nested-flow? block)
    (make-nested-flow
     (add-style-property-inner nested-flow block)
     (nested-flow-blocks block))]
   [else block]))

(define (make-exercise-prefix level tag)
  (make-element (make-style #f (list 'exact-chars))
                (string-append
                 "\\begin{Exercise}"
                 "[difficulty="
                 (number->string level)
                 ", "
                 (when (> (string-length tag))
                   (string-append "label=" tag ", "))
                 "counter=ChapterCounter]{\n")))

(define exercise-postfix
  (make-element (make-style #f (list 'exact-chars))
                "}\\end{Exercise}"))

(define (add-prefix-to-block block prefix)
  (cond [(paragraph? block)
         (if (list? (paragraph-content block))
             (make-paragraph (paragraph-style block)
                             (cons prefix (paragraph-content block)))
             (make-paragraph (paragraph-style block)
                             (list prefix (paragraph-content block))))]
        [(compound-paragraph? block)
         (make-compound-paragraph
          (compound-paragraph-style block)
          (add-prefix (compound-paragraph-blocks block) prefix))]
        [else (list (make-paragraph (make-style #f '()) prefix) block)]))

(define (add-prefix a-flow exer-prefix)
  (if (null? a-flow)
      (make-paragraph (make-style #f '() exer-prefix))
      (let ((prefixed-block
             (add-prefix-to-block (car a-flow) exer-prefix)))
        (if (list? prefixed-block)
            (append prefixed-block (cdr a-flow))
            (cons prefixed-block (cdr a-flow))))))

(define (exercise #:level [level 1] #:tag [tag ""] . c)
  (cond-block
   [latex
    (let* ((exer-prefix (make-exercise-prefix level tag))
           (decoded-flow (add-prefix (decode-flow (remove-leading-newlines c)) exer-prefix))
           (wrapped-flow
            (make-compound-paragraph
             (make-style #f '())
             (append
              decoded-flow
              (list
               (make-paragraph (make-style #f '(never-indents))
                               exercise-postfix))))))
      wrapped-flow)]
   [else (decode-flow c)]))

(provide (except-out (all-defined-out)
                     add-prefix-to-block
                     remove-leading-newlines
                     add-style-property-inner
                     add-style-property
                     make-exercise-prefix
                     exercise-postfix
                     add-style-property))
