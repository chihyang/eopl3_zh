#lang racket
(require scribble/core scribble/latex-properties)

(define right
  (make-style "Sright" (list (make-tex-addition "../style/right.tex"))))

(define underline
  (make-style "Sunderline" (list (make-tex-addition "../style/underline.tex"))))

(define question
  (make-style "Squestion" (list (make-tex-addition "../style/question.tex"))))

(define frontmatter
  (make-style "Sfrontmatter" (list (make-tex-addition "../style/frontmatter.tex"))))

(define mainmatter
  (make-style "Smainmatter" (list (make-tex-addition "../style/mainmatter.tex"))))

(define definition
  (make-style "definition" (list (make-tex-addition "../style/definition.tex"))))

(provide right underline question frontmatter mainmatter definition)
