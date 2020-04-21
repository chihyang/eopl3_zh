#lang scribble/book
@(require "style.rkt"
          latex-utils/scribble/math
          latex-utils/scribble/utils
          scribble/manual
          scribble-math
          scribble/example
          scribble/core
          scribble/example
          scriblib/footnote
          racket/sandbox)

@title[#:tag "sllgen-parsing-system"]{SLLGEN解析系统}

@section[#:tag "B1.1"]{扫描}

@nested[#:style eopl-figure]{
@(image "../images/task-of-scanner"
  #:suffixes (list ".pdf" ".svg")
  "扫描器的任务")

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "扫描器的任务"))]
}
