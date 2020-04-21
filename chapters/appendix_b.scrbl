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

@section[#:tag "B.1"]{扫描}

@nested[#:style eopl-figure]{
@(image "../images/task-of-scanner"
  #:suffixes (list ".pdf" ".svg")
  "扫描器的任务")

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "扫描器的任务"))]
}

@section[#:tag "B.2"]{解析}

@section[#:tag "B.3"]{SLLGEN中的扫描器和解析器}

@subsection[#:style 'unnumbered #:tag "B.3-scanners"]{定义扫描器}

@subsection[#:style 'unnumbered #:tag "B.3-grammars"]{定义语法}

@subsection[#:style 'unnumbered #:tag "B.3-operations"]{SLLGEN的操作}

@subsection[#:style 'unnumbered #:tag "B.3-arbno"]{@tt{arbno}和
@tt{separated-list}模板关键字}
