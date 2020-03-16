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

@title[#:style 'numbered #:tag "modules"]{模块}

对只有几百行代码的系统，我们介绍的语言特性已非常强大。如果我们要设计更大的系统，
有数千行代码，我们就还需要一些别的佐料。
