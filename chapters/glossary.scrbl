#lang scribble/book
@(require "style.rkt"
          latex-utils/scribble/utils
          scribble-math
          scribble-math/asymptote
          scriblib/footnote
          scribble-math/dollar)

@title[#:style part-title-style-unnumbered #:tag "glo"]{译名表}

@tabular[#:sep @hspace[1]
(list
@list["Call-by-reference" "按指调用"]
@list["Call-by-value" "按值调用"]
@list["Call-by-value-and-result" "按值和结果调用"]
@list["Contact" "合约"]
@list["Continuation" "续文"]
@list["Dereference" "解引用"]
@list["Derivation" "推导"]
@list["Explicit reference" "显式引用"]
@list["Form" "形式"]
@list["Formal parameter" "形参"]
@list["Implementation" "实现"]
@list["Implicit reference" "隐式引用"]
@list["List" "列表"]
@list["Pair" "序对"]
@list["Rule of inference" "推理规则"]
@list["Scope" "作用域"]
@list["Scoping" "定界"]
@list["Specification" "规范"]
@list["Specifying" "定义"]
@list["Store" "存储器"]
@list[@tt{car} "首项"]
@list[@tt{cdr} "余项"]
)]
