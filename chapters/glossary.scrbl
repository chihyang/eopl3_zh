#lang scribble/book
@(require "style.rkt"
          latex-utils/scribble/utils
          scribble-math
          scribble-math/asymptote
          scriblib/footnote
          scribble-math/dollar)

@title[#:style part-title-style-unnumbered #:tag "glo"]{译名表}

下表列出本书专有名词及其翻译，并对有疑义者略加说明。

@tabular[#:sep @hspace[1]
(list
@list["Bounce" "弹球，或不译"]
@list["Call by name" "按名调用"]
@list["Call by need" "按需调用"]
@list["Call by reference" "按指调用"]
@list["Call by value and result" "按值和结果调用"]
@list["Call by value" "按值调用"]
@list["Contact" "合约"]
@list["Continuation" "续文"]
@list["Control context" "控制上下文"]
@list["Dereference" "解引用"]
@list["Derivation" "推导"]
@list["Dynamic extent" "动态期限"]
@list["Effect" "效果、计算效果"]
@list["Explicit reference" "显式引用"]
@list["Flowchart program" "流程图程序"]
@list["Form" "形式"]
@list["Formal parameter" "形参"]
@list["Frozen" "冻结"]
@list["Implementation" "实现"]
@list["Implicit reference" "隐式引用"]
@list["List" "列表"]
@list["Memoization" "助记法"]
@list["Pair" "序对"]
@list["Procedure" "过程，即编程语言中的函数"]
@list["Register" "寄存器"]
@list["Registerization" "寄存"]
@list["Registerize" "寄存"]
@list["Rule of inference" "推理规则"]
@list["Scope" "作用域"]
@list["Scoping" "定界"]
@list["Specification" "规范"]
@list["Specifying" "定义"]
@list["Store" "存储器"]
@list["Thawed" "解冻"]
@list["Thunk" "值箱"]
@list["Trampoline" "跳床"]
@list["Trampolining" "跳跃"]
@list[@tt{car} "首项，或不译"]
@list[@tt{cdr} "余项，或不译"]
)]
