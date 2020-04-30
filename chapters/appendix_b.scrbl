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

程序只是字符串。要处理程序，需要将这些字符归类为有意义的单元。这种归类通常分为两
个阶段：@emph{扫描} (@emph{scanning})和@emph{解析} (@emph{parsing})。

扫描过程将字符序列分为单词，标点等等。这些单元叫做@emph{词条} (@emph{lexical
items})，@emph{词素} (@emph{lexeme})，或者最常见的@emph{词牌} (@emph{token})。解
析过程将词牌序列组织成有层次的语法结构，如表达式，语句和块。这就像用从句组织句子。

SLLGEN是一个Scheme包，用来生成解析器和扫描器。在本附录中，我们首先讨论扫描和解析
的基础，然后考虑如何用SLLGEN实现这些功能。

@section[#:tag "B.1"]{扫描}

扫描问题如图B.1所示。我们在其中展示了一小段程序，以及应如何将其分割为小单元。

字符串流应如何分割为词条是语言规范的一部分。语言的这部分规范有时称为@emph{词法规
范} (@emph{lexical specification})。典型的词法规范可能包括：

@itemlist[

 @item{任何空格和换行序列都等价于单个空格。}

 @item{注释以@tt{%}开头，持续到行尾。}

 @item{标识符是以字母开头的字母和数字序列。}

]

@nested[#:style eopl-figure]{
@(image "../images/task-of-scanner"
  #:suffixes (list ".pdf" ".svg")
  "扫描器的任务")

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "扫描器的任务"))]
}

扫描器的任务是遍历和分析输入，产生含有这些词条的数据结构。通常的语言中，扫描器可
能是一个过程，在调用时，由输入产生“下一个”词牌。

可以从头写出一个扫描器，但那又麻烦，又易错。更好的方式是写出指定语言的词法规范。
这一任务最常用的语言是@emph{正则表达式} (@emph{regular expressions})。正则表达式
语言定义如下：

@$${\mathit{R} ::= \mathit{Character} \mid \mathit{RR} \mid \mathit{R} \cup \mathit{R} \mid \neg\mathit{Character}}

每个正则表达式匹配一些字符串。我们可以用归纳法定义每个正则表达式匹配的字符串集合。

@section[#:tag "B.2"]{解析}

@section[#:tag "B.3"]{SLLGEN中的扫描器和解析器}

@subsection[#:style 'unnumbered #:tag "B.3-scanners"]{定义扫描器}

@subsection[#:style 'unnumbered #:tag "B.3-grammars"]{定义语法}

@subsection[#:style 'unnumbered #:tag "B.3-operations"]{SLLGEN的操作}

@subsection[#:style 'unnumbered #:tag "B.3-arbno"]{@tt{arbno}和
@tt{separated-list}模板关键字}
