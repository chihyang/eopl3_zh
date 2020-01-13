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

@title[#:style 'numbered #:tag "expr"]{表达式}

本章研究变量绑定和作用域。我们用一系列小型语言解释这些概念。我们为这些语言写出规
范，遵照@secref{isd}的解释器秘方实现其解释器。我们的规范和解释器取一名为@emph{环
境} (@emph{environment})的上下文参数，以记录待求值的表达式中各个变量的含义。

@section[#:tag "spec-and-imp-strategy"]{规范和实现策略}

我们的规范包含若干断言，形如：
@nested{
@$${@tt{(value-of @${exp} @${\rho})} = val}

意为在环境@${\rho}中，表达式@${exp}的值应为@${val}。我们像第1章那样，写出推理规
则和方程，推导出这样的断言。我们手写规则和方程，以发现期望的一些表达式的值。

}

而我们的目的是写出程序，实现我们的语言。概况如图3.1(a)所示。首先是程序，由我们要
实现的语言写出。这叫做@emph{源语言} (@emph{source language})或@emph{待定语言}
(@emph{defined language})。程序文本（由源语言写成的程序）传给前端，前端将其转化
为抽象语法树。之后，语法树传给解释器。解释器是一程序，它查看一段数据结构，根据结
构执行一些动作。解释器自身当然也由某种语言写成。我们把那种语言叫做@emph{实现语言}
(@emph{implementation language})或@emph{施定语言} (@emph{defining language})。我
们的大多数实现都遵照这种方式。

另一种常见的组织方式如图3.1(b)所示。其中，编译器替代了解释器，将抽象语法树翻译为
另一种语言（称为@emph{目标语言} (@emph{target language})）写成的程序，然后执行。
目标语言可能由一个解释器执行，如图3.1(b)那样，也可能为了执行而翻译成更底层的语言。

通常，目标语言是一种机器语言，由硬件解释。但目标语言也可能是一种特定用途的语言，
比原本的语言简单，为它写一个解释器相对容易。这样，程序可以编译一次，然后在多种不
同的硬件平台上执行。由于历史原因，这样的目标语言常称作@emph{字节码} (@emph{byte
code})，它的解释器称作@emph{虚拟机} (@emph{virtual machine})。

编译器常常分为两部分：@emph{分析器} (@emph{analyzer})，尝试推断关于程序的有用信
息；@emph{翻译器} (@emph{translator})，执行翻译，可能用到来自分析器的信息。这些
阶段既能用推理规则指定，也能用专做规范的语言指定。之后就是实现。第6章和第7章探讨
了一些简单的分析器和翻译器。

不论采用哪种实现策略，我们都需要一个@emph{前端} (@emph{front end})，将程序转换为
抽象语法树。因为程序只是字符串，我们的前端需要将这些字符组成有意义的单元。分组通
常分为两个阶段：@emph{扫描} (@emph{scanning})和@emph{解析} (@emph{parsing})。

扫描就是将字符序列分为单词，数字，标点，注释等等。这些单元称作@emph{词条}
(@emph{lexical item})，或者@emph{词素} (@emph{lexeme})，或者最常见的@emph{词牌}
(@emph{token})。把程序分为词牌的方式叫做语言的@emph{词法规范} (@emph{lexical
specification})。扫描器取一字符序列，生成词牌序列。

解析就是将词牌序列组成有层次的语法结构，如表达式，语句和块。这就像用从句组织（或
称图解@note{西方有diagram sentence之说，以树状图表示句子结构，如我国中学生学习英
文之主、谓、宾。——@emph{译注}}）句子。我们称之为语言的@emph{句法}
(@emph{syntactic})或@emph{语法} (@emph{grammatical})结构。解析器取一词牌序列（由
扫描器给出），生成一棵抽象语法树。

设计前端的标准方式是使用@emph{解析器制造机} (@emph{parser generator})。解析器制
造机是一程序，取一词法规范和语法，生成一扫描器和解析器。

@nested[#:style eopl-figure]{
@centered{
@(image "../images/exe-via-interpreter"
  #:suffixes (list ".eps" ".pdf" ".svg")
  "由解释器执行")
}

@centered{
@(image "../images/exe-via-compiler"
  #:suffixes (list ".eps" ".pdf" ".svg")
  "由解释器执行")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "语言处理系统块状图"))]
}

大多数主流语言都有解析器制造系统。如果没有解析器制造机，或者没有适用的，可以手写
扫描器和解析器。编译器教材描述了这一过程。我们使用的解析技术及相关语法设计从简，
专门满足我们的需求。

另一种方式是忽略具体语法的细节，把表达式写成列表结构，就像在@secref{asir}和练习
2.31中，处理lambda演算表达式那样。

@section[#:tag "let-a-simple-language"]{LET：一门简单语言}

我们先来定义一种非常简单的语言，根据它最有趣的特性命名为LET。

@subsection[#:tag "specifying-the-syntax"]{定义语法}

图3.2展示了我们这门简单语言的语法。在这种语言中，程序只能是一个表达式。一个表达
式是个整数常量，或差值表达式，或判零表达式，或条件表达式，或变量，或@tt{let}表达
式。

这里是这门语言的一个简单表达式，及其抽象语法表示。

@racketblock[
(scan&parse "-(55, -(x,11))")
#(struct:a-program
  #(struct:diff-exp
    #(struct:const-exp 55)
    #(struct:diff-exp
      #(struct:var-exp x)
      #(struct:const-exp 11))))]

@nested[#:style eopl-figure]{

@linebreak[]
@envalign*{\mathit{Program} &::= \mathit{Expression} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{a-program (exp1)}} \\[5pt]
        \mathit{Expression} &::= \mathit{Number} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{const-exp (num)}} \\[5pt]
        \mathit{Expression} &::= @tt{(- @m{\mathit{Expression}} , @m{\mathit{Expression}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{diff-exp (exp1 exp2)}} \\[5pt]
        \mathit{Expression} &::= @tt{(zero? @m{\mathit{Expression}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{zero?-exp (exp1)}} \\[5pt]
        \mathit{Expression} &::= @tt{if @m{\mathit{Expression}} then @m{\mathit{Expression}} else @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{if-exp (exp1 exp2 exp3)}} \\[5pt]
        \mathit{Expression} &::= \mathit{Identifier} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{var-exp (var)}} \\[5pt]
        \mathit{Expression} &::= @tt{let @m{\mathit{Identifier}} = @m{\mathit{Expression}} in @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{let-exp (var exp1 body)}}}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "LET语言的语法"))]
}

@subsection[#:tag "specification-of-values"]{定义值}

任何编程语言规范中，最重要的一部分就是语言处理的值集合。每种语言至少有两个这样的
集合：@emph{表达值} (@emph{expressed values})和@emph{指代值} (@emph{denoted
values})。表达值是指表达式的可能取值，指代值是指可以绑定到变量的值。

本章的语言中，表达值和指代值总是相同的。它们最初是：

@nested{
@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} \\
\mathit{DenVal} &= \mathit{Int} + \mathit{Bool} \\
}

第四章展示表达值和指代值不同的语言。
}

