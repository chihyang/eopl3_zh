#lang scribble/book
@(require "style.rkt"
          latex-utils/scribble/math
          latex-utils/scribble/utils
          scribble/manual
          scribble-math
          scribble/example
          scribble/core
          scribble/example
          racket/sandbox)

@title[#:style 'numbered #:tag "expr"]{表达式}

本章，我们研究变量绑定和作用域。我们用一系列小型语言解释这些概念。我们为这些语言
写出规范，遵照@secref{isd}的解释器秘方实现它们的解释器。我们的规范和解释器取一上
下文参数，叫做@emph{环境} (@emph{environment})，它记录待求值的表达式中每个变量的
含义。

@section[#:tag "spec-and-imp-strategy"]{规范和实现策略}

我们的规范包含若干判断，形如：

@nested{
@$${@tt{(value-of @${exp} @${\rho})} = val}

意为在环境@${\rho}中，表达式@${exp}的值应为@${val}。我们像第一章那样，写出推理规
则和方程，推导出这样的判断。我们手写规则和方程，以发现期望的一些表达式的值。

}

但我们的目的是写出程序，实现我们的语言，概况如图3.1(a)所示。首先是程序，由我们要
实现的语言写出。这叫做@emph{源语言} (@emph{source language})或 @emph{}
(@emph{defined language})。程序文本（由源语言写成的程序）传给前端，前端将其转化
为抽象语法树。之后，语法树传给解释器，解释器程序查看数据结构，根据结构执行一些动
作。解释器自身当然也由某种语言写成。我们把那种语言叫做@emph{实现语言}
(@emph{implementation language})或 @emph{} (@emph{defining language})。我们的大
多数实现都遵照这种方式。

另一种常见的组织方式如图3.1(b)所示。其中，解释器由编译器替代，后者将抽象语法树翻
译为另一种语言（叫@emph{目标语言} (@emph{target language})）写成的程序，然后执行。
目标语言可能由一个解释器执行，如图3.1(b)那样，也可能被翻译为更底层的语言再执行。

通常，目标语言是一种机器语言，由硬件解释。但目标语言也可能是一种特定用途的语言，
比原本的语言简单，为它写一个解释器相对容易。这样，程序可以编译一次，然后在多种不
同的硬件平台上执行。由于历史原因，这样的目标语言常称作@emph{字节码} (@emph{byte
code})，它的解释器称作@emph{虚拟机} (@emph{virtual machine})。

编译器常常分为两部分：@emph{分析器} (@emph{analyzer})，尝试推断关于程序的有用信
息；@emph{翻译器} (@emph{translator})，执行翻译，可能用到来自分析器的信息。这些
阶段既能用推理规则指定，也能用专做规范的语言指定。之后就是实现。第6章和第7章探讨
了一些简单的分析器和翻译器。

不论采用哪种实现策略，我们都需要一个@emph{前端} (@emph{front end})将程序转换为抽
象语法树。因为程序只是字符串，我们的前端需要将这些字符组织成有意义的单元。分组通
常分为两个阶段：@emph{扫描} (@emph{scanning})和@emph{解析} (@emph{parsing})。

扫描就是将字符序列分为单词，数字，标点，注释等等。这些单元叫做@emph{词条}
(@emph{lexical item})，@emph{词素} (@emph{lexeme})，或者最常见的，@emph{词牌}
(@emph{token})。程序分割为词牌的方式叫做语言的@emph{词法规范} (@emph{lexical
specification})。扫描器取一字符序列，生成词牌序列。

解析就是将词牌序列组织为有层次的语法结构，如表达式，语句和块。
