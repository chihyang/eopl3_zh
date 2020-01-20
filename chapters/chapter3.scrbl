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

任何编程语言规范中，最重要的一部分就是语言能处理的值的集合。每种语言至少有两个这
种集合：@emph{表达值} (@emph{expressed values})和@emph{指代值} (@emph{denoted
values})。表达值是指表达式可能的取值，指代值是指可以绑定到变量的值。

本章的语言中，表达值和指代值总是相同。现在，它们是：

@nested{
@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} \\
\mathit{DenVal} &= \mathit{Int} + \mathit{Bool} \\
}

第四章展示表达值和指代值不同的语言。

}

要使用这个定义，我们要有表达值数据类型的接口。我们有这几个接口：

@envalign*{
@bold{@tt{num-val}}  &: \mathit{Int} \to \mathit{ExpVal} \\
@bold{@tt{bool-val}} &: \mathit{Bool} \to \mathit{ExpVal} \\
@bold{@tt{expval->num}}   &: \mathit{ExpVal} \to \mathit{Int} \\
@bold{@tt{expval->bool}}  &: \mathit{ExpVal} \to \mathit{Bool} \\
}

我们假定传给@tt{expval->num}的参数不是整数，或传给@tt{expval->bool}的参数不是布
尔值时，二者未定义。

@subsection[#:tag "environment"]{环境}

若要求取表达式的值，我们得知道每个变量的值。我们靠环境记录这些值，就像在
@secref{rsdt}那样。

环境是一函数，定义域为变量的有限集合，值域为指代值。写环境时我们用一些缩写。

@itemlist[

 @item{@${\rho}@@elem[#:style question]{一环境}。}

 @item{@${\textnormal{\lbrack\rbrack}}表示空环境。}

 @item{@${[var = val]\rho}表示@tt{(extend-env @${var} @${val} @${\rho})}。}

 @item{@${[var_1 = val_1, var_2 = val2]\rho}是@${var_1 = val_1([var_2 =
 val_2]\rho)}的缩写，等等。}

 @item{@${[var_1 = val_1, var_2 = val2,\dots]}表示的环境中，@${var_1}的值为
 @${val_1}，等等。}

]

我们偶尔用不同缩进写出复杂环境，以便阅读。例如，我们可能把

@nested{

@racketblock[
(extend-env 'x 3
  (extend-env 'y 7
    (extend-env 'u 5 @#,elem{@${\rho}})))]

缩写为

@racketblock[
[x=3]
 [y=7]
  [u=5]@#,elem{@${\rho}}]

}

@subsection[#:tag "specifying-the-behavior-of-expressions"]{指定表达式的行为}

我们语言中的六种表达式各对应一个左边为@${Expression}的生成式。表达式接口包含七个
过程，六个是构造器，一个是观测器。我们用@${ExpVal}表示表达值的集合。

构造器：

@envalign*{
@bold{@tt{const-exp}}  &: \mathit{Int} \to \mathit{Exp} \\
@bold{@tt{zero?-exp}} &: \mathit{Exp} \to \mathit{Exp} \\
@bold{@tt{if-exp}}   &: \mathit{Exp} \times \mathit{Exp} \times \mathit{Exp} \to \mathit{Exp} \\
@bold{@tt{diff-exp}}  &: \mathit{Exp} \times \mathit{Exp} \to \mathit{Exp} \\
@bold{@tt{var-exp}}  &: \mathit{Var} \to \mathit{Exp} \\
@bold{@tt{let-exp}}  &: \mathit{Var} \times \mathit{Exp} \times \mathit{Exp} \to \mathit{Exp} \\
}

观测器：

@envalign*{
@bold{@tt{value-of}}  &: \mathit{Exp} \times \mathit{Env} \to \mathit{ExpVal} \\
}

实现之前，我们先写出这些过程的行为规范。依照解释器秘方，我们希望@tt{value-of}查
看表达式，判断其类别，然后返回恰当的值。

@racketblock[
(value-of (const-exp @#,elem{@${n}}) @#,elem{@${\rho}}) = (num-val @#,elem{@${n}})
]

@racketblock[
(value-of (var-exp @#,elem{@${var}}) @#,elem{@${\rho}}) = (apply-env @#,elem{@${\rho}} @#,elem{@${var}})
]

@racketblock[
(value-of (diff-exp @#,elem{@${exp_1}} @#,elem{@${exp_2}}) @#,elem{@${\rho}})
= (num-val
    (-
      (expval->num (value-of @#,elem{@${exp_1}} @#,elem{@${\rho}}))
      (expval->num (value-of @#,elem{@${exp_2}} @#,elem{@${\rho}}))))
]

任何环境中，常量表达式的值都是该常量。某一环境中，变量引用的值须在其中查询该变量
得到。某一环境中，差值表达式的值为第一个操作数在该环境中的值减去第二个在该环境中
的值。当然，准确来说我们得确保操作数的值是整数，并且结果是表示为表达值的整数值。

图3.3展示了如何结合这些规则求取一个构造器生成的表达式的值。在本例以及其他例子中，
我们用@${\textnormal{\guillemotleft} exp \textnormal{\guillemotright}}表示表达式
@${exp}的抽象语法树。我们还用@${\lceil n \rceil}代替@tt{(num-val @${n})}，用
@${\lfloor val \rfloor}代替@tt{(expval->num @${val})}。我们也运用了一点事实：
@${\lfloor \lceil n \rceil \rfloor = n}。

@exercise[#:level 1 #:tag "ex3.1"]{

列出在图3.3中，所有运用事实@${\lfloor \lceil n \rceil \rfloor = n}的地方。

}

@exercise[#:level 2 #:tag "ex3.2"]{

给出一个表达值@${val \in ExpVal}，且@${\lceil \lfloor n \rfloor \rceil \neq n}。

}

@subsection[#:tag "specifying-the-behavior-of-programs"]{指定程序的行为}

在我们的语言中，整个程序只是一个表达式。要找出这个表达式的值，我们需要指定程序中
自由变量的值。所以程序的值就是在适当的初始环境中求出的那个表达式的值。我们把初始
环境设为@tt{[i=1,v=5,x=10]}。

@racketblock[
(value-of-program @#,elem{@${exp}})
= (value-of @#,elem{@${exp}} [@#,elem{@tt{i=}@${\lceil \tt{1} \rceil},@tt{v=}@${\lceil \tt{5} \rceil},@tt{x=}@${\lceil \tt{10} \rceil}}])
]

@subsection[#:tag "specifying-conditions"]{指定条件}

下一部分介绍我们语言的布尔值接口。语言有一个布尔值构造器，@tt{zero?}，一个布尔值
观测器，@tt{if}表达式。

当且仅当操作数的值为0，@tt{zero?}表达式的值为真。可以将其写为一条推理规则，像定
义1.1.5那样。我们用@tt{bool-val}作为构造器，把布尔值转换为表达值，用
@tt{expval->num}作为抽词器，判断表达式是否为整数，如果是，则返回该整数。

@nested[#:style eopl-figure]{

令@${\rho =} @tt{[i=1,v=5,x=10]}。

@nested[#:style two-columns]{

@verbatim|{
(value-of
  <<-(-(x,3), -(v,i))>>
  |@${\rho})

= |@${\lceil}(-
    |@${\lfloor}(value-of <<-(x,3)>> |@${\rho})|@${\rfloor}
    |@${\lfloor}(value-of <<-(v,i)>> |@${\rho})|@${\rfloor})|@${\rceil}

= |@${\lceil}(-
    (-
      |@${\lfloor}(value-of <<x>> |@${\rho})|@${\rfloor}
      |@${\lfloor}(value-of <<3>> |@${\rho})|@${\rfloor})
    |@${\lfloor}(value-of <<-(v,i)>> |@${\rho})|@${\rfloor})|@${\rceil}

= |@${\lceil}(-
    (-
      10
      |@${\lfloor}(value-of <<3>> |@${\rho})|@${\rfloor})
    |@${\lfloor}(value-of <<-(v,i)>> |@${\rho})|@${\rfloor})|@${\rceil}

= |@${\lceil}(-
    (-
      10
      3)
    |@${\lfloor}(value-of <<-(v,i)>> |@${\rho})|@${\rfloor})|@${\rceil}

= |@${\lceil}(-
    7
    |@${\lfloor}(value-of <<-(v,i)>> |@${\rho})|@${\rfloor})|@${\rceil}
|@${\columnbreak}

= |@${\lceil}(-
    7
    (-
      |@${\lfloor}(value-of <<v>> |@${\rho})|@${\rfloor}
      |@${\lfloor}(value-of <<i>> |@${\rho})|@${\rfloor}))|@${\rceil}

= |@${\lceil}(-
    7
    (-
      5
      |@${\lfloor}(value-of <<i>> |@${\rho})|@${\rfloor}))|@${\rceil}

= |@${\lceil}(-
    7
    (-
      5
      1))|@${\rceil}

= |@${\lceil}(-
    7
    4)|@${\rceil}

= |@${\lceil}3|@${\rceil}
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "按照规范做简单运算"))]

}
