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

本章研究变量绑定和及其作用范围。我们用一系列小型语言解释这些概念。我们为这些语言
写出规范，遵照@secref{isd}的解释器秘方实现其解释器。我们的规范和解释器取一名为
@emph{环境} (@emph{environment})的上下文参数，以记录待求值的表达式中各个变量的含
义。

@section[#:tag "s3.1"]{规范和实现策略}

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

 @nested[#:style eopl-subfigure]{
 @centered{
 @(image "../images/exe-via-interpreter"
   #:suffixes (list ".pdf" ".svg")
   "由解释器执行")
 }

 @make-nested-flow[
  (make-style "caption" (list 'multicommand))
  (list (para "由解释器执行"))]
 }

 @nested[#:style eopl-subfigure]{
 @centered{
 @(image "../images/exe-via-compiler"
   #:suffixes (list ".pdf" ".svg")
   "由编译器执行")
 }

 @make-nested-flow[
  (make-style "caption" (list 'multicommand))
  (list (para "由编译器执行"))]
 }

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "语言处理系统块状图"))]
}

大多数主流语言都有解析器制造系统。如果没有解析器制造机，或者没有合适的，可以手写
扫描器和解析器。编译器教材描述了这一过程。我们使用的解析技术及相关语法设计从简，
专门满足我们的需求。

另一种方式是忽略具体语法的细节，把表达式写成列表结构，就像在@secref{asir}和练习
2.31中，处理lambda演算表达式那样。

@section[#:tag "s3.2"]{LET：一门简单语言}

我们先来定义一种非常简单的语言，根据它最有趣的特性命名为LET。

@subsection[#:tag "s3.2.1"]{定义语法}

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

@subsection[#:tag "s3.2.2"]{定义值}

任何编程语言规范中，最重要的一部分就是语言能处理的值的集合。每种语言至少有两个这
种集合：@emph{表达值} (@emph{expressed values})和@emph{指代值} (@emph{denoted
values})。表达值是指表达式可能的取值，指代值是指可以绑定到变量的值。

本章的语言中，表达值和指代值总是相同。现在，它们是：

@nested{
@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} \\
\mathit{DenVal} &= \mathit{Int} + \mathit{Bool}
}

第四章展示表达值和指代值不同的语言。

}

要使用这个定义，我们要有表达值数据类型的接口。我们有这几个接口：

@envalign*{
@bold{@tt{num-val}}  &: \mathit{Int} \to \mathit{ExpVal} \\
@bold{@tt{bool-val}} &: \mathit{Bool} \to \mathit{ExpVal} \\
@bold{@tt{expval->num}}   &: \mathit{ExpVal} \to \mathit{Int} \\
@bold{@tt{expval->bool}}  &: \mathit{ExpVal} \to \mathit{Bool}
}

我们假定传给@tt{expval->num}的参数不是整数，或传给@tt{expval->bool}的参数不是布
尔值时，二者未定义。

@subsection[#:tag "s3.2.3"]{环境}

若要求取表达式的值，我们得知道每个变量的值。我们靠环境记录这些值，就像在
@secref{rsdt}那样。

环境是一函数，定义域为变量的有限集合，值域为指代值。写环境时我们用一些缩写。

@itemlist[

 @item{@${\rho}表示任一环境。@note{原文为“@${\rho} ranges over environments”。
 这是将所有环境视为一集合，@${\rho}视为一变量，@${\rho}可以取集合中的任意值。
 @tech[#:key "sigma_for_store"]{下文}中的@${\sigma}同理。}}

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

@subsection[#:tag "s3.2.4"]{指定表达式的行为}

我们语言中的六种表达式各对应一个左边为@${Expression}的生成式。表达式接口包含七个
过程，六个是构造器，一个是观测器。我们用@${ExpVal}表示表达值的集合。

构造器：

@envalign*{
@bold{@tt{const-exp}}  &: \mathit{Int} \to \mathit{Exp} \\
@bold{@tt{zero?-exp}} &: \mathit{Exp} \to \mathit{Exp} \\
@bold{@tt{if-exp}}   &: \mathit{Exp} \times \mathit{Exp} \times \mathit{Exp} \to \mathit{Exp} \\
@bold{@tt{diff-exp}}  &: \mathit{Exp} \times \mathit{Exp} \to \mathit{Exp} \\
@bold{@tt{var-exp}}  &: \mathit{Var} \to \mathit{Exp} \\
@bold{@tt{let-exp}}  &: \mathit{Var} \times \mathit{Exp} \times \mathit{Exp} \to \mathit{Exp}
}

观测器：

@envalign*{
@bold{@tt{value-of}}  &: \mathit{Exp} \times \mathit{Env} \to \mathit{ExpVal}
}

实现之前，我们先写出这些过程的行为规范。依照解释器秘方，我们希望@tt{value-of}查
看表达式，判断其类别，然后返回恰当的值。

@nested[#:style 'code-inset]{
@verbatim|{
(value-of (const-exp |@${n}) |@${\rho}) = (num-val |@${n})

(value-of (var-exp |@${var}) |@${\rho}) = (apply-env |@${\rho} |@${var})

(value-of (diff-exp |@${exp_1} |@${exp_2}) |@${\rho})
= (num-val
    (-
      (expval->num (value-of |@${exp_1} |@${\rho}))
      (expval->num (value-of |@${exp_2} |@${\rho}))))
}|
}

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

@subsection[#:tag "s3.2.5"]{指定程序的行为}

在我们的语言中，整个程序只是一个表达式。要找出这个表达式的值，我们需要指定程序中
自由变量的值。所以程序的值就是在适当的初始环境中求出的那个表达式的值。我们把初始
环境设为@tt{[i=1,v=5,x=10]}。

@racketblock[
(value-of-program @#,elem{@${exp}})
= (value-of @#,elem{@${exp}} [@#,elem{@tt{i=}@${\lceil \tt{1} \rceil},@tt{v=}@${\lceil \tt{5} \rceil},@tt{x=}@${\lceil \tt{10} \rceil}}])
]

@subsection[#:tag "s3.2.6"]{指定条件}

下一部分介绍我们语言的布尔值接口。语言有一个布尔值构造器，@tt{zero?}，一个布尔值
观测器，@tt{if}表达式。

当且仅当操作数的值为0，@tt{zero?}表达式的值为真。可以将其写为一条推理规则，像定
义1.1.5那样。我们用@tt{bool-val}作为构造器，把布尔值转换为表达值，用
@tt{expval->num}作为抽词器，判断表达式是否为整数，如果是，则返回该整数。

@nested[#:style eopl-figure]{
令@${\rho =} @tt{[i=1,v=5,x=10]}。@linebreak[]

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

@$${
\infer{\begin{alignedat}{-1}
         &@tt{ (value-of (zero?-exp @${exp_1}) @${\rho})} \\
         &\hphantom{xx}= \begin{cases}
                           @tt{(bool-val #t)} & 若 @tt{(expval->num @${val_1})} = 0 \\
                           @tt{(bool-val #f)} & 若 @tt{(expval->num @${val_1})} \neq 0 \hphantom{x}
                         \end{cases}
       \end{alignedat}}
      {@tt{(value-of @${exp_1} @${\rho}) = @${val_1}}}
}

一个@tt{if}表达式就是一个布尔值观测器。要求取@tt{if}表达式@tt{(if-exp @${exp_1}
@${exp_2} @${exp_3})}的值，首先要判断子表达式@${exp_1}的值。如果该值为真，整个表
达式的值应该是子表达式@${exp_2}的值，否则是子表达式@${exp_3}的值。这也很容易写出
推理规则。我们用@tt{expval->bool}提取表达值的布尔部分，就像在前一个例子中使用
@tt{expval->num}一样。

@$${
\infer{\begin{alignedat}{-1}
         &@tt{ (value-of (if-exp @${exp_1} @${exp_2} @${exp_3}) @${\rho}) } \\
         &\hphantom{xx}= \begin{cases}
                          @tt{(value-of @${exp_2} @${\rho})} & 若 @tt{(expval->bool @${val_1})} = @tt{#t} \\
                          @tt{(value-of @${exp_3} @${\rho})} & 若 @tt{(expval->bool @${val_1})} = @tt{#f} \hphantom{x}
                        \end{cases}
       \end{alignedat}}
      {@tt{(value-of @${exp_1} @${\rho}) = @${val_1}}}
}

这种推理规则可用来很容易地指定任何单个表达式的期望行为，但却不很适合展示推理过程。
像@tt{(value-of @${exp_1} @${\rho})}这样的前件表示一部分计算，所以一个计算过程应
该是一棵树，就像@elem[#:style question]{第5页}那种。很不幸的是，这种树很难读懂。
因此，我们经常把规则转为方程，然后就能用相等待换展示计算过程。

@tt{if-exp}的方程式规范是：

@nested[#:style 'code-inset]{
@verbatim|{
(value-of (if-exp |@${exp_1} |@${exp_2} |@${exp_3}) |@${\rho})
= (if (expval->bool (value-of |@${exp_1} |@${\rho}))
    (value-of |@${exp_2} |@${\rho})
    (value-of |@${exp_3} |@${\rho}))
}|
}

图3.4展示了用这些规则进行简单运算的过程。

@nested[#:style eopl-figure]{
令@${\rho =} @tt{[x=@${\lceil}33@${\rceil},y=@${\lceil}22@${\rceil}]}。@linebreak[]

@verbatim|{
(value-of
  <<if zero?(-(x,11)) then -(y,2) else -(y,4)>>
  |@${\rho})

= (if (expval->bool (value-of <<zero?(-(x,11))>> |@${\rho}))
    (value-of <<-(y,2)>> |@${\rho})
    (value-of <<-(y,4)>> |@${\rho}))

= (if (expval->bool (bool-val #f))
    (value-of <<-(y,2)>> |@${\rho})
    (value-of <<-(y,4)>> |@${\rho}))

= (if #f
    (value-of <<-(y,2)>> |@${\rho})
    (value-of <<-(y,4)>> |@${\rho}))

= (value-of <<-(y,4)>> |@${\rho})

= |@${\lceil}18|@${\rceil}
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "条件表达式的简单计算过程"))]
 }

@subsection[#:tag "s3.2.7"]{指定@tt{let}}

接下来我们解决用@tt{let}表达式创建新变量绑定的问题。我们给这种解释性语言添加语法，
用关键字@tt{let}引导一个声明，关键字@tt{in}，以及主体。例如，


@nested{

@nested[#:style 'code-inset]{
@verbatim|{
let x = 5
in -(x, 3)
}|
}

@tt{let}变量绑定在主体中，就像@tt{lambda}变量绑定一样（见@secref{o-f}）。
}

整个@tt{let}式是一个表达式，就像其主体一样，所以@tt{let}表达式可以嵌套，例如

@nested{

@nested[#:style 'code-inset]{
@verbatim|{
let z = 5
in let x = 3
   in let y = -(x, 1)    % 这里 x = 3
      in let x = 4
         in -(z, -(x,y)) % 这里 x = 4
}|
}

在本例中，第一个差值表达式中使用的@tt{x}指代外层声明，另一个差值表达式中使用的
@tt{x}指代内层声明，所以整个表达式的值是3。
}

@tt{let}声明的右边也是一个表达式，所以它可以任意复杂。例如

@nested{

@nested[#:style 'code-inset]{
@verbatim|{
let x = 7
in let y = 2
   in let y = let x = -(x,1)
              in -(x,y)
      in -(-(x,8), y)
}|
}

这里第三行声明的@tt{x}绑定到6，所以@tt{y}的值是4，整个表达式的值是@${((-1)-4) =
-5}。
}

可以将规范写成一条规则。

@$${
\infer{\begin{alignedat}{-1}
        &@tt{ (value-of (let-exp @${var} @${exp_1} @${body}) @${\rho}) } \\
        &\hphantom{xx}= @tt{(value-of @${body} [@${var}=@${val_1}]@${\rho}) }
       \end{alignedat}}
      {@tt{(value-of @${exp_1} @${\rho}) = @${val_1}}}
}

像之前那样，将其转为方程通常更方便。

@nested{

@nested[#:style 'code-inset]{
@verbatim|{
(value-of (let-exp |@${var} |@${exp_1} |@${body}) |@${\rho})
= (value-of |@${body} [var=(value-of |@${exp_1} |@${\rho})]|@${\rho})
}|
}

图3.5展示了一个例子，其中@${\rho_0}表示任意环境。
}

@nested[#:style eopl-figure]{
@verbatim|{

(value-of
  <<let x = 7
    in let y = 2
       in let y = let x = -(x,1) in -(x,y)
          in -(-(x,8),y)>>
  |@${\rho_0})

= (value-of
    <<let y = 2
      in let y = let x = -(x,1) in -(x,y)
         in -(-(x,8),y)>>
    [x=|@${\lceil}7|@${\rceil}]|@${\rho_0})

= (value-of
    <<let y = let x = -(x,1) in -(x,y)
      in -(-(x,8),y)>>
    [y=|@${\lceil}2|@${\rceil}][x=|@${\lceil}7|@${\rceil}]|@${\rho_0})

令 |@${\rho_1} = [y=|@${\lceil}2|@${\rceil}][x=|@${\lceil}7|@${\rceil}]|@${\rho_0}。

= (value-of
    <<-(-(x,8),y)>>
    [y=(value-of <<let x = -(x,1) in -(x,y)>> |@${\rho_1})]
    |@${\rho_1})

= (value-of
    <<-(-(x,8),y)>>
    [y=(value-of <<-(x,2)>> [x=(value-of <<-(x,1)>> |@${\rho_1})]|@${\rho_1})]
    |@${\rho_1})

= (value-of
    <<-(-(x,8),y)>>
    [y=(value-of <<-(x,2)>> [x=|@${\lceil}6|@${\rceil}]|@${\rho_1})]
    |@${\rho_1})

= (value-of
    <<-(-(x,8),y)>>
    [y=|@${\lceil}4|@${\rceil}]|@${\rho_1})

= |@${\lceil}(- (- 7 8) 4)|@${\rceil}

= |@${\lceil}-5|@${\rceil}
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para (tt "let") "一例"))]
 }

@subsection[#:tag "s3.2.8"]{实现LET的规范}

接下来的任务是用一组Scheme过程实现这一规范。我们的实现采用SLLGEN@note{见
@elem[#:style question]{附录B}。}作为前端，表达式用图3.6中的数据类型表示。我们的
实现中，表达值的表示如图3.7所示。数据类型声明了构造器@tt{num-val}和@tt{bool-val}，
用来将整数和布尔值转换为表达值。我们还定义了抽词器，用来将表达值转为整数或布尔值。
如果表达值类型不符预期，则抽词器报错。

@nested[#:style eopl-figure]{
@racketblock[
(define-datatype program program?
  (a-program
   (exp1 expression?)))

(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "LET语言的语法数据类型"))]
}

只要满足@secref{rsdt}中的定义，我们可以用任意一种环境的实现。过程@tt{init-env}创
建指定的初始环境，由@tt{value-of-program}使用。

@nested[#:style samepage]{
@racketblock[
@#,elem{@bold{@tt{init-env}} : @${() \to \mathit{Env}}}
@#,elem{@bold{用法} : @tt{(init-env)} = @tt{[i=@${\lceil}1@${\rceil},v=@${\lceil}5@${\rceil},x=@${\lceil}10@${\rceil}]}}
(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))
]
}

@nested[#:style eopl-figure]{
@racketblock[
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?)))

@#,elem{@bold{@tt{expval->num}} : @${\mathit{ExpVal} \to \mathit{Int}}}
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

@#,elem{@bold{@tt{expval->bool}} : @${\mathit{ExpVal} \to \mathit{Bool}}}
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "LET语言的表达值"))]
}

现在我们可以写出解析器，如图3.8和3.9所示。主过程是@tt{run}，它取一个字符串，解析
它，把结果交给@tt{value-of-program}。最有意思的过程是@tt{value-of}，它取一表达式
和一环境，用解释器秘方计算规范要求的答案。在代码中，我们插入了相关推理规则定义，
以便观察@tt{value-of}的代码如何与规范对应。

@nested[#:style 'noindent]{

@smaller{@linebreak[]在下面的练习以及全书之中，短句“扩展语言，添加……”表示向
语言规范添加规则或者方程，并增改相应的解释器，实现指定特性。}

}

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{run}} : @${\mathit{String} \to \mathit{ExpVal}}}
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{ExpVal}}}
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))

@#,elem{@bold{@tt{value-of}} : @${\mathit{ExpVal} \times \mathit{Env} \to \mathit{Bool}}}
(define value-of
  (lambda (exp env)
    (cases expression exp

      @#,elem{@${\fbox{@tt{ (value-of (const-exp @${n}) @${\rho}) = n}}}}
      (const-exp (num) (num-val num))

      @#,elem{@${\fbox{@tt{ (value-of (var-exp @${var}) @${\rho}) = @tt{(apply-env @${\rho} @${var})}}}}}
      (var-exp (var) (apply-env env var))
      @; very long and ugly equation, to avoid surplus vspace after the equation
      @#,elem{@${\fbox{\begin{math}\begin{alignedat}{-1}&@tt{ (value-of (diff-exp @${exp_1} @${exp_2}) @${\rho}) =} \\ &\hphantom{xxx}@tt{@${\lceil}(- @${\lfloor}(value-of @${exp_1} @${\rho})@${\rfloor} @${\lfloor}(value-of @${exp_2} @${\rho})@${\rfloor})@${\rceil}}\end{alignedat}\end{math}}}}
      (diff-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((num1 (expval->num val1))
                (num2 (expval->num val2)))
            (num-val
              (- num1 num2)))))
...)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "LET语言的解释器"))]
}

@nested[#:style eopl-figure]{
@racketblock[
(((...
      @#,elem{@${\fbox{\infer{\begin{alignedat}{-1}&@tt{(value-of (zero?-exp @${exp_1}) @${\rho})} \\ &\hphantom{x}= \begin{cases} @tt{(bool-val #t)} & 若 @tt{(expval->num @${val_1})} = 0 \\ @tt{(bool-val #f)} & 若 @tt{(expval->num @${val_1})} \neq 0 \end{cases} \end{alignedat}}{@tt{(value-of @${exp_1} @${\rho}) = @${val_1}}}}}}
      (zero?-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (let (num1 (expval->num val1))
            (if (zero? num1)
              (bool-val #t)
              (bool-val #f)))))

      @#,elem{@${\fbox{\infer{\begin{alignedat}{-1}&@tt{(value-of (if-exp @${exp_1} @${exp_2} @${exp_3}) @${\rho})} \\ &\hphantom{x}= \begin{cases} @tt{(value-of @${exp_2} @${\rho})} & 若 @tt{(expval->bool @${val_1})} = @tt{#t} \\ @tt{(value-of @${exp_3} @${\rho})} & 若 @tt{(expval->bool @${val_1})} = @tt{#f} \end{cases} \end{alignedat}}{@tt{(value-of @${exp_1} @${\rho}) = @${val_1}}}}}}
      (if-exp (exp1 exp2 exp3)
        (if (expval->bool (value-of exp1 env))
          (value-of exp2 env)
          (value-of exp3 env)))

      @#,elem{@${\fbox{\infer{\begin{alignedat}{-1}&@tt{(value-of (let-exp @${var} @${exp_1} @${body}) @${\rho})} \\ &\hphantom{x}= @tt{(value-of @${body} [@${var}=@${val_1}]@${\rho})} \end{alignedat}}{@tt{(value-of @${exp_1} @${\rho}) = @${val_1}}}}}}
      (let-exp (var exp1 body)
        (let ((val1 (value-of exp1 env)))
          (value-of body
            (extend-env var val1 env)))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "LET语言的解释器，续"))]
}

@exercise[#:level 1 #:tag "ex3.3"]{

我们只能选一个算数操作，减法为什么比加法好？

}

@exercise[#:level 1 #:tag "ex3.4"]{

把图3.4中的推导写成@elem[#:style question]{第5页}那样的推理树。

}

@exercise[#:level 1 #:tag "ex3.5"]{

把图3.5中的推导写成@elem[#:style question]{第5页}那样的推理树。

}

@exercise[#:level 1 #:tag "ex3.6"]{

扩展语言，添加新操作符@tt{minus}，它取一参数@${n}，返回@${-n}。例如，
@tt{-(minus(5),9)}的值应为14。

}

@exercise[#:level 1 #:tag "ex3.7"]{

扩展语言，添加加法、乘法和整数除法操作。

}

@exercise[#:level 1 #:tag "ex3.8"]{

向该语言的操作符谓词@tt{equal?}，@tt{greater?}和@tt{less?}，做数值等于、大于和小
于比较。

}

@exercise[#:level 2 #:tag "ex3.9"]{

向该语言添加列表处理操作，包括@tt{cons}，@tt{car}，@tt{cdr}，@tt{null?}和
@tt{emptylist}。列表可以包含任何表达值，包括其他列表。像@secref{s3.2.2}那样，给
出语言表达值和指代值的定义。例如：

@nested{

@nested[#:style 'code-inset]{
@verbatim|{
let x = 4
in cons(x,
        cons(cons(-(x,1),
                  emptylist),
             emptylist))
}|
}

应返回一表达值，表示列表@tt{(4 (3))}。
}

}

@exercise[#:level 2 #:tag "ex3.10"]{

向该语言添加操作@tt{list}。该操作取任意数量的参数，返回一表达值，包含由参数值组
成的列表。例如：

@nested[#:style 'code-inset]{
@verbatim|{
let x = 4
in list(x, -(x,1), -(x,3))
}|

}

应返回一表达值，表示列表@tt{(4 3 1)}。

}

@exercise[#:level 1 #:tag "ex3.11"]{

真正的语言可能有很多上述练习那样的操作符。调整解释器代码，以便添加新操作符。

}

@exercise[#:level 1 #:tag "ex3.12"]{

向该语言添加组件，用来增加@tt{cond}表达式。语法为：@linebreak[]

@$${\mathit{Expression} ::= @tt{cond @${\{}@${\mathit{Expression}} @tt{ ==> } @${\mathit{Expression}}@${\}^{*}} end}}

在这种表达式里，@tt{==>}左边的表达式按序求值，直到其中一个返回真。整个表达式的值
则是对应的右边表达式的值。如果没有条件为真，表达式应报错。

}

@exercise[#:level 1 #:tag "ex3.13"]{

改变语言，把整数作为唯一的表达值。修改@tt{if}，把0作为假，所有其他值作为真。相应
地修改谓词。

}

@exercise[#:level 2 #:tag "ex3.14"]{

前一题的另一做法是给语言添加新的非终止符@${Bool\mbox{-}exp}，作为布尔值表达式。修改条件
表达式的生成式：@linebreak[]

@$${\mathit{Expression} ::= @tt{if @${\mathit{Bool\mbox{-}exp}} then @${\mathit{Expression}} else @${\mathit{Expression}}}}

为@${Bool\mbox{-}exp}写出适当的生成式，实现@tt{value-of-bool-exp}。按这种方式，
练习3.8中的谓词应放在哪里？

}

@exercise[#:level 1 #:tag "ex3.15"]{

扩展语言，添加新操作@tt{print}，它取一参数，打印出来，返回整数1。照我们这样定义
规范，为什么不能表示这一操作？

}

@exercise[#:level 2 #:tag "ex3.16"]{

扩展语言，允许@tt{let}声明任意数量的变量，语法为：@linebreak[]

@$${\mathit{Expression} ::= @tt{let @${\{}@${\mathit{Identifier}} = @${\mathit{Expression}\}^*} in @${\mathit{Expression}}}}

像Scheme中的@tt{let}那样，声明右边在当前环境中求值，每个新变量绑定到对应的声明右
边的值，然后求值主体。例如：

@nested[#:style 'code-inset]{
@verbatim|{
let x = 30
in let x = -(x,1)
       y = -(x,2)
   in -(x,y)
}|
}

值应为1。
}

@exercise[#:level 2 #:tag "ex3.17"]{

扩展语言，添加表达式@tt{let*}，像Scheme的@tt{let*}那样。则：

@nested[#:style 'code-inset]{
@verbatim|{
let x = 30
in let* x = -(x,1) y = -(x,2)
   in -(x,y)
}|
}

值应为2。
}

@exercise[#:level 2 #:tag "ex3.18"]{

向该语言添加表达式：@linebreak[]

@$${\mathit{Expression} ::= @tt{unpack @${\{\mathit{Identifier}\}^*} = @${\mathit{Expression}} in @${\mathit{Expression}}}}

则：如果@tt{lst}恰好是有三个元素的列表，@tt{unpack x y z = lst in ...}将@tt{x}，
@tt{y}，@tt{z}绑定到@tt{lst}的各元素；否则报错。例如：

@nested[#:style 'code-inset]{
@verbatim|{
let u = 7
in unpack x y = cons(u,cons(3,emptylist))
   in -(x,y)
}|
}

值应为4。
}

@section[#:tag "s3.3"]{PROC：有过程的语言}

到现在为止，我们的语言只能做语言已定义的操作。要想让我们这种解释性语言更有用，必
须能创建新过程。我们把新语言叫做PROC。

我们将按照Scheme的设计，把过程作为语言的表达值，则：

@nested{
@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} \\
\mathit{DenVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc}
}

其中，@${Proc}是一值集合，表示过程。我们把@${Proc}作为一种抽象数据类型。下面我们
考虑它的接口和规范。

}

我们还需要语法来创建和调用过程。对应的生成式为：

@envalign*{
        \mathit{Expression} &::= @tt{proc (@m{\mathit{Identifier}}) @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{proc-exp (var body)}} \\[5pt]
        \mathit{Expression} &::= @tt{(@m{\mathit{Expression}} @m{\mathit{Expression}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{call-exp (rator rand)}}}

在@tt{(proc @${var} @${body})}中，变量@${var}是 @emph{绑定变量} (@emph{bound
variable})或@emph{形式参数} (@emph{formal parameter})。在过程调用@tt{(call-exp
@${exp_1} @${exp_2})}中，表达式@${exp_1}是@emph{操作符} (@emph{operator})，表达
式@${exp_2}是@emph{操作数} (@emph{operand})或@emph{实际参数} (@emph{actual
parameter})。我们用名词@emph{实参} (@emph{argument})指代实际参数的值。

这里是这种语言的两个简单例子。

@nested[#:style 'code-inset]{
@verbatim|{
let f = proc (x) -(x,11)
in (f (f 77))

(proc (f) (f (f 77))
 proc (x) -(x,11))
}|
}

第一个程序创建一过程，将实参减11。它调用创建的过程@tt{f}，两次对77使用@tt{f}，得
到的答案为55。第二个程序创建一过程，它取一参数，连续两次对77使用其实参。然后该程
序将减11的过程传给该过程。结果仍然是55。

现在我们来看数据类型@${Proc}。它的接口包含构造器@tt{procedure}，用于创建过程值；
观测器@tt{apply-procedure}，用于调用过程值。

接下来我们的任务是确定表示一个过程需要在值里面包含什么信息。欲知此，我们考虑在程
序中任意位置写出@tt{proc}表达式时发生了什么。

词法定界规则告诉我们，调用一个过程时，过程的形式参数绑定到调用时的实参，然后在该
环境内求值过程的主体。过程中出现的自由变量也应该遵守词法绑定规则。考虑表达式：

@nested[#:style 'code-inset]{
@verbatim|{
let x = 200
in let f = proc (z) -(z,x)
   in let x = 100
      in let g = proc (z) -(z,x)
         in -((f 1), (g 1))
}|
}

这里我们两次求值表达式@tt{proc (z) -(z,x)}。第一次求值时，@tt{x}绑定到200，所以
根据词法绑定规则，得出的过程将实参减200。我们将其命名为@tt{f}。第二次求值时，
@tt{x}绑定到100，得出的过程应将实参减100。我们将该过程命名为@tt{g}。

这两个过程由同一个表达式生成，而表现必定不同。我们得出结论，@tt{proc}表达式的值
一定以某种方式依赖求值时的环境。因此，构造器@tt{procedure}必定取三个参数：绑定变
量，主体，以及环境。@tt{proc}表达式定义为：

@nested{

@nested[#:style 'code-inset]{
@verbatim|{
(value-of (proc-exp |@${var} |@${body}) |@${\rho})
= (proc-val (procedure |@${var} |@${body} |@${\rho}))
}|
}

其中，@tt{proc-val}是一构造器，像@tt{bool-val}和@tt{num-val}，生成一个@${Proc}的
表达值。
}

调用过程时，我们要找出操作符和操作数的值。如果操作符是一个@tt{proc-val}，那么我
们要用操作数的值调用它。

@nested{

@nested[#:style 'code-inset]{
@verbatim|{
(value-of (call-exp |@${rator} |@${rand}) |@${\rho})
= (let ((proc (expval->proc (value-of |@${rator} |@${\rho})))
        (arg (value-of |@${rand} |@${\rho})))
    (apply-procedure proc arg))
}|
}

这里，我们用了一个@elem[#:style question]{抽词器}@tt{expval->proc}，像
@tt{expval->num}，它判断表达值@tt{(value-of @${rator} @${\rho})}是否用
@tt{proc-val}生成，如果是，则从中提取出包含的过程。

}

@subsection[#:tag "s3.3.1"]{一个例子}

我们用一个例子展示定义的各部分是如何配合的。由于我们还没有写出过程的实现，这个计
算过程用@emph{规范}表示。令@${\rho}为任一环境。

@verbatim|{

(value-of
  <<let x = 200
    in let f = proc (z) -(z,x)
       in let x = 100
          in let g = proc (z) -(z,x)
             in -((f 1), (g 1))>>
  |@${\rho})

= (value-of
    <<let f = proc (z) -(z,x)
      in let x = 100
         in let g = proc (z) -(z,x)
            in -((f 1), (g 1))>>
    [x=|@${\lceil}200|@${\rceil}]|@${\rho})

= (value-of
    <<let x = 100
      in let g = proc (z) -(z,x)
         in -((f 1), (g 1))>>
    [f=(proc-val (procedure z <<-(z,x)>> [x=|@${\lceil}200|@${\rceil}]|@${\rho}))]
     [x=|@${\lceil}200|@${\rceil}]|@${\rho})

= (value-of
    <<let g = proc (z) -(z,x)
      in -((f 1), (g 1))>>
    [x=|@${\lceil}100|@${\rceil}]|@${\rho}
     [f=(proc-val (procedure z <<-(z,x)>> [x=|@${\lceil}200|@${\rceil}]|@${\rho}))]
      [x=|@${\lceil}200|@${\rceil}]|@${\rho})

= (value-of
    <<let g = proc (z) -(z,x)
      in -((f 1), (g 1))>>
    [g=(proc-val (procedure z <<-(z,x)>>
                            [x=|@${\lceil}100|@${\rceil}][f=...][x=|@${\lceil}200|@${\rceil}]|@${\rho}))]
     [x=|@${\lceil}100|@${\rceil}]|@${\rho}
      [f=(proc-val (procedure z <<-(z,x)>> [x=|@${\lceil}200|@${\rceil}]|@${\rho}))]
       [x=|@${\lceil}200|@${\rceil}]|@${\rho})

= |@${\lceil}(-
    (value-of <<(f 1)>>
      [g=(proc-val (procedure z <<-(z,x)>>
                              [x=|@${\lceil}100|@${\rceil}][f=...][x=|@${\lceil}200|@${\rceil}]|@${\rho}))]
       [x=|@${\lceil}100|@${\rceil}]|@${\rho}
        [f=(proc-val (procedure z <<-(z,x)>> [x=|@${\lceil}200|@${\rceil}]|@${\rho}))]
         [x=|@${\lceil}200|@${\rceil}]|@${\rho})
    (value-of <<(g 1)>>
      [g=(proc-val (procedure z <<-(z,x)>>
                              [x=|@${\lceil}100|@${\rceil}][f=...][x=|@${\lceil}200|@${\rceil}]|@${\rho}))]
       [x=|@${\lceil}100|@${\rceil}]|@${\rho}
        [f=(proc-val (procedure z <<-(z,x)>> [x=|@${\lceil}200|@${\rceil}]|@${\rho}))]
         [x=|@${\lceil}200|@${\rceil}]|@${\rho}))|@${\rceil}

= |@${\lceil}(-
    (apply-procedure
      (procedure z <<-(z,x)>> [x=|@${\lceil}200|@${\rceil}]|@${\rho})
      |@${\lceil}1|@${\rceil})
    (apply-procedure
      (procedure z <<-(z,x)>> [x=|@${\lceil}100|@${\rceil}][f=...][x=|@${\lceil}200|@${\rceil}]|@${\rho})
      |@${\lceil}1|@${\rceil}))|@${\rceil}

= |@${\lceil}(-
    (value-of <<-(z,x)>> [z=|@${\lceil}1|@${\rceil}][x=|@${\lceil}200|@${\rceil}]|@${\rho})
    (value-of <<-(z,x)>> [z=|@${\lceil}1|@${\rceil}][x=|@${\lceil}100|@${\rceil}][f=...][x=|@${\lceil}200|@${\rceil}]|@${\rho}))|@${\rceil}

= |@${\lceil}(- -199 -99)|@${\rceil}

= |@${\lceil}-100|@${\rceil}
}|

其中，绑定到的@tt{f}过程将实参减@${200}，绑定到@tt{g}的过程将实参减@${100}，所以
@tt{(f 1)}的值是@${-199}，@tt{(g 1)}的值是@${-99}。

@subsection[#:tag "s3.3.2"]{表示过程}

根据@secref{pr}中介绍的方法，我们可以按照过程表示法，用过程在
@tt{apply-procedure}中的动作表示它们。欲如此，我们定义@tt{procedure}的值为实现语
言的过程，它取一实参，返回规范指定的值：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(apply-procedure (procedure |@${var} |@${body} |@${\rho}) |@${val})
= (value-of |@${body} (extend-env |@${var} |@${val} |@${\rho}))
}|
}

因此，完整的实现是：

@racketblock[
@#,elem{@bold{@tt{proc?}} : @${\mathit{SchemeVal} \to \mathit{Bool}}}
(define proc?
  (lambda (val)
    (procedure? val)))

@#,elem{@bold{@tt{procedure}} : @${\mathit{Var} \times \mathit{Exp} \times \mathit{Env} \to \mathit{Proc}}}
(define procedure
  (lambda (var body env)
    (lambda (val)
      (value-of body (extend-env var val env)))))

@#,elem{@bold{@tt{apply-procedure}} : @${\mathit{Proc} \times \mathit{ExpVal} \to \mathit{ExpVal}}}
(define apply-procedure
  (lambda (proc1 val)
    (proc1 val)))
]

这里定义的函数@tt{proc?}有些不大准确，因为不是每个Scheme过程都能作为我们语言中的
过程。我们只是用来它定义数据类型@tt{expval}。

}

另一种方式是用@secref{dsr}那样的数据结构表示法。

@racketblock[
@#,elem{@bold{@tt{proc?}} : @${\mathit{SchemeVal} \to \mathit{Bool}}}
@#,elem{@bold{@tt{procedure}} : @${\mathit{Var} \times \mathit{Exp} \times \mathit{Env} \to \mathit{Proc}}}
(define-datatype proc proc?
  (procedure
   (var identifier?)
   (body expression?)
   (saved-env environment?)))

@#,elem{@bold{@tt{apply-procedure}} : @${\mathit{Proc} \times \mathit{ExpVal} \to \mathit{ExpVal}}}
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var val saved-env))))))
]

这些数据结构常称为@emph{闭包} (@emph{closure})，因为它们自给自足，包含过程调用所
需要的一切。有时，我们说过程@emph{闭合于}（@emph{closed over} 或@emph{closed in}）
创建时的环境。

显然，这些实现都满足过程接口的定义。

在完整的实现中，我们向数据类型@tt{expval}添加一种变体：

@nested{
@nested[#:style samepage]{
@racketblock[
(define-datatype exp-val exp-val?
  (num-val
    (val number?))
  (bool-val
    (val boolean?))
  (proc-val
    (val proc?)))
]
}

同时向@tt{value-of}添加两条新语句：

@codeblock[#:indent 7]{
(proc-exp (var body)
  (proc-val (procedure var body env)))

(call-exp (rator rand)
  (let ((proc (expval->proc (value-of rator env)))
        (arg (value-of rand env)))
    (apply-procedure proc arg)))
}

提醒：为语言的每个扩展写出规范。参见 @elem[#:style question]{第70页}的说明。

}

@exercise[#:level 1 #:tag "ex3.19"]{

在很多语言中，过程创建和命名必须同时进行。修改本节的语言，用@tt{letproc}替换
@tt{proc}，以支持此属性。

}

@exercise[#:level 1 #:tag "ex3.20"]{

在PROC中，过程只能有一个参数，但是可以用返回其他过程的过程来模拟多参数过程。例如，
可以写出这样的代码：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
let f = proc (x) proc (y) ...
in ((f 3) 4)
}|
}

这个小技巧叫做@emph{咖哩化} (@emph{Currying})，该过程则称作@emph{咖喱式}
(@emph{Curried})的。写出一个咖喱式的过程，它取两个参数，返回二者之和。在我们的语
言中，可以把@${x+y}写成@tt{-(x,-(0,y))}。

}
}

@exercise[#:level 2 #:tag "ex3.21"]{

扩展本节的语言，添加多参数过程及其调用，语法为：

@envalign*{
        \mathit{Expression} &::= @tt{proc (@m{\{\mathit{Identifier}\}^{*(,)}}) @m{\mathit{Expression}}} \\[-3pt]
        \mathit{Expression} &::= @tt{(@m{\mathit{Expression}} @m{\mathit{\{Expression\}^{*}}})}
}
}

@exercise[#:level 3 #:tag "ex3.22"]{

本节的具体语法中，内置操作（如差值）和过程调用使用不同的语法。修改具体语法，不要
让该语言的用户区分哪些是内置操作，哪些是定义的过程。根据所使用的解析技术，这道练
习可能很容易，也可能非常难。

}

@exercise[#:level 2 #:tag "ex3.23"]{

下面的PROC程序值是什么？

@nested[#:style 'code-inset]{
@verbatim|{
let makemult = proc (maker)
                proc (x)
                 if zero?(x)
                 then 0
                 else -(((maker maker) -(x,1)), -4)
in let times4 = proc (x) ((makemult makemult) x)
   in (times4 3)
}|
}

用这个程序里的小技巧写出PROC阶乘过程。提示：你可以使用咖喱化（练习3.20）定义双参
数过程@tt{times}。

}

@exercise[#:level 2 #:tag "ex3.24"]{

用上述程序里的小技巧写出两个互递归程序，@tt{odd}和@tt{even}，像练习3.32那样。

}

@exercise[#:level 1 #:tag "ex3.25"]{

提取上述练习中的技巧，可以用来定义任何PROC递归过程。考虑下面的代码：

@nested[#:style 'code-inset]{
@verbatim|{
let makerec = proc (f)
               let d = proc (x)
                        proc (z) ((f (x x)) z)
               in proc (n) ((f (d d)) n)
in let maketimes4 = proc (f)
                     proc (x)
                      if zero?(x)
                      then 0
                      else -((f -(x,1)), -4)
   in let times4 = (makerec maketimes4)
      in (times4 3)
}|
}

证明它返回12。
}

@exercise[#:level 2 #:tag "ex3.26"]{

我们用数据结构表示过程时，在闭包中记录了整个环境。但是显然，我们只需要自由变量的
绑定。修改过程的表示，只保留自由变量。

}

@exercise[#:level 1 #:tag "ex3.27"]{

向语言添加一种新的过程@tt{traceproc}。@tt{traceproc}像@tt{proc}一样，除了在进入
和退出时打印一条跟踪消息。

}

@exercise[#:level 2 #:tag "ex3.28"]{

设计过程的另一种方法是@emph{动态绑定} (@emph{dynamic binding})（或称@emph{动态定
界} (@emph{dynamic scoping})）：过程主体求值时的环境由扩展调用处的环境得到。例如，
在

@nested[#:style 'code-inset]{
@verbatim|{
let a = 3
in let p = proc (x) -(x,a)
       a = 5
   in -(a, (p 2))
}|
}

中，过程主体内的@tt{a}绑定到5，而不是3。修改语言，使用动态绑定。做两次，一次使用
过程表示法表示过程，一次使用数据结构表示法。

}

@exercise[#:level 2 #:tag "ex3.29"]{

很不幸的是，使用动态绑定的程序很可能异常难懂。例如，在词法绑定中，批量替换过程的
绑定变量，决不会改变程序的行为：我们甚至可以像@elem[#:style question]{3.6节}那样，
删除所有变量，将它们替换为词法地址。但是在动态绑定中，这种转换是危险的。

例如，在动态绑定中，过程@tt{proc (z) a}返回调用者环境中的变量@tt{a}。那么程序

@nested[#:style 'code-inset]{
@verbatim|{
let a = 3
in let p = proc (z) a
   in let f = proc (x) (p 0)
      in let a = 5
         in (f 2)
}|
}

返回5，因为调用处@tt{a}的值为5。如果@tt{f}的形式参数为@tt{a}呢？

}

@section[#:tag "s3.4"]{LETREC：支持递归过程的语言}

现在我们来定义一门新语言LETREC，让我们的语言支持递归。因为我们的语言只有单参数过
程，我们降低难度，只让@tt{letrec}表达式声明一个单参数过程，例如：

@nested[#:style 'code-inset]{
@verbatim|{
letrec double (x)
        = if zero?(x) then 0 else -((double -(x,1)), -2)
in (double 6)
}|
}

递归声明的左边是递归过程的名字以及绑定变量。@tt{=}右边是过程主体。它的生成式为：

@envalign*{
\mathit{Expression} &::= @tt{letrec @m{\mathit{Identifier}} (@m{\mathit{Identifier}}) = @m{\mathit{Expression}} in @m{\mathit{Expression}}} \\[-3pt]
  &\mathrel{\phantom{::=}} \fbox{@tt{letrec-exp (p-name b-var p-body letrec-body)}}}

@tt{letrec}表达式的值是其主体的值，在符合这种行为的环境中求出：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(value-of
  (letrec-exp |@${proc\mbox{-}name} |@${bound\mbox{-}var} |@${proc\mbox{-}body} |@${letrec\mbox{-}body})
  |@${\rho})
= (value-of
    |@${letrec\mbox{-}body}
    (extend-env-rec |@${proc\mbox{-}name} |@${bound\mbox{-}var} |@${proc\mbox{-}body} |@${\rho}))
}|
}

这里，我们给环境接口新添一个过程@tt{extend-env-rec}。但我们仍然得回答这个问题：
@tt{(extend-env-rec @${proc\mbox{-}name} @${bound\mbox{-}var}
@${proc\mbox{-}body} @${\rho})}应符合什么行为？

}

我们定义该环境的行为如下：设@${\rho_1}为@tt{(extend-env-rec @${proc\mbox{-}name}
@${bound\mbox{-}var} @${proc\mbox{-}body} @${\rho})}产生的环境，那么
@tt{(apply-env @${\rho_1} @${var})}应返回什么？

@itemlist[#:style 'ordered

 @item{如果变量@${var}与@${proc\mbox{-}name}相同，那么@tt{(apply-env @${\rho_1}
 @${var})}应返回一个闭包，其绑定变量是@${bound\mbox{-}var}，其主体为
 @${proc\mbox{-}body}，其环境为绑定@${proc\mbox{-}name}时所在的环境。但我们已经
 有这个环境了，那就是@${\rho_1}本身！所以：

 @nested[#:style 'code-inset]{
 @verbatim|{
 (apply-env |@${\rho_1} |@${proc\mbox{-}name})
 = (proc-val (procedure |@${bound\mbox{-}var} |@${proc\mbox{-}name} |@${\rho_1}))
 }|
 }
 }

 @item{如果@${var}与@${proc\mbox{-}name}不同，那么：

 @nested[#:style 'code-inset]{
 @verbatim|{
 (apply-env |@${\rho_1} |@${var}) = (apply-env |@${\rho} |@${var})
 }|
 }}

]

图3.10和图3.11展示了一个例子。图3.11的最后一行，递归调用@tt{double}找出了原来的
@tt{double}，正合所愿。

我们按这些要求实现@tt{extend-env-rec}。这里我们用抽象语法树表示。练习中讨论其他
实现策略。

如图3.12，在抽象语法树表示中，我们新添一种变体@tt{extend-env-rec}。
@tt{apply-env}倒数第二行中的@tt{env}对应上述@${\rho_1}。

@nested[#:style eopl-figure]{
@verbatim|{

(value-of <<letrec double(x) = if zero?(x)
                               then 0
                               else -((double -(x,1)), -2)
            in (double 6)>> |@${\rho_0})

= (value-of <<(double 6)>>
    (extend-env-rec double x <<if zero?(x) ...>> |@${\rho_0}))

= (apply-procedure
    (value-of <<double>> (extend-env-rec double x
                           <<if zero?(x) ...>> |@${\rho_0}))
    (value-of <<6>> (extend-env-rec double x
                      <<if zero?(x) ...>> |@${\rho_0})))

= (apply-procedure
    (value-of <<double>> (extend-env-rec double x
                           <<if zero?(x) ...>> |@${\rho_0}))
    |@${\lceil}6|@${\rceil})

= (value-of
    <<if zero?(x) ...>>
    [x=|@${\lceil}6|@${\rceil}](extend-env-rec
                   double x <<if zero?(x) ...>> |@${\rho_0}))

...

= (-
    (value-of
      <<(double -(x,1))>>
      [x=|@${\lceil}6|@${\rceil}](extend-env-rec
                      double x <<if zero?(x) ...>> |@${\rho_0}))
    -2)
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para (tt "extend-env-rec") "计算过程"))]
}

@nested[#:style eopl-figure]{
@verbatim|{

= (-
    (apply-procedure
      (value-of
        <<double>>
        [x=|@${\lceil}6|@${\rceil}](extend-env-rec
                      double x <<if zero?(x) ...>> |@${\rho_0}))
      (value-of
        <<(double -(x,1))>>
        [x=|@${\lceil}6|@${\rceil}](extend-env-rec
                      double x <<if zero?(x) ...>> |@${\rho_0})))
    -2)

= (-
    (apply-procedure
      (value-of
        <<double>>
        [x=|@${\lceil}6|@${\rceil}](extend-env-rec
                      double x <<if zero?(x) ...>> |@${\rho_0}))
      |@${\lceil}5|@${\rceil})
    -2)

= ...
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para (tt "extend-env-rec") "计算过程，续"))]
}

@nested[#:style eopl-figure]{
@racketblock[
(define-datatype environment environment?
  (empty-env)
  (extend-env
    (var identifier?)
    (val expval?)
    (env environment?))
  (extend-env-rec
    (p-name identifier?)
    (b-var identifier?)
    (body expression?)
    (env environment?)))

(define apply-env
  (lambda (env search-var)
    (cases environment env
      (empty-env ()
        (report-no-binding-found search-var))
      (extend-env (saved-var saved-val saved-env)
        (if (eqv? saved-var search-var)
          saved-val
          (apply-env saved-env search-var)))
      (extend-env-rec (p-name b-var p-body saved-env)
        (if (eqv? search-var p-name)
          (proc-val (procedure b-var p-body env))
          (apply-env saved-env search-var))))))]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "向环境添加" (tt "extend-env-rec")))]
}

@exercise[#:level 1 #:tag "ex3.30"]{

@tt{apply-env}倒数第二行调用@tt{proc-val}的目的是什么？

}

@exercise[#:level 1 #:tag "ex3.31"]{

扩展上面的语言，允许声明有任意多个参数的过程，像练习3.21那样。

}

@exercise[#:level 1 #:tag "ex3.32"]{

扩展上面的语言，允许声明有任意多个单参数互递归过程，例如：

@nested[#:style 'code-inset]{
@verbatim|{
letrec
  even(x) = if zero?(x) then 1 else (odd -(x,1))
  odd(x)  = if zero?(x) then 0 else (even -(x,1))
in (odd 13)
}|
}

}

@exercise[#:level 2 #:tag "ex3.33"]{

扩展上面的语言，允许声明有任意多个互递归过程，且参数数量任意，像练习3.21那样。

}

@exercise[#:level 3 #:tag "ex3.34"]{

用@secref{pr}环境的过程表示法实现@tt{extend-env-rec}。

}

@exercise[#:level 1 #:tag "ex3.35"]{

到目前为止，我们看到的表示法都很低效，因为每次查找过程时，它们都要新创建一个闭包。
但是每次的闭包都相同。我们可以只创建一次闭包，把值放入长度为1的向量，并主动放入
一个循环式结构中，像这样：

@centered{
@(image "../images/vector-env"
  #:scale 0.95
  #:suffixes (list ".pdf" ".svg")
  "vector环境")
}

这里是创建这种数据结构的代码：

@racketblock[
(define extend-env-rec
  (lambda (p-names b-vars bodies saved-env)
    (let ((vec (make-vector (length p-names))))
      (let ((new-env (extend-env p-names vec saved-env)))
        (make-proc-vec! vec 0
          b-vars bodies new-env)
        new-env))))
]

按此修改环境数据结构的定义和@tt{apply-env}，实现这种表示法。确保@tt{apply-env}总
是返回表达值。

}

@exercise[#:level 2 #:tag "ex3.36"]{

扩展这种实现，使之能够处理练习3.32中的语言。

}

@exercise[#:level 2 #:tag "ex3.37"]{

使用动态绑定（练习3.28），用@tt{let}就可以创建递归过程；不需要任何特殊的递归机制。
这是出于历史兴趣。在早年的编程语言设计中，像@secref{s3.4}讨论的那些方法还鲜为人
知。要明白动态绑定实现的递归，试试程序：

@nested[#:style 'code-inset]{
@verbatim|{
let fact = proc (n) add1(n)
in let fact = proc (n)
               if zero?(n)
               then 1
               else *(n, (fact -(n,1)))
   in (fact 5)
}|
}

试试词法绑定，再试试动态绑定。用动态绑定写出@secref{s3.4}中的互递归过程@tt{even}
和@tt{odd}。

}

@section[#:tag "s3.5"]{定界和变量绑定}

我们已经在很多地方见到过变量的声明和使用，现在我们来系统讨论这些思想。

在大多数编程语言中，变量只能以两种方式出现：@emph{引用} (@emph{reference})或
@emph{声明} (@emph{declaration})。变量引用就是使用变量。例如，在Scheme表达式

@nested{
@codeblock{(f x y)}

中，所有的变量@tt{f}，@tt{x}和@tt{y}都为引用。但在

@codeblock{(lambda (x) (+ x 3))}

或

@codeblock{(let ((x (+ y 7))) (+ x 3))}

中，第一个出现的@tt{x}是声明：引入一个变量，作为某个值的名字。在@tt{lambda}表达
式中，变量的值在过程调用时提供。在@tt{let}表达式，变量的值由表达式@tt{(+ y z)}求
得。

}

我们说变量引用由对应的声明@emph{绑定} (@emph{bound})到值。在@secref{o-f}，我们已
经见过用声明绑定变量的例子。

@nested[#:style eopl-figure]{
@centered{
@(image "../images/simple-contour"
  #:suffixes (list ".pdf" ".svg")
  "简单等深线")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "简单等深线"))]
}

大多数编程语言中的声明都有有限的作用范围，所以同一个变量名在程序的部分可用于不同
的目的。例如，我们反复把@tt{lst}用作绑定变量，每次它的作用范围都限制到对应的
@tt{lambda}表达式主体内。

每种编程语言都有一些规则来判断每个变量引用指代哪个声明。这些规则通常叫做@emph{定
界} (@emph{scoping})规则。程序中，声明有效的部分叫做声明的@emph{作用范围}
(@emph{scope})。

不需要执行程序，就能判断各个变量引用对应于哪个声明。这样的属性不需要执行程序就能
算出来，叫做@emph{静态} (@emph{static})属性。

要找出某个变量引用对应于哪一声明，我们@emph{向外} (@emph{outward})查找，直到找出
变量的声明。这里有个简单的Scheme示例。

@nested[#:style 'code-inset]{
@verbatim|{
(let ((x 3)                             |@elem{称之为@tt{x1}}
      (y 4))
  (+ (let ((x                           |@elem{称之为@tt{x2}}
             (+ y 5)))
       (* x y))                         |@elem{这个@tt{x}指代@tt{x2}}
     x))                                |@elem{这个@tt{x}指代@tt{x1}}
}|
}

在这个例子中，内层的@tt{x}绑定到9，所以表达式的值为：

@nested[#:style 'code-inset]{
@verbatim|{
(let ((x 3)
      (y 4))
  (+ (let ((x
             (+ y 5)))
       (* x y))
     x))

= (+ (let ((x
             (+ 4 5)))
       (* x 4))
     3)

= (+ (let ((x 9))
       (* x 4))
     3)

= (+ 36
     3)

= 39
}|
}

这样的定界规则叫做@emph{词法定界} (@emph{lexical scoping})规则，这样声明的变量叫
做@emph{词法变量} (@emph{lexical variable})。

使用词法定界，我们可以重新声明一个变量，给一个作用范围捅出个“洞”。这样的内层声
明@emph{遮蔽} (@emph{shadow})外层声明。例如，在上例的乘式@tt{(* x y)}中，内层
@tt{x}遮蔽了外层的。

词法作用范围是嵌套式的：每个作用范围完全包裹在另一个里面。我们用@deftech{等深线}
(@emph{contour diagram})解释这点。图3.13展示了上例的@tech{等深线}。每个作用范围
用一个框圈起来，垂线连接声明与其作用范围。

图3.14展示了一个更复杂的程序，绘有等深线。这里面，在第5，第7和第8行，表达式
@tt{(+ x y z)}出现了三次。第5行在@tt{x2}和@tt{z2}的作用范围内，@tt{x2}和@tt{z2}
的作用范围在@tt{z1}的作用范围内，@tt{z1}的作用范围在@tt{x1}和@tt{y1}的作用范围内。
所以，第5行的@tt{x}指代@tt{x2}，@tt{y}指代@tt{y1}，@tt{z}指代@tt{z2}。第7行在
@tt{x4}和@tt{y2}的作用范围内，@tt{x4}和@tt{y2}的作用范围在@tt{x2}和@tt{z2}的作用
范围内，@tt{x2}和@tt{z2}的作用范围在@tt{z1}的作用范围内，@tt{z1}的作用范围在
@tt{x1}和@tt{y1}的作用范围内。所以，第7行的@tt{x}指代@tt{x4}，@tt{y}指代@tt{y2}，
@tt{z}指代@tt{z2}。最后，第8行在@tt{x3}的作用范围内，@tt{x3}的作用范围在@tt{x2}
和@tt{z2}的作用范围内，@tt{x2}和@tt{z2}的作用范围在@tt{z1}的作用范围内，@tt{z1}
的作用范围在@tt{x1}和@tt{y1}的作用范围内。所以，第8行的@tt{x}指代@tt{x3}，@tt{y}
指代@tt{y1}，@tt{z}指代@tt{z2}。

@nested[#:style eopl-figure]{
@centered{
@(image "../images/complicated-contour"
  #:suffixes (list ".pdf" ".svg")
  "较复杂的等深线")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "较复杂的等深线"))]
}

变量与值的对应关系叫做@emph{绑定} (@emph{binding})。可以查看我们语言的规范来理解
绑定如何创建。

由@tt{proc}声明的变量在过程调用时绑定。

@nested[#:style 'code-inset]{
@verbatim|{
(apply-procedure (procedure |@${var} |@${body} |@${\rho}) |@${val})
= (value-of |@${body} (extend-env |@${var} |@${val} |@${\rho}))
}|
}

@tt{let}变量绑定到声明右边的值。

@nested[#:style 'code-inset]{
@verbatim|{
(value-of (let-exp |@${var} |@${val} |@${body}) |@${\rho})
= (value-of |@${body} (extend-env |@${var} |@${val} |@${\rho}))
}|
}

@tt{letrec}声明的变量也要绑定到声明右边。

@nested[#:style samepage]{
@nested[#:style 'code-inset]{
@verbatim|{
(value-of
  (letrec-exp |@${proc\mbox{-}name} |@${bound\mbox{-}var} |@${proc\mbox{-}body} |@${letrec\mbox{-}body})
  |@${\rho})
= (value-of
    |@${letrec\mbox{-}body}
    (extend-env-rec |@${proc\mbox{-}name} |@${bound\mbox{-}var} |@${proc\mbox{-}body} |@${\rho}))
}|
}
}

绑定的@emph{期限} (@emph{extent})是指绑定保持的时长。在我们的语言中，就像在
Scheme中一样，所有的绑定都是@emph{半无限} (@emph{semi-infinite})的，意思是变量一
旦绑定，该绑定就要（至少是有可能）无限制地保留。这是因为绑定可能藏在已返回的闭包
之中。在半无限的语言中，垃圾回收器收集不能再访问的绑定。这只能在运行时确定，因此
我们说这是一条@emph{动态} (@emph{dynamic})属性。

很可惜的是，“动态”有时表示“在表达式求值期间”，有时却又表示“无法事先计算”。
如果我们不允许@tt{let}的值为过程，那么let绑定会在@tt{let}主体求值结束时到期。这
叫做@emph{动态}期限，而它是一条@emph{静态}属性。因为这种期限是一条静态属性，所以
我们可以准确预测绑定何时可以抛弃。3.28等几道练习中的动态绑定表现类似。

@section[#:tag "s3.6"]{消除变量名}

定界算法的执行过程可以看作始自变量引用的外出旅行。在旅程中，到达对应的声明之前可
能会跨过很多等深线。跨越的等深线数目叫做变量引用的@emph{词深} (@emph{lexical
depth})（或 @emph{静深} (@emph{static depth})）。由于惯用“从0开始的索引”，所以
不计最后跨过的等深线。例如，在Scheme表达式

@nested{
@racketblock[
(lambda (x)
  ((lambda (a)
     (x a))
   x))
]

中，最后一行@tt{x}的引用以及@tt{a}的引用词深均为0，而第三行@tt{x}的引用词深为1。

}

因此，我们可以完全消除变量名，写成这样：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(nameless-lambda
  ((nameless-lambda
     (#1 #0))
   #0))
}|
}

这里，每个@tt{nameless-lambda}声明了一个新的无名变量，每个变量引用由其词深替代；
这个数字准确标示了要使用的声明。这些数字叫做@emph{词法地址} (@emph{lexical
address})或@emph{德布鲁金索引} (@emph{de Bruijn index})。编译器例行计算每个变量
引用的词法地址。除非用来提供调试信息，一旦完成，变量名即可丢弃。

}

这样记录信息有用，因为词法地址@emph{预测}了怎样从环境中找出某个变量。

考虑一个我们语言的表达式：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
let x = |@${exp_1}
in let y = |@${exp_2}
   in -(x,y)
}|
}

在差值表达式中，@tt{y}和@tt{x}的词深分别为0和1。
}

现在，设在某个适当环境中，@${exp_1}和@${exp_2}的值分别为@${val_1}和@${val_2}，那
么这个表达式的值为：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(value-of
  <<let x = |@${exp_1}
    in let y = |@${exp_2}
       in -(x,y)>>
  |@${\rho})
=
(value-of
  <<let y = |@${exp_2}
    in -(x,y)>>
  [x=|@${val_1}]|@${\rho})
=
(value-of
  <<-(x,y)>>
  [y=|@${val_2}][x=|@${val_1}]|@${\rho})
}|
}

那么求值差值表达式时，@tt{y}深度为0,@tt{x}深度为1，正如词深预测的那样。

}

如果用关联列表表示环境（见练习2.5），那么环境看起来像是：

@nested{
@centered{
@(image "../images/lexical-addr-env"
  #:suffixes (list ".pdf" ".svg")
  "关联列表表示的词法地址环境")
}

所以不论@${val_1}和@${val_2}值为何，@tt{x}和@tt{y}的值都是取环境中第1个元素的余
项和第0个元素的余项。

}

过程的主体也是这样。考虑：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
let a = 5
in proc (x) -(x,1)
}|
}

在过程的主体中，@tt{x}的词深是0，@tt{a}的词深是1。

}

这个表达式的值为：

@nested[#:style 'code-inset]{
@verbatim|{
(value-of
  <<let a = 5 in proc (x) -(x,a)>>
  |@${\rho})
= (value-of <<proc (x) -(x,a)>>
    (extend-env a |@${\lceil}5|@${\rceil} |@${\rho}))
= (proc-val (procedure x <<-(x,a)>> [a=|@${\lceil}5|@${\rceil}]|@${\rho}))
}|
}

这个过程的主体只能通过@tt{apply-procedure}求值：

@nested[#:style samepage]{
@nested[#:style 'code-inset]{
@verbatim|{
(apply-procedure
  (procedure x <<-(x,a)>> [a=|@${\lceil}5|@${\rceil}]|@${\rho})
  |@${\lceil}7|@${\rceil})
= (value-of <<-(x,a)>>
    [x=|@${\lceil}7|@${\rceil}][a=|@${\lceil}5|@${\rceil}]|@${\rho})
}|
}
}

每个变量又一次在词深预测的位置从环境中找到。

@section[#:tag "s3.7"]{实现词法地址}

现在，我们来实现上面分析的词法地址。我们写个过程@tt{translation-of-program}，它
取一程序，从声明中移除所有变量，并将每个变量引用替换为词深。

例如，程序

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
let x = 37
in proc (y)
    let z = -(y,x)
    in -(x,y)
}|
}

将翻译为：

@racketblock[
#(struct:a-program
   #(struct:nameless-let-exp
      #(struct:const-exp 37)
      #(struct:nameless-proc-exp
         #(struct:nameless-let-exp
            #(struct:diff-exp
               #(struct:nameless-var-exp 0)
               #(struct:nameless-var-exp 1))
            #(struct:diff-exp
               #(struct:nameless-var-exp 2)
               #(struct:nameless-var-exp 1))))))
]

然后，我们另写一个@tt{value-of-program}，不必把变量放入环境就能求取无名程序的值。

}

@subsection[#:tag "s3.7.1"]{翻译器}

因为是写翻译器，我们得知道源语言和目标语言。目标语言中的某些部分源语言中没有，像
@tt{nameless-var-exp}和@tt{nameless-let-exp}；源语言中的某些部分目标语言中没有，
它们由后者中的对应构造器取代，像@tt{var-exp}和@tt{let-exp}。

我们可以给每种语言写一个@tt{define-datatype}，也可以让二者用同一个。因为我们使用
的前端是SLLGEN，后者更容易。我们给SLLGEN的语法添加生成式：

@envalign*{
        \mathit{Expression} &::= @tt{%lexref @m{\mathit{number}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{nameless-var-exp (num)}} \\[5pt]
        \mathit{Expression} &::= @tt{%let @m{\mathit{Expression}} in @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{nameless-let-exp (exp1 body)}} \\[5pt]
        \mathit{Expression} &::= @tt{%lexproc @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{nameless-proc-exp (body)}}}

新的生成式中，我们用@tt{%}开头的名字，因为在我们的语言中，@tt{%}通常是注释字符。

我们的翻译器将拒绝任何含有无名构造器（@tt{nameless-var-exp}，
@tt{nameless-let-exp}或@tt{nameless-proc-exp}）的程序。具名构造器（@tt{var-exp}，
@tt{let-exp}或@tt{proc-exp}）应被替换，我们的解释器将拒绝任何含有这些的程序。

要计算任何变量引用的词法地址，我们需要它所在的作用范围。这是一种@emph{上下文}
(@emph{context})信息，所以它就像@secref{apca}的继承属性一样。

那么@tt{translation-of-program}将取两个参数：一个表达式和一个@emph{静态环境}
(@emph{static environment})。静态环境是一个变量列表，表示当前表达式所在的作用范
围。最内部作用范围声明的变量成为列表的第一个元素。

例如，翻译上例中的最后一行时，静态环境为：

@nested{

@centered{
@tt{(z y x)}
}

所以，在静态环境中搜索变量就是查找它在静态环境中的位置，也就是词法地址：查得
@tt{x}为2，@tt{y}为1，@tt{z}为0。

}

@nested[#:style eopl-figure]{

@racketblock[

@#,elem{@${\mathit{Senv}} = @${\mathit{Listof}}@tt{(@${\mathit{Sym}})}}
@#,elem{@${\mathit{Lexaddr}} = @${\mathit{N}}}

@#,elem{@bold{@tt{empty-senv}} : @${\mathit{()} \to \mathit{Senv}}}
(define empty-senv
  (lambda ()
    '()))

@#,elem{@bold{@tt{extend-senv}} : @${\mathit{Var} \times \mathit{Senv} \to \mathit{Senv}}}
(define extend-senv
  (lambda (var senv)
    (cons var senv)))

@#,elem{@bold{@tt{apply-senv}} : @${\mathit{Senv} \times \mathit{Var} \to \mathit{Lexaddr}}}
(define apply-senv
  (lambda (senv var)
    (cond
      ((null? senv)
       (report-no-binding-found var))
      ((eqv? var (car senv))
       0)
      (else
        (+ 1 (apply-senv (cdr senv) var))))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "实现静态环境"))]
}

进入新的作用范围就要给静态环境添加一个新元素。我们添加过程@tt{extend-senv}来完成
这点。

由于静态环境只是变量列表，这些过程很容易实现，如图3.15所示。

翻译器有两个过程：@tt{translation-of}处理表达式，@tt{translation-of-program}处理
程序。

@tt{senv}表示一些声明，我们从中翻译表达式@tt{e}。要完成这点，我们像练习1.33或
2.26那样递归复制语法树，除了：

@nested{

@itemlist[#:style 'ordered

 @item{调用@tt{apply-senv}，用正确的词法地址，把每个@tt{var-exp}替换为
 @tt{nameless-var-exp}。}

 @item{把每个@tt{let-exp}替换为一个@tt{nameless-let-exp}。新表达式的右侧由旧表达
 式右侧译得。这与原式的作用范围相同，所以我们在同样的静态环境@tt{senv}中翻译。新
 表达式的主体由旧表达式的主体译得。但是主体位于新的作用范围内，多了一个绑定变量
 @${var}。所以我们在静态环境@tt{(extend-senv @${var} @${senv})}中翻译主体。}

 @item{把每个@tt{proc-exp}替换为一个@tt{nameless-proc-exp}，主体在新的作用范围内
 译得，该作用范围由静态环境@tt{(extend-senv @${var} senv)}表示。}

]

@tt{translation-of}代码如图3.16所示。

}

过程@tt{translation-of-program}在适当的初始静态环境中运行@tt{translation-of}。

@nested[#:style samepage]{
@racketblock[
@#,elem{@bold{@tt{translation-of}} : @${\mathit{Program} \to \mathit{Nameless\mbox{-}exp}}}
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (a-program
          (translation-of exp1 (init-senv)))))))

@#,elem{@bold{@tt{translation-of}} : @${\mathit{()} \to \mathit{Senv}}}
(define init-senv
  (lambda ()
    (extend-senv 'i
      (extend-senv 'v
        (extend-senv 'x
          (empty-senv))))))
]
}

@nested[#:style eopl-figure]{

@racketblock[
@#,elem{@bold{@tt{translation-of}} : @${\mathit{Exp} \times \mathit{Senv} \to \mathit{Nameless\mbox{-}exp}}}
(define translation-of
  (lambda (exp senv)
    (cases expression exp
      (const-exp (num)
        (const-exp num))
      (diff-exp (exp1 exp2)
        (diff-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)))
      (zero?-exp (exp1)
        (zero?-exp
          (translation-of exp1 senv)))
      (if-exp (exp1 exp2 exp3)
        (if-exp
          (translation-of exp1 senv)
          (translation-of exp2 senv)
          (translation-of exp3 senv)))
      (var-exp (var)
        (nameless-var-exp
          (apply-senv senv var)))
      (let-exp (var exp1 body)
        (nameless-let-exp
          (translation-of exp1 senv)
          (translation-of body
            (extend-senv var senv))))
      (proc-exp (var body)
        (nameless-proc-exp
          (translation-of body
            (extend-senv var senv))))
      (call-exp (rator rand)
        (call-exp
          (translation-of rator senv)
          (translation-of rand senv)))
      (else
        (report-invalid-source-expression exp)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "词法地址翻译器"))]
}

@subsection[#:tag "s3.7.2"]{无名解释器}

我们的解释器利用词法分析器的预测，从而避免在运行时直接搜索变量。

由于我们的程序中没有任何变量，我们不能把变量放入环境中；但是因为我们准确知道在环
境中到哪儿去找，我们不需要！

我们的顶层过程是@tt{run}：

@racketblock[
@#,elem{@bold{@tt{run}} : @${\mathit{String} \to \mathit{ExpVal}}}
(define run
  (lambda (string)
    (value-of-program
     (translation-of-program
      (scan&parse string)))))
]

我们不用全功能的环境，而是用无名环境，其接口如下：

@envalign*{
@bold{@tt{nameless-environment?}}  &: \mathit{SchemeVal} \to \mathit{Bool} \\
@bold{@tt{empty-nameless-env}}     &: \mathit{()} \to \mathit{Nameless\mbox{-}env} \\
@bold{@tt{extend-nameless-env}}    &: \mathit{ExpVal} \times \mathit{Nameless\mbox{-}env} \to \mathit{Nameless\mbox{-}env} \\
@bold{@tt{apply-nameless-env}}     &: \mathit{Nameless\mbox{-}env} \times \mathit{Lexaddr} \to \mathit{DenVal}
}

我们可以用指代值列表实现无名环境，这样@tt{apply-nameless-env}只须调用
@tt{list-ref}。这种实现如图3.17所示。

@nested[#:style eopl-figure]{

@racketblock[
@#,elem{@bold{@tt{nameless-environment?}} : @${\mathit{SchemeVal} \to \mathit{Bool}}}
(define nameless-environment?
  (lambda (x)
    ((list-of exp-val?) x)))

@#,elem{@bold{@tt{empty-nameless-env}} : @${\mathit{()} \to \mathit{Nameless\mbox{-}env}}}
(define empty-nameless-env
  (lambda () '()))

@#,elem{@bold{@tt{extend-nameless-env}} : @${\mathit{Expval} \times \mathit{Nameless\mbox{-}env} \to \mathit{Nameless\mbox{-}env}}}
(define extend-nameless-env
  (lambda (val nameless-env)
    (cons val nameless-env)))

@#,elem{@bold{@tt{apply-nameless-env}} : @${\mathit{Nameless\mbox{-}env} \times \mathit{Lexaddr} \to \mathit{DenVal}}}
(define apply-nameless-env
  (lambda (nameless-env n)
    (list-ref nameless-env n)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "无名环境"))]
}

在@elem[#:style question]{第93页}例子中的最后一行，无名环境看起来像是：

@centered{
@(image "../images/nameless-env"
  #:suffixes (list ".pdf" ".svg")
  "无名环境")
}

我们更改了环境接口，需要查看代码中所有依赖接口的地方。我们的解释器中使用环境的只
有两处：过程实现和@tt{value-of}。

修改过程规范时，只用把旧规范中的变量名移除：

@nested[#:style 'code-inset]{
@verbatim|{
(apply-procedure (procedure |@${var} |@${body} |@${\rho}) |@${val})
= (value-of |@${body} (extend-nameless-env |@${val} |@${\rho}))
}|
}

实现这一规范时可定义：

@nested[#:style samepage]{
@racketblock[
@#,elem{@bold{@tt{procedure}} : @${\mathit{Nameless\mbox{-}exp} \times \mathit{Nameless\mbox{-}env} \to \mathit{Proc}}}
(define-datatype proc proc?
  (procedure
   (body expression?)
   (saved-nameless-env nameless-environment?)))

@#,elem{@bold{@tt{apply-procedure}} : @${\mathit{Proc} \times \mathit{ExpVal} \to \mathit{ExpVal}}}
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure
        (body saved-nameless-env)
        (value-of body
          (extend-nameless-env val saved-nameless-env))))))
]
}

现在，我们可以写出@tt{value-of}。它大部分与前一个解释器相同，只是原先使用
@tt{env}的地方现在用@tt{nameless-env}。但我们要处理新的部分：
@tt{nameless-var-exp}，@tt{nameless-let-exp}和@tt{nameless-proc-exp}，它们分别对
应并取代@tt{var-exp}，@tt{let-exp}和@tt{proc-exp}。实现如图3.18所示。
@tt{nameless-var-exp}用于环境查询。@tt{nameless-let-exp}先求出式子右边的
@${exp_1}，然后用式子右边的值扩展环境，并在该环境内求值主体。这和@tt{let}做的相
同，只是没有变量。@tt{nameless-proc}生成一个@tt{proc}，随后可供
@tt{apply-procedure}使用。

@nested[#:style eopl-figure]{

@racketblock[
@#,elem{@bold{@tt{value-of}} : @${\mathit{Nameless\mbox{-}exp} \times \mathit{Nameless\mbox{-}env} \to \mathit{ExpVal}}}
(define value-of
  (lambda (exp nameless-env)
    (cases expression exp

      (const-exp (num) @#,elem{@${\ldots} @emph{同前} @${\ldots}})
      (diff-exp (exp1 exp2) @#,elem{@${\ldots} @emph{同前} @${\ldots}})
      (zero?-exp (exp1) @#,elem{@${\ldots} @emph{同前} @${\ldots}})
      (if-exp (exp1 exp2 exp3) @#,elem{@${\ldots} @emph{同前} @${\ldots}})
      (call-exp (rator rand) @#,elem{@${\ldots} @emph{同前} @${\ldots}})

      (nameless-var-exp (n)
        (apply-nameless-env nameless-env n))

      (nameless-let-exp
        (exp1 body)
        (let ((val (value-of exp1 nameless-env)))
          (value-of body
            (extend-nameless-env val nameless-env))))

      (nameless-proc-exp (body)
        (proc-val
          (procedure body nameless-env)))

      (else
        (report-invalid-translated-expression exp)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "无名解释器的" (tt "value-of")))]
}

最后是新的@tt{value-of-program}：

@racketblock[
@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Nameless\mbox{-}program} \to \mathit{ExpVal}}}
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-nameless-env))))))
]

@exercise[#:level 1 #:tag "ex3.38"]{

扩展词法地址翻译器和解释器，处理练习3.12中的@tt{cond}。

}

@exercise[#:level 1 #:tag "ex3.39"]{

扩展词法地址翻译器和解释器，处理练习3.18中的@tt{pack}。

}

@exercise[#:level 2 #:tag "ex3.40"]{

扩展词法地址翻译器和解释器，处理@tt{letrec}。修改@tt{translation-of}的上下文参数，
不仅记录每个绑定变量名，也记录变量是否由@tt{letrec}绑定。对@tt{letrec}绑定变量的
引用，生成一种新的引用，名为@tt{nameless-letrec-var-exp}。然后可以继续用上面的无
名环境表示法，而解释器要对@tt{nameless-letrec-var-exp}做适当处理。

}

@exercise[#:level 2 #:tag "ex3.41"]{

修改词法地址翻译器和解释器，处理多参数的@tt{let}表达式，过程和过程调用，像练习
3.21那样。用肋排表示法（练习2.21）表示无名环境。在这种表示法中，词法地址包含两个
非负数：词深指示跨过的等深线数目，与之前相同；@emph{位置} (@emph{position})指示
变量在声明中的位置。

}

@exercise[#:level 3 #:tag "ex3.42"]{

修改词法地址翻译器和解释器，处理练习3.26中的瘦身过程表示法。要完成这点，你不能在
@tt{(extend-senv @${var} @${senv})}中翻译过程的主体，而是在一个新的静态环境中，
它准确标明了各个变量在瘦身表示法中的位于何处。

}

@exercise[#:level 3 #:tag "ex3.43"]{

翻译器不仅仅可以记录变量的名字。例如，考虑程序：

@nested[#:style 'code-inset]{
@verbatim|{
let x = 3
in let f = proc (y) -(y,x)
   in (f 13)
}|
}

这里，我们不必运行就能看出：在过程调用处，@tt{f}绑定到一个过程，其主体为
@tt{-(y,x)}，@tt{x}的值与过程创建处相同。因此我们完全可以避免在环境中查找@tt{f}。
扩展翻译器，记录“已知过程”，为这样的过程生成代码，避免搜索环境。

}

@exercise[#:level 3 #:tag "ex3.44"]{

在前一个例子中，@tt{f}的唯一用途是作为一个已知过程。因此，由表达式@tt{proc (y)
-(y,x)}产生的过程从未使用。修改解释器，避免产生这样的过程。

}
