#lang scribble/book
@(require "style.rkt"
          latex-utils/scribble/theorem
          latex-utils/scribble/math
          latex-utils/scribble/utils
          scribble/manual
          scribble-math
          scribble/example
          scribble/core
          scribble/example
          scriblib/footnote
          racket/sandbox)
@(define (List-of-Int-$) @${\mathit{List\mbox{-}of\mbox{-}Int}})
@(define (List-of-Int-m) @m{\mathit{List\mbox{-}of\mbox{-}Int}})
@(define (List-of-Int-raw) "\\mathit{List\\mbox{-}of\\mbox{-}Int}")

@mainmatter

@title[#:style 'numbered #:tag "isd"]{归纳式数据集}

解释器与检查器一类的程序是编程语言处理器的核心，本章介绍写这些用到的基本编程工具。

因为编程语言的语法通常为嵌套或者树状结构，递归将是我们的主要技巧。@secref{rd}和
@secref{drp}介绍递推定义数据结构的方法，并展示如何用这类定义指导递归程序的编写。
@secref{apca}展示如何将这些技巧推广到更为复杂的程序。本章以大量练习作结。这些练
习是本章的核心。欲掌握本书余下部分依赖的递归编程技巧，得自它们的经验不可或缺。

@section[#:tag "rd"]{递推定义的数据}

编写过程代码时，必须明确知道什么样的值能作为过程的参数，什么样的值是过程的合法返
回值。这些值的集合通常很复杂。本节介绍定义值集合的形式化技术。

@subsection[#:tag "is"]{归纳定义法}

归纳定义法是定义值集合的有效方法。为解释这一方法，我们用它来描述自然数 @${N =
{0,1,2,...}} 的某一子集@${S}。

@; definition: (def #:title title #:tag tag pre-flow ...)
@; @def {
@; 自然数@${n}属于@${S}，当且仅当：
@; @itemlist[#:style 'ordered

@;  @item{@${{n = 0}}，或}

@;  @item{@${n - 3 \in S}。

@; }]
@; }
@env["sdef"]{
 自然数@m{n}属于@m{S}，当且仅当：
 \begin{enumerate}
  \item @m{n = 0}，或
  \item @m{n - 3 \in S}
 \end{enumerate}
}

来看看如何用这一定义判断哪些自然数属于@${S}。已知@${0 \in S}，因此@${3 \in S}，
因为@${(3 - 3) = 0}，而@${0 \in S}。同样地，@${6 \in S}，因为@${(6 - 3) = 3}，而
@${3 \in S}。依此类推，可得结论：所有@${3}的整数倍都属于@${S}。

其他自然数呢？@${1 \in S}吗？已知@${1 \ne 0}，所以条件一不满足。此外，@${(1 - 3)
= -2}，不是自然数，故不是@${S}的元素，因此条件二不满足。因为@${1}不满足任一条件，
所以@${1 \notin S}。同样地，@${2 \notin S}。@${4}呢？仅当@${1 \in S}时@${4 \in
S}。但@${1 \notin S}，所以@${4 \notin S}。同理可得，如果@${n}是自然数且不是@${3}
的整数倍，则@${n \notin S}。

据此推论，可得@${S}是@${3}的整数倍自然数集合。

可以用该定义编写一个函数，判断一个自然数@${n}是否属于@${S}。

@; codeblock with contracts and usage
@nested[#:style samepage]{
@racketblock[
@#,elem{@bold{@tt{in-S?}} : @${N \to Bool}}
@#,elem{@bold{用法} : @tt{(in-S? n) = #t 若 n 属于 S，否则 #f}}
(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))
]
}
@;

这里根据定义，我们用Scheme编写了一个递归过程。符号 @racket[in-S? : @#,elem{@${N \to Bool}}]  @;contract
是一条注释，称为该函数的@emph{合约} (@emph{contact})。它表示@racket[in-S?] 应为
一过程，取一自然数，产生一布尔值。这样的注释对阅读和编写代码很有帮助。

要判断是否@${n \in S}，先判断是否@${n = 0}。如果是，那么答案为真。否则，判断是否
@${n - 3 \in S}。欲知此，首先判断是否@${(n - 3) \geqslant 0}。如果是，那么可以用
我们的过程判断它是否属于@${S}。如果不是，那么@${n}不可能属于@${S}。

@${S}又能够定义为：

@; @def{
@; 集合@${S}为@${N}所包含的集合中，满足如下两条性质的最小集合：

@; @itemlist[#:style 'ordered

@;  @item{@${0 \in S}，且}

@;  @item{若@${n \in S}，则@${n + 3 \in S}。}

@; ]
@; }
@env["sdef"]{
 集合@m{S}为@m{N}所包含的集合中，满足如下两条性质的最小集合：
 \begin{enumerate}
  \item @m{0 \in S}，且
  \item 若@m{n \in S}，则@m{n + 3 \in S}。
 \end{enumerate}
}

“最小集合”是指该集合满足性质 1 和 2，并且是其他任何满足性质 1 和 2 的集合的子
集。易知只能有一个这样的集合：如果@${S_1}和@${S_2}都满足性质 1 和 2，并且都为最
小，那么@${S_1 \subseteq S_2}（因为@${S_1}最小）且@${S_2 \subseteq S_1}（因为
@${S_2}最小），因此@${S_1 = S_2}。之所以需要这一额外条件，是因为否则的话将有许多
集合满足其他两个条件（见练习1.3）。

该定义还能表示为：

@; infer should be implemented in scribble rather than use \infer directly
@; infer: (infer conclusion hypothesis ...)
@; @infer[${0 \in S}]
@$${\infer{0 \in S}{}}

@; @infer[${(n + 3) \in S}]{$@{n \in S}}
@$${\infer{(n + 3) \in S}{n \in S}}

这只是前一定义的简便表示。每个条目称为一条@emph{推理规则} (@emph{rule of
inference})，或称@emph{规则} (@emph{rule})；水平线读作“若-则”。线上部分称
作@emph{假设} (@emph{hypothesis}) 或者@emph{前件} (@emph{antecedent})；线下部分
称作@emph{结论} (@emph{conclusion}) 或者@emph{后件} (@emph{consequent})。要罗列
两个或更多假设，用“和”连接（见定义1.1.5）。没有假设的规则称作@emph{公理}
(@emph{axiom})。写公理时通常不加水平线，如：

@$${0 \in S}

该规则意为，自然数@${n}属于@${S}，当且仅当能用有限次推理规则，从公理推得陈述
“@${n \in S}”。这一解释自然使@${S}成为闭合于该规则的最小集合。

这些定义意思相同。我们把版本一称作@emph{自顶向下} (@emph{top-down}) 的定义，版本
二称作@emph{自底向上} (@emph{bottom-up}) 的定义，版本三称作@emph{推理规则}
(@emph{rules-of-inference}) 定义。

再来看几个运用这些的例子。

@; @def[ #:title
@; （整数列表，自顶向下）
@; ]
@; {
@;  Scheme列表是整数列表，当且仅当
@;
@;  @itemlist[#:style 'ordered
@;   @item{列表为空，或}
@;   @item{列表为序对，首项为整数，余项为整数列表。}
@; ]
@; }
@nested[#:style samepage]{
@env["sdef" #:opt (list (bracket "整数列表，自顶向下"))]{
  Scheme列表是整数列表，当且仅当：
 \begin{enumerate}
  \item 列表为空，或
  \item 列表为序对，首项为整数，余项为整数列表。
 \end{enumerate}
}
}

我们用@${Int}表示所有整数的集合，用@List-of-Int-$[]表示所有整数列表
的集合。

@; @def[ #:title
@; （整数列表，自底向上）
@; ]{

@; 集合@List-of-Int-$[]是满足如下两条性质的最小Scheme列表集合：

@; @itemlist[#:style 'ordered

@;  @item{@${@tt{()} \in @List-of-Int{}}，或}

@;  @item{若@${n \in Int}且@${l \in @List-of-Int{}}，则 @tt{(@${n} . @${l})
@;        @${\in} @List-of-Int{}}。}
@; ]
@; }
@env["sdef" #:opt (list (bracket "整数列表，自底向上"))]{
 集合@List-of-Int-m[]是满足如下两条性质的最小Scheme列表集合：

 \begin{enumerate}

  \item @m{\normalfont{@tt{()}} \in @List-of-Int-raw[]}，或

  \item 若 @m{n \in Int}且@m{l \in @List-of-Int-raw[]}，则
  @m{\normalfont{@tt{(@m{n} . @m{l})}} \in @List-of-Int-raw[]}。

 \end{enumerate}
}


这里，我们用中缀“@tt{.}”代表Scheme中 @racket[cons] 操作的结果。式子@tt{(@${n}
. @${l})}代表Scheme序对的首项为@${n}，余项为@${l}。

@; @def[ #:title
@; （整数列表，推理规则）
@; ]{

@; @$${\infer{() \in @List-of-Int{}}{}}

@; @$${\infer{(n . l) \in @List-of-Int{}}{n \in Int & l \in @List-of-Int{}}}

@; }
@env["sdef" #:opt (list (bracket "整数列表，推理规则"))]{

 @mp{\normalfont{@tt{()}} \in @List-of-Int-raw[]}

 @mp{\infer{\normalfont{@tt{(@m{n} . @m{l})}} \in @List-of-Int-raw[]}
           {n \in Int & l \in @List-of-Int-raw[]}}

}

这三个定义等价。来看看如何用它们生成一些@List-of-Int-$[]的元素。

@itemlist[#:style 'ordered

 @item{由定义1.1.4的性质1或定义1.1.5的规则1，@tt{()}是整数列表。}

 @item{由定义1.1.4的性质2，@tt{(14 . ())}是整数列表。因为@tt{14}是整数，@tt{()}
       是整数列表。写成@List-of-Int-$[]规则二的形式，就是

       @$${\infer{@tt{(14 . ())} \in @List-of-Int-$[]} {@tt{14} \in Int &
           @tt{()} \in @List-of-Int-$[]}} }

 @item{由定义1.1.4的性质2，@tt{(3 . (14 . ()))}是整数列表。因为 @tt{3} 是整数，
       @tt{(14 . ())}是整数列表。仍写成@List-of-Int-$[]规则二的形式，是

       @$${\infer{@tt{(3 . (14 . ()))} \in @List-of-Int-$[]} {@tt{3} \in Int &
           @tt{(14 . ())} \in @List-of-Int-$[]}} }

 @item{由定义1.1.4的性质2，@tt{(-7 . (3 . (14 . ())))}是整数列表。因为 @tt{-7}
       是整数，@tt{(3 . (14 . ()))}是整数列表。再次写成@List-of-Int-$[]规则二的
       形式，是

       @$${\infer{@tt{(-7 . (3 . (14 . ())))} \in @List-of-Int-$[]} {@tt{-7} \in
           Int & @tt{(3 . (14 . ()))}\in @List-of-Int-$[]}} }

 @item{不按照这种方式得到的都不是整数列表。}

]

改点缀表示法为列表表示法，可知 @tt{()}、 @tt{(14)}、 @tt{(3 14)} 以及 @tt{(-7 3
14)} 都是@List-of-Int-$[]的元素。

还可以结合各条规则来证明@${@tt{(-7 . (3 . (14 . ())))} \in @List-of-Int-$[]}，以见
出整个推理过程。下面的树状图叫做@emph{推导} (@emph{derivation}) 或@emph{推理树}
(@emph{deduction tree})。

@$${\infer{@tt{(-7 . (3 . (14 . ())))} \in @List-of-Int-$[]}
          {@tt{-7} \in Int &
           \infer{@tt{(3 . (14 . ()))} \in @List-of-Int-$[]}
                 {@tt{3} \in Int & \infer{@tt{(14 . ())} \in @List-of-Int-$[]}
                                       {@tt{14} \in Int & @tt{()} \in @List-of-Int-$[]}}
          }}

@exercise[#:level 1 #:tag "ex1.1"]{

 写出下列集合的归纳定义。以三种方式（自顶向下，自底向上，推理规则）写出每个定
 义，并用你的规则推导出各集合的一些元素。

  @itemlist[#:style 'ordered

   @item{@m{\{ 3n + 2 \mid n \in N \}}}

   @item{@m{\{ 2n + 3m + 1 \mid n, m \in N \}}}

   @item{@m{\{ (n, 2n + 1) \mid n \in N \}}}

   @item{@m{\{ (n, n^2) \mid n \in N \}}。不要在你的规则中使用平方。提示：想一想
         方程@m{ (n + 1) ^ 2 = n ^ 2 + 2n + 1}。}

  ]
 }

@exercise[#:level 2 #:tag "ex1.2"]{

 下面的几对规则分别定义了什么集合？给出解释。

 @itemlist[#:style 'ordered

  @item{@m{(0, 1) \in S \qquad \infer{(n + 1, k + 7) \in S}{(n, k) \in S}}}

  @item{@m{(0, 1) \in S \qquad \infer{(n + 1, 2k) \in S}{(n, k) \in S}}}

  @item{@m{(0, 0, 1) \in S \qquad \infer{(n + 1, j, i + j) \in S}{(n, i, j) \in S}}}

  @item{@m{\textnormal{\lbrack}\mathord{\star}\mathord{\star}\mathord{\star}\textnormal{\rbrack}} @m{\quad}
  @m{(0, 1, 0) \in S \qquad \infer{(n + 1, i + 2, i + j) \in S}{(n, i, j) \in S}}}

 ]

}

@exercise[#:level 1 #:tag "ex1.3"]{

 找出自然数的子集 @m{T}，满足 @m{0 \in T}，且对任何 @m{n \in T}，都有 @m{n + 3
 \in T}，但 @m{T \neq S}，@m{S} 是由定义 1.1.2 给出的集合。

}

@subsection[#:tag "s1.1.2"]{语法定义法}

前述例子较为直观，但是不难想象，描述更复杂的数据类型会有多麻烦。为了方便，我们展
示如何用@emph{语法} (@emph{grammar}) 定义集合。语法通常用来指定字符串的集合，但
也能用来定义值的集合。

例如，集合 @List-of-Int-$[]可用语法定义为：

@; @grammar : (grammar production ...)
@; @production : (production name expression #:code code-item)
@; @code-item : elem
@; @grammar{

@envalign*{@List-of-Int-raw[] &::= @tt{()} \\
@List-of-Int-raw[] &::= @tt{(@m{Int} . @List-of-Int-m[])}
}

@; }

这两条规则对应上述定义 1.1.4 中的两条属性。规则一是说空表属于@List-of-Int-$[]；规
则二是说，若 @${n} 属于 @${Int} 且 @${l} 属于 @List-of-Int-$[]，则@tt{(@${n}
. @${l})} 属于 @List-of-Int-$[]。这些规则叫做@emph{语法}。

来看看该定义的各个部分，其中有：

@itemlist[

  @item{@bold{非终止符}。这些是所定义的集合名。本例中只定义了一个集合，但是通常，
        可能会定义数个集合。这些集合有时称为@emph{句法类别} (@emph{syntactic
        category})。

        依照惯例，我们将非终止符和集合名的首字母大写，在文中提及它们的元素时，则
        用小写。这要比听起来容易。例如， @${Expression} 是非终止符，但我们写作
        @${e \in Expression} 或 “@${e} 是一个 expression”。

        另一常见做法，名叫@emph{巴科斯-诺尔范式} (@emph{Backus-Naur Form}) 或
        @emph{BNF}，是在词周围加尖括号，如 @${\langle}expression@${\rangle}。}

  @item{@bold{终止符}。这些是集合外在表示中的字符，在本例中，是“@tt{.}”，
        “@tt{(}”和“@tt{)}”。这些常用打字机字体写出，如 @tt{lambda}。}

  @item{@bold{生成式}。规则叫做@emph{生成式} (@emph{production})。每个生成式的左
        边是一个非终止符，右边包含终止符和非终止符。左右两边通常用符号 @${::=}
        分隔，读作@emph{是}或@emph{可能是}。式子右边用其他句法类别和@emph{终止
        符}（如左括号、右括号和句点）指定一种方法，用以构建当前句法类别的元素。}

]

如果某些句法类别的含义在上下文中足够清晰，在生成式中提到它们时通常不作定义，如
@${Int}。

语法常常简写。当一个生成式的左边与前一生成式相同时，一般会略去。根据这一惯例，我
们的语法可以写作

@envalign*{
@List-of-Int-raw[] &::= @tt{()} \\
                          &::= @tt{(@m{Int} . @List-of-Int-m[])}
}

给同一句法类别编写一组规则时，也可以只写一次@${::=}和左边内容，随后的各个右边内
容用特殊符号“@${\mid}”（竖线，读作@emph{或}）分隔。 用“@${\mid}”，
@List-of-Int-$[]的语法可写成：


@$${@List-of-Int-$[] ::= @tt{()} @$${\mid} @tt{(@${Int} . @List-of-Int-$[])}}

另一种简写是@emph{克莱尼星号} (@emph{Kleene Star})，写作 @${\{...\}^*}。当它出现
在右边时，表示一个序列，由任意多个花括号之间的内容组成。用克莱尼星号，
@List-of-Int-$[] 的定义可以简写为

@$${@List-of-Int-$[] ::= @tt{(@${\{Int\}^*})}}

这也包含没有任何内容的情况。如果内容出现 0 次，得到的是空字符串。

星号的变体是@emph{克莱尼加号} (@emph{Kleene Plus}) @${\{...\}^+}，表示一个或多个
内容的序列。把上例中的@${^*}换成@${^+}，定义的句法类别是非空整数列表。

星号的另一变体是@emph{分隔表} (@emph{separated list})。例如，@${Int^{*(c)}} 表示
一个序列，包含任意数量的非终止符@${Int}元素，以非空字符序列 @${c} 分隔。这也包含
没有元素的情况。如果有 0 个元素，得到的是空字符串。例如，@${Int^{*(,)}} 包含字符
串

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
8
14, 12
7, 3, 14, 16
}|
}

@${Int^{*(;)}} 包含字符串

@nested[#:style 'code-inset]{
@verbatim|{
8
14; 12
7; 3; 14; 16
}|
}
}

这些简写不是必需的，总能够不用它们重写语法。

对由语法定义的集合，可以用@emph{句法推导} (@emph{syntactic derivation})证明给定
值是其元素。这样的推导从集合对应的非终止符开始，在由箭头@${\Rightarrow} 指示的每
一步中，如果非终止符对应的句法类别未做定义，则将其代换为该类别的已知元素，否则代
换为对应规则右边的内容。例如，前述证明 “@tt{(14 . ())}是整数列表”，可以用句法
推导化为

@envalign*{
 @List-of-Int-raw[] &\Rightarrow @tt{(@m{Int} . @List-of-Int-m[])} \\
                           &\Rightarrow @tt{(14 . @List-of-Int-m[])} \\
                           &\Rightarrow @tt{(14 . ())}}

非终止符的替换顺序无关紧要，所以@tt{(14 . ())}的推导也可以写成：

@envalign*{
 @List-of-Int-raw[] &\Rightarrow @tt{(@m{Int} . @List-of-Int-m[])} \\
                           &\Rightarrow @tt{(@m{Int} . ())} \\
                           &\Rightarrow @tt{(14 . ())}
}

@exercise[#:level 1 #:tag "ex1.4"]{

 写出从 @List-of-Int-m[] 到 @tt{(-7 . (3 . (14 ())))} 的推导。
 @linebreak[]

}

再来看一些有用集合的定义。

@itemlist[#:style 'ordered

 @item{许多符号操作过程用于处理只包含符号和具有类似限制的列表。我们把这些叫做
 @tt{s-list}，定义如下：

 @; @def[ #:title
 @; （s-list，s-exp）
 @; ]
 @; {
 @; }

 @env["sdef" #:opt (list (bracket "s-list, s-exp"))]{

  @envalign*{S\mbox{-}list &::= \normalfont{@tt{(@m{\{S\mbox{-}exp\}^*})}} \\
             S\mbox{-}list &::= Symbol \mid S\mbox{-}list}
 }

 s-list 是 s-exp 的列表，s-exp 或者是 s-list，或者是一个符号。这里是一些 s-list。

 @verbatim[#:indent 2]{
 (a b c)
 (an (((s-list)) (with () lots) ((of) nesting)))
 }

 有时也使用更宽松的 s-list 定义，既允许整数，也允许符号。

 }

 @item{使用三元素列表表示内部节点，则以数值为叶子，以符号标示内部节点的二叉树可
 用语法表示为：

 @; @def[ #:title
 @; （二叉树）
 @; ]
 @; {
 @; @$${Bintree ::= Int \mid @tt{(@${Symbol} @${Bintree} @${Bintree})}}
 @; }

 @env["sdef" #:opt (list (bracket "二叉树"))]{

   @mp{Bintree ::= Int \mid \normalfont{@tt{(@m{Symbol} @m{Bintree} @m{Bintree})}}}

 }

 这是此类树的几个例子：

 @verbatim[#:indent 2]{
 1
 2
 (foo 1 2)
 (bar 1 (foo 1 2))
 (baz
  (bar 1 (foo 1 2))
  (biz 4 5))}
 }

 @item{@emph{lambda 演算} (@emph{lambda calculus}) 是一种简单语言，常用于研究编
 程语言理论。这一语言只包含变量引用，单参数过程，以及过程调用，可用语法定义为：

 @; @def[ #:title
 @; （lambda 演算）
 @; ]
 @; {
 @; @envalign*{LcExp &::= @m{Identifier} \\
 @;                  &::= @tt{(lambda (@m{Identifier}) @m{LcExp})} \\
 @;                  &::= @tt{(@m{LcExp} @m{LcExp})}}

 @; 其中，identifier 是除 @tt{lambda} 之外的任何符号。
 @; }

 @env["sdef" #:opt (list (bracket "lambda 演算"))]{

  @envalign*{LcExp &::= @m{Identifier} \\
                   &::= \normalfont{@tt{(lambda (@m{Identifier}) @m{LcExp})}} \\
                   &::= \normalfont{@tt{(@m{LcExp} @m{LcExp})}}}

  其中，identifier 是除 {\normalfont{@tt{lambda}}} 之外的任何符号。

 }

 第二个生成式中的 identifier 是 @tt{lambda} 表达式主体内的变量名。这一变量叫做表
 达式的@emph{绑定变量} (@emph{bound variable})，因为它绑定（或称捕获）主体内出现
 的任何同名变量。出现在主体内的同名变量都指代这一个。

 要明白这怎么用，考虑推广到算术操作符的 lambda 演算。在这种语言里，

 @codeblock{(lambda (x) (+ x 5))}

 是一表达式，@tt{x} 是其绑定变量。这式子表示一个过程，把它的参数加5。因此，在

 @codeblock{((lambda (x) (+ x 5)) (- x 7))}

 中，最后一个出现的 @tt{x} 不是指 @tt{lambda} 表达式中绑定的 @tt{x}。
 @secref{o-f}节中介绍了 @tt{occurs-free?}，到时我们再讨论这个问题。

 该语法定义 @${LcExp} 的元素为 Scheme 值，因此很容易写出程序来处理它们。

 }

]

这些语法叫做@emph{上下文无关} (@emph{context-free}) 语法，因为一条规则定义的句法
类别可以在涉及它的任何语境中使用。有时这不够严格。考虑二叉搜索树。其节点或者为空，
或者包含一个整数、两棵子树

@$${Binary\mbox{-}search\mbox{-}tree ::= @tt{()} \mid
    @tt{(@${Int} @${Binary\mbox{-}search\mbox{-}tree} @${Binary\mbox{-}search\mbox{-}tree})}}

这如实反映了每个节点的结构，但是忽略了二叉搜索树的一个要点：所有左子树的键值都小
于（或等于）当前节点，所有右子树的键值都大于当前节点。

因为这条额外限制，从 @${Binary\mbox{-}search\mbox{-}tree} 得出的句法推导并不都是
正确的二叉搜索树。要判定某个生成式能否用于特定的句法推导，必须检查生成式用在哪种
语境。这种限制叫做@emph{上下文敏感限制} (@emph{context-sensitive constraints})，
或称@emph{不变式} (@emph{invariants})。

定义编程语言的语法也会产生上下文敏感限制。例如，在许多编程语言中变量必须在使用之
前声明。对变量使用的这一限制就对其上下文敏感。虽然可以用形式化方法定义上下文敏感
限制，但这些方法远比本章考虑的复杂。实际中，常用的方法是先定义上下文无关语法，随
后再用其他方法添加上下文敏感限制。@secref{types}展示了这种技巧的一个例子。

@subsection[#:tag "induct"]{归纳证明法}

用归纳法描述的集合，其定义有两种用法：证明关于集合元素的定理，写出操作集合元素的
程序。这里给出一个此类证明的例子，写程序留作下节的主题。

@; @theorem
@; {
@; 令 t 为二叉树，形如定义 1.1.7，则 t 包含奇数个节点。
@; }

@env["sthm"]{

 令 t 为二叉树，形如定义 1.1.7，则 t 中包含奇数个节点。

}

@; @proof
@; @{
@parprf{

 用归纳法证明 @${t} 的大小。令 @${t} 的大小等于 @${t} 中节点的个数。归纳假设为
 @${\mathit{IH}(k)}：树的大小@${\leq k}时有奇数个节点。依照归纳法的惯例：先证明
 @${\mathit{IH}(0)}为真，然后证明若对任一整数 @${k}，@${\mathit{IH}} 为真，则对
 @${k + 1}，@${\mathit{IH}} 也为真。

 @itemlist[#:style 'ordered

  @item{没有哪棵树只有 @${0} 个节点，所以 @${\mathit{IH}(0)} 显然成立。}

  @item{设 @${k} 为整数时，@${\mathit{IH}(k)} 成立，即，任何树的节点数 @${\leq
  k} 时，其准确数目为奇数。需证明 @${\mathit{IH}(k + 1)} 也成立：任何树的节点数
  @${\leq k + 1} 时，节点数为奇数。若 @${t} 有 @${\leq k + 1} 个节点，根据二叉树
  的定义，只有两种可能：

  @itemlist[#:style 'ordered

   @item{@${t} 形如 @${n}，@${n} 为整数。此时 @${t} 只有一个节点，1为奇数。}

   @item{@${t} 形如 @${@tt{(@${sym} @${t_1} @${t_2})}}，其中，@${sym} 是一符号，
   @${t_1} 和 @${t_2} 是树。此时 @${t_1} 和 @${t_2} 节点数少于 @${t}。因为 @${t}
   有 @${\leq k + 1}个节点，@${t_1} 和 @${t_2} 一定有 @${\leq k} 个节点。因此它
   们符合 @${\mathit{IH}(k)}，一定各有奇数个节点，不妨分别设为 @${2n_1 + 1} 和
   @${2n_2 + 1}。则算上两棵子树和根，原树中的节点总数为

   @$${(2n_1 + 1) + (2n_2 + 1) + 1 = 2(n_1 + n_2 + 1) + 1}

   也是一个奇数。}]}]

 陈述“@${\mathit{IH}(k + 1)} 成立”证毕，归纳完成。
 }
@; @}

证明的关键是树 @${t} 的子结构总是比 @${t} 自身小。这种证明模式叫做@emph{结构化归
纳法} (@emph{structural induction})。

@nested[#:style tip]{
 @centered{@bold{结构化归纳证明}}

 @para[#:style tip-content]{欲证明假设 @${\mathit{IH}(s)} 对所有结构 @${s} 为真，
 需证明：}

 @itemlist[#:style 'ordered
   @item{@${\mathit{IH}} 对简单结构（没有子结构）为真。}

   @item{若 @${\mathit{IH}} 对 @${s} 的子结构为真，则对 @${s} 本身也为真。}
 ]
}

@exercise[#:level 2 #:tag "ex1.5"]{
 证明若 @m{e \in LcExp}，则 @m{e} 中的左右括号数量相等。

}

@section[#:tag "drp"]{推导递归程序}

我们已经用归纳定义法描述了复杂集合。我们能够分析归纳式集合的元素，观察如何从较小
元素构建集合。我们用这一想法写出了过程 @tt{in-S?}，用以判断自然数是否属于集合
@${S}。现在，我们用同样的想法定义更通用的过程，以便对归纳式集合做运算。

递归过程依赖于一条重要原则：

@nested[#:style tip]{
 @centered{@bold{较小子问题原则}}

 @para[#:style tip-content]{若能化问题为较小子问题，则能调用解决原问题的过程解决
 子问题。}}

已求得的子问题解随后可用来求解原问题。这可行，因为每次过程调用都是针对较小的子问
题，直至最终调用，针对一个可以直接求解的问题，不需再次调用自身。

我们用一些例子解释这一想法。

@subsection[#:tag "l-l"]{@tt{list-length}}

标准的 Scheme 程序 @tt{length} 求出列表中的元素个数。

@examples[#:label #f (length '(a b c))
                     (length '((x) ()))]

我们来写出自己的过程 @tt{list-length}，做同样的事。

先来写出过程的@emph{合约}。合约指定了过程可取参数和可能返回值的集合。合约也可以
包含过程的期望用法或行为。这有助于我们在编写时及以后追查我们的意图。在代码中，这
是一条注释，我们用打字机字体示之，以便阅读。

@racketblock[
@#,elem{@bold{@tt{list-length}} : @${List \to Int}}
@#,elem{@bold{用法} : @tt{(list-length @${l}) = @${l}@emph{的长度}}}
(define list-length
  (lambda (lst)
    ...))
]

列表的集合定义为

@$${List ::= @tt{()} \mid @tt{(@${Scheme \ value} . @${List})}}

因此，考虑列表的每种情况。若列表为空，则长度为0。

@nested[#:style samepage]{
@racketblock[
@#,elem{@bold{@tt{list-length}} : @${List \to Int}}
@#,elem{@bold{用法} : @tt{(list-length @${l}) = @${l} 的长度}}
(define list-length
  (lambda (lst)
@; diff{
    (if (null? lst)
        0
@; }
        ...)))
]
}

若列表非空，则其长度比其余项长度多1。这就给除了完整定义。

@racketblock[
@#,elem{@bold{@tt{list-length}} : @${List \to Int}}
@#,elem{@bold{用法} : @tt{(list-length @${l}) = @${l} 的长度}}
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        @; diff{
        (+ 1 (list-length (cdr lst))))))
        @; }
]

通过 @tt{list-length} 的定义，我们可以看到它的运算过程。

@nested[#:style 'code-inset]{
@verbatim|{
  (list-length '(a (b c) d))
= (+ 1 (list-length '((b c) d)))
= (+ 1 (+ 1 (list-length '(d))))
= (+ 1 (+ 1 (+ 1 (list-length '()))))
= (+ 1 (+ 1 (+ 1 0)))
= 3
}|
}

@subsection[#:tag "n-e"]{@tt{nth-element}}

标准的 Scheme 过程 @tt{list-ref} 取一列表 @tt{lst} 和从 0 开始计数的索引 @tt{n}，
返回 @tt{lst} 的第 @tt{n} 个元素。

@examples[#:label #f (list-ref '(a b c) 1)]

我们来写出自己的过程 @tt{nth-element}，做同样的事。

仍沿用上述 @${List} 的定义。

当 @${lst} 为空时，@tt{(nth-element @${lst} @${n})} 应当返回什么？这种情况下，
@tt{(nth-element @${lst} @${n})} 想要取空列表的元素，所以报错。

当 @${lst} 非空时，@tt{(nth-element @${lst} @${n})} 应当返回什么？答案取决于
@${n}。若 @${n = 0}，答案是 @${lst} 的首项。

当 @${lst} 非空，且 @${n \neq 0} 时，@tt{(nth-element @${lst} @${n})} 应当返回什
么？这种情况下，答案是 @${lst} 余项的第 @${(n - 1)} 个元素。由 @${n \in N} 且
@${n \neq 0}，可知 @${n - 1} 一定属于 @${N}，因此可以递归调用 @tt{nth-element}找
出第 @${(n - 1)} 个元素。

这就得出定义

@racketblock[
@#,elem{@bold{@tt{nth-element}} : @${List \times Int \to SchemeVal}}
@#,elem{@bold{用法} : @tt{(nth-element @${lst} @${n}) = @${lst} 的第 @${n} 个元素}}
(define nth-element
  (lambda (lst)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (eopl:error 'nth-element
                "列表太短，没有第~s个元素.~%" (+ n 1))))
]

这里的注释 @tt{@bold{@tt{nth-element}} : @${List \times Int \to SchemeVal}} 表示
@bold{@tt{nth-element}}是一个过程，取两个参数，一个为列表，一个为整数，返回一个
Scheme 值。这与数学中的表示 @${f : A \times B \to C} 相同。

过程 @tt{report-list-too-short} 调用 @tt{eopl:} @tt{error} 来报告错误。过程
@tt{eopl:error} 会终止计算。它的首个参数是一符号，用于在错误信息中指示调用
@tt{eopl:error} 的过程。第二个参数是一个字符串，会打印为错误信息。对应于字符串中
的每个字符序列 @tt{~s} ，都必须有一个额外参数。打印字符串时，这些参数的值会替换
对应的 @tt{~s} 。@tt{~%} 代表换行。错误信息打印后，计算终止。过程@tt{eopl:error}
并非标准 Scheme 的一部分，但大多数 Scheme 实现提供这样的组件。在本书中，我们以类
似方式，用名字含 @tt{report-} 的过程报告错误。

来看看 @tt{nth-element} 如何算出答案：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
  (nth-element '(a b c d e) 3)
= (nth-element   '(b c d e) 2)
= (nth-element     '(c d e) 1)
= (nth-element       '(d e) 0)
= d
}|
}

这里，@tt{nth-element} 递归处理越来越短的列表和越来越小的数字。
}

如果排除错误检查，我们得靠 @tt{car} 和 @tt{cdr} 的抱怨来获知传递了空列表，但它们
的错误信息无甚帮助。例如，当我们收到 @tt{car} 的错误信息，可能得找遍整个程序中使
用 @tt{car} 的地方。

@exercise[#:level 1 #:tag "ex1.6"]{
 如果翻转 @tt{nth-element} 中两个条件的顺序，会有什么问题？

}

@exercise[#:level 2 #:tag "ex1.7"]{
 @tt{nth-element} 的错误信息不够详尽。重写 @tt{nth-element}，给出更详细的错误信
 息，像是 “@tt{(a b c)} 不足 8 个元素”。

}

@subsection[#:tag "r-f"]{@tt{remove-first}}

过程 @tt{remove-first} 取两个参数：符号 @${s} 和符号列表 @${los}。它返回一个列表，
除了不含第一个出现在 @${los} 中的符号 @${s} 外，所含元素及其排列顺序与 @${los}
相同。如果 @${s} 没有出现在 @${los} 中，则返回 @${los}。

@(define remove-first-eval
(parameterize ([sandbox-output 'string]
               [sandbox-error-output 'string]
               [sandbox-memory-limit 50])
  (make-evaluator
   'racket/base
   '(define (remove-first s los)
      (if (null? los)
          los
          (if (eq? s (car los))
              (cdr los)
              (cons (car los)
                    (remove-first s (cdr los)))))))))

@examples[#:eval remove-first-eval
          #:label #f
          (remove-first 'a '(a b c))
          (remove-first 'b '(e f g))
          (remove-first 'a4 '(c1 a4 c1 a4))
          (remove-first 'x '())]

写出此过程之前，我们先要定义符号列表集合 @${List\mbox{-}of\mbox{-}Symbol} ，以便
给出问题的完整描述。不像上一节介绍的 s-lists，符号列表不包含子列表。

@$${List\mbox{-}of\mbox{-}Symbol ::= @tt{()} \mid @tt{(@${Symbol} . @${List\mbox{-}of\mbox{-}Symbol})}}

符号列表或者是空列表，或者首项为符号，余项为符号列表。

如果列表为空，不需要移除 @${s}，则答案为空列表。

@racketblock[
@#,elem{@bold{@tt{remove-first}} : @${Sym \times Listof(Sym) \to Listof(Sym)}}
@#,elem{@bold{用法} : @tt{(remove-first @${s} @${los}) 返回一列表，除了不含第一个出现在 @${los} 中的符号 @${s} 外，元素及其排列顺序与 @${los} 相同。}}
(define remove-first
  (lambda (lst)
    (if (null? lst)
        '()
        ...)))
]

写合约时，我们用 @${Listof(Sym)} 而不是 @${List\mbox{-}of\mbox{-}Symbol}。用这种
写法可以免除许多上面那样的定义。

如果 @${los} 非空，有没有哪种情况可以立刻得出答案？如果 @${los} 的第一个元素是
@${s}，比如 @${los = @tt{(@${s} @${s_1} @${...} @${s_{n-1}})}}，@${s} 首次出现时是
@${los} 的第一个元素，那么把它删除之后的结果是 @tt{(@${s_1} @${...} @${s_{n-1}})}。

@racketblock[
@#,elem{@bold{@tt{remove-first}} : @${Sym \times Listof(Sym) \to Listof(Sym)}}
(define remove-first
  (lambda (lst)
    (if (null? lst)
        '()
        @; diff {
        (if (eqv? (car los) s)
            (cdr los)
            ...
            ))))
        @; }
]

如果 @${los} 的第一个元素不是 @${s}，比如 @${los = @tt{(@${s_0} @${s_1} @${...}
@${s_{n-1}})}}，可知 @${s_0} 不是第首个出现的 @${s}，因此答案中的第一个元素一定
是@${s_0}，即表达式 @tt{(car los)} 的值。而且，@${los} 中的首个 @${s} 一定在
@tt{(@${s_1} @${...} @${s_{n-1}})} 中。所以答案的余下部分一定是移除 @${los} 余项
中首个 @${s} 的结果。因为 @${los} 的余项比 @${los} 短，我们可以递归调用
@tt{remove-first}，从 @${los} 的余项中移除 @${s}，即答案的余项可用
@tt{(remove-first s (cdr los))} 求得。已知如何找出答案的首项和余项，可以用
@tt{cons} 结合二者，通过表达式 @tt{(cons (car los) (remove-first s (cdr los)))}
求得整个答案。由此，@tt{remove-first} 的完整定义为

@nested[#:style samepage]{
@racketblock[
@#,elem{@bold{@tt{remove-first}} : @${Sym \times Listof(Sym) \to Listof(Sym)}}
(define remove-first
  (lambda (lst)
    (if (null? lst)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            @; diff {
            (cons (car los) (remove-first s (cdr los)))))))
            @; }
]
}

@exercise[#:level 1 #:tag "ex1.8"]{

 如果把 @tt{remove-first} 定义中的最后一行改为 @tt{(remove-first s (cdr los))}，
 得到的过程做什么运算？对修改后的版本，给出合约，包括使用说明。

}

@exercise[#:level 2 #:tag "ex1.9"]{

 定义 @tt{remove}。它类似于 @tt{remove-first}，但会从符号列表中移除出现的所有给
 定符号，而不只是第一个。

}

@subsection[#:tag "o-f"]{@tt{occurs-free?}}

过程 @tt{occurs-free?} 取一个变量 @${var}，由 Scheme 符号代表；一个 lambda 演算
表达式 @${exp}，形如定义 1.1.8；判断 @${var} 是否自由出现于 @${exp}。如果一个变
量出现于表达式 @${exp} 中，但不在某一 @tt{lambda} 绑定之内，我们说该变量@emph{自
由出现} (@emph{occurs free}) 于表达式 @${exp} 中。例如，

@(define occurs-free?-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
     '(define (occurs-free? var exp)
        (cond ((symbol? exp)
               (eqv? var exp))
              ((eqv? (car exp) 'lambda)
               (and (not (eqv? var (car (cadr exp))))
                    (occurs-free? var (caddr exp))))
              (else
               (or (occurs-free? var (car exp))
                   (occurs-free? var (cadr exp)))))))))

@examples[#:eval occurs-free?-eval
          #:label #f
          (occurs-free? 'x 'x)
          (occurs-free? 'x 'y)
          (occurs-free? 'x '(lambda (x) (x y)))
          (occurs-free? 'x '(lambda (y) (x y)))
          (occurs-free? 'x '((lambda (x) x) (x y)))
          (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))]

遵照 lambda 演算表达式的语法，我们可以解决此问题：

@envalign*{LcExp &::= @m{Identifier} \\
                 &::= \normalfont{@tt{(lambda (@m{Identifier}) @m{LcExp})}} \\
                 &::= \normalfont{@tt{(@m{LcExp} @m{LcExp})}}}

我们可以总结出规则的各种情况：

@itemlist[

 @item{若表达式 @${e} 是一变量，则当且仅当 @${x} 与 @${e} 相同时，变量 @${x} 自
 由出现于 @${e}。}

 @item{若表达式 @${e} 形如 @tt{(@${lambda} (@${y}) @${e'})}，则当且仅当 @${y} 不
 同于 @${x} 且 @${x} 自由出现于 @${e'} 时，变量 @${x} 自由出现于 @${e}。}

 @item{若表达式 @${e} 形如 @tt{(@${e_1} @${e_2})}，则当且仅当 @${x} 自由出现于
 @${e_1} 或 @${e_2} 时，@${x} 自由出现于 @${e}。这里的“或”表示@emph{涵盖或}
 (@emph{inclusive or})，意为它包含 @${x} 同时自由出现于 @${e_1} 和 @${e_2} 的情
 况。我们通常用“或”表示这种意思。}

]

你可以说服自己，这些规则涵盖了“@${x} 不在某一 lambda 绑定之中”表示的所有意思。

@exercise[#:level 1 #:tag "ex1.10"]{

 我们常用“或”表示“涵盖或”。“或”还有什么含义？@linebreak[]

}

然后，定义 @tt{occurs-free?} 就很容易了。因为有三种情况要检查，我们不用 Scheme
的 @tt{if}，而是用 @tt{cond}。在 Scheme 中，若 @${exp_1} 或 @${exp_2} 返回真值，
则@tt{(or @${exp_1} @${exp_2})} 返回真值。

@racketblock[
@#,elem{@bold{@tt{occurs-free?}} : @${Sym \times LcExp \to Bool}}
@#,elem{@bold{用法} : 若符号 @${var} 自由出现于 @${exp}，返回 @tt{#t}，否则返回 @tt{#f}}
(define (occurs-free? var exp)
  (cond
    ((symbol? exp) (eqv? var exp))
    ((eqv? (car exp) 'lambda)
     (and
      (not (eqv? var (car (cadr exp))))
      (occurs-free? var (caddr exp))))
    (else
     (or
      (occurs-free? var (car exp))
      (occurs-free? var (cadr exp))))))
]

这一过程略显晦涩。比如，很难弄明白 @tt{(car (cadr exp))} 指代 @tt{lambda} 表达式
中的变量声明，或者 @tt{(caddr exp)} 指代 @tt{lambda} 表达式的主体。在 2.5 节，我
们展示如何显著改善这种情况。

@subsection[#:tag "s1.2.5"]{@tt{subst}}

过程 @tt{subst} 取三个参数：两个符号 @tt{new} 和 @tt{old}，一个 s-list，
@tt{slist}。它检查 @tt{slist} 的所有元素，返回类似 @tt{slist} 的新列表，但把其中
所有的 @tt{old} 替换为 @tt{new}。

@(define subst-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
     '(define (subst new old slist)
        (if (null? slist)
            '()
            (cons
              (subst-in-s-exp new old (car slist))
              (subst new old (cdr slist)))))
     '(define (subst-in-s-exp new old sexp)
        (if (symbol? sexp)
            (if (eqv? sexp old) new sexp)
            (subst new old sexp))))))

@examples[#:eval subst-eval
          #:label #f
          (subst 'a 'b '((b c) (b () d)))]

因为 @tt{subst} 定义于 s-list 上，它的结构应当反映 s-list 的定义（定义 1.1.6）：

@envalign*{S\mbox{-}list &::= \normalfont{@tt{(@m{\{S\mbox{-}exp\}^*})}} \\
           S\mbox{-}list &::= Symbol \mid S\mbox{-}list}

克莱尼星号准确描述了集合 s-list，但对写程序没什么用。因此我们的第一步是抛开克莱
尼星号重写语法。得出的语法表明，我们的过程应当该递归处理 s-list 的首项和余项。

@envalign*{S\mbox{-}list &::= {\normalfont@tt{()}} \\
                         &::= {\normalfont{@tt{(@m{S\mbox{-}exp} . @m{S\mbox{-}list})}}} \\
           S\mbox{-}exp &::= Symbol \mid S\mbox{-}list}

这一例子比之前的复杂，因为它的语法输入包含两个非终止符，@${S\mbox{-}list} 和
@${S\mbox{-}exp}。因此，我们需要两个过程，一个处理 @${S\mbox{-}list}，另一个处理
@${S\mbox{-}exp}。

@racketblock[
@#,elem{@bold{@tt{subst}} : @m{Sym \times Sym \times S\mbox{-}list \to S\mbox{-}list}}
(define subst
  (lambda (new old slist)
    ...))

@#,elem{@bold{@tt{subst-in-s-exp}} : @m{Sym \times Sym \times S\mbox{-}exp \to S\mbox{-}exp}}
(define subst-in-s-exp
  (lambda (new old sexp)
    ...))
]

我们首先处理 @tt{subst}。如果列表为空，不需要替换 @tt{old}。

@racketblock[
@#,elem{@bold{@tt{subst}} : @m{Sym \times Sym \times S\mbox{-}list \to S\mbox{-}list}}
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        ...)))
]

如果 @tt{slist} 非空，它的首项是一个 @${S\mbox{-}exp}，余项是另一 s-list。这时，
答案应当是一个列表，它的首项是把 @tt{slist} 首项中的 @tt{old} 替换为 @tt{new} 的
结果，它的余项是把 @tt{slist} 余项中的 @tt{old} 替换为 @tt{new} 的结果。因为
@tt{slist} 的首项是 @${S\mbox{-}exp} 的元素，我们用 @tt{subst-in-s-exp}解决这一
子问题。因为 @tt{slist} 的余项是 @${S\mbox{-}list} 的元素，我们递归调用
@tt{subst} 处理它。

@racketblock[
@#,elem{@bold{@tt{subst}} : @m{Sym \times Sym \times S\mbox{-}list \to S\mbox{-}list}}
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        @; diff {
        (cons
         (subst-in-s-exp new old (car slist))
         (subst new old (cdr slist))))))
        @; }
]

现在来处理 @tt{subst-in-s-exp}。由语法，可知符号表达式 @tt{sexp} 或者是符号，或
者是 s-list。如果它是符号，那么得检查它与符号 @tt{old} 是否相同。如果是，答案为
@tt{new}；否则，答案还是 @tt{sexp}。如果 @tt{sexp} 是一个 s-list，那么我们递归调
用 @tt{subst} 找出答案。

@racketblock[
@#,elem{@bold{@tt{subst-in-s-exp}} : @m{Sym \times Sym \times S\mbox{-}exp \to S\mbox{-}exp}}
(define subst-in-s-exp
  (lambda (new old sexp)
    @; diff {
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))
    @;}
]

因为我们严格依照 @${S\mbox{-}list} 和 @${S\mbox{-}exp} 的定义，这个递归一定会终
止。因为 @tt{subst} 和 @tt{subst-in-s-exp} 递归调用彼此，我们称之为@emph{互递归}
(@emph{mutually recursive})。

把 @tt{subst} 拆解为两个过程——每个处理一种句法类别——是个重要技巧。对更为复杂的程
序，我们得以每次考虑一个句法类别，从而化繁为简。

@exercise[#:level 1 #:tag "ex1.11"]{

 @tt{subst-in-s-exp} 的最后一行中，递归是针对 @tt{sexp} 而非更小的子结构，为什
 么一定能终止？

}

@exercise[#:level 1 #:tag "ex1.12"]{

 用 @tt{subst-in-s-exp} 的定义替换 @tt{subst} 中的调用，从而排除这次调用，然后
 简化得到的过程。结果中的 @tt{subst} 应当不需要 @tt{subst-in-s-exp}。这种技巧叫
 做@emph{内联} (@emph{inlining})，用于优化编译器。

}

@exercise[#:level 2 #:tag "ex1.13"]{

 在我们的例子中，我们从排除 @m{S\mbox{-}list} 语法内的克莱尼星号开始。依照原本的
 语法，用 @tt{map} 重写 @tt{subst}。@linebreak[]

}

现在，我们有了编写过程处理归纳数据集的窍门，来把它总结成一句口诀。@linebreak{}

@nested[#:style tip]{
 @centered{@bold{遵循语法！}}

 @para[#:style tip-content]{定义过程处理归纳式数据时，程序的结构应当反映数据的结
 构。}}

更准确地说：

@itemlist[

 @item{为语法中的每个非终止符编写一个过程。每一过程负责处理相应非终止符的数据，
 不做其他。}

 @item{在每个过程中，为相应非终止符的每一生成式写一分支。你可能需要额外的分支结
 构，但这样才能写得下去。对生成式右边出现的每个非终止符，递归调用相应的过程。}

]

@section[#:tag "apca"]{辅助过程和上下文参数}

窍门@emph{遵循语法}很有效，有时却还是不够。考虑过程 @tt{number-elements}。这一过
程取任何列表 @tt{(@${v_0} @${v_1} @${v_2} ...)} ，返回一列表 @tt{((0 @${v_0}) (1
@${v_1}) (2 @${v_2}) ...)}。

我们用过的那种直拆法不凑效，因为没有明显的方法能从 @tt{(number-elements (cdr
lst))} 得出 @tt{(number-elements lst)} （但是，看看练习 1.36）。

要解决这个问题，我们@emph{放宽} (@emph{generalize}) 问题。我们写一个过程
@tt{number-elements-from} ，它取一个额外参数 @${n}，指定起始编号。用递归处理列表，
这个过程很容易写。

@; @racketblock with contracts and usage
@racketblock[
@#,elem{@bold{@tt{number-elements-from}} : @m{Listof(SchemeVal) \times Int \to Listof(List(Int, SchemeVal))}}
@#,elem{@${\begin{alignedat}{-1}@bold{用法} : &@tt{(number-elements-from '(@${v_0} @${v_1} @${v_2} ...) n)} \\ &\hphantom{x}= @tt{((@${n} @${v_0}) (@${n + 1} @${v_1}) (@${n + 2} @${v_2}) ...)}\end{alignedat}}}
(define number-elements-from
  (lambda (lst n)
    (if (null? lst) '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1))))))
]
@;

合约的标题告诉我们这个过程取两个参数，一个列表（包含任意 Scheme 值）和一个整数，
返回一个列表，列表中的每个元素是包含两个元素的列表：一个整数，一个 Scheme 值。

一旦我们定义了 @tt{number-elements-from}，很容易写出所需的过程。

@racketblock[
@#,elem{@bold{@tt{number-elements}} : @m{Listof(SchemeVal) \to Listof(List(Int, SchemeVal))}}
(define number-elements
  (lambda (lst n)
    (number-elements-from lst 0)))
]

这里有两个要点。首先，过程 @tt{number-elements-from} 的定义独立于
@tt{number-elements}。程序员经常要写一些过程，只调用其他辅助过程，多传递一些常量
参数。除非我们理解辅助过程对参数的@emph{每个}值做什么，我们很难理解调用它的过程
做什么。这给了我们一条口诀：

@nested[#:style tip]{
 @centered{@bold{避免神秘小工具！}}

 @para[#:style tip-content]{定义辅助过程时，总是指明它对所有参数值做什么，而不只
 是初始值。}}

其次，@tt{number-elements-from} 的两个参数各有作用。第一个参数是我们要处理的列表，
随每一次递归调用而减小。而第二个参数，则是对我们当前任务@emph{上下文}
(@emph{context}) 的抽象。在本例中，当调用 @tt{number-elements} 时，我们最终调用
@tt{number-elements-from} 处理原列表的每个子列表。第二个参数告知我们子列表在原列
表中的位置。随递归调用，它不减反增，因为我们每次经过原列表的一个元素。有时我们称
之为@emph{上下文参数} (@emph{context argument})，或者@emph{继承属性}
(@emph{inherited attribute})。

另一个例子是向量求和。

要求列表中各项的和，我们可以遵循语法，递归处理列表的余项。那么我们的过程看起来像
是：

@; racketblock with contracts
@racketblock[
@#,elem{@bold{@tt{list-sum}} : @m{Listof(Int) \to Int}}
(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))
]

但是无法按照这种方式处理向量，因为它们不能够很方便地拆解。

因为我们无法拆解向量，我们放宽问题，为向量某一部分求和。问题定义为，计算

@$${\sum_{i=0}^{i=length(v)-1} v_i}

其中，@${v} 是整数向量。通过把上界改为一个参数 @${n}，我们放宽了原问题，所以新的
任务是计算

@$${\sum_{i=0}^{i=n} v_i}

其中，@${0 \leq n \leq length(v)}。

按照定义，用归纳法处理第二个参数 @${n}，可以直接写出此过程。

@racketblock[
@#,elem{@bold{@tt{partial-vector-sum}} : @${Vectorof(Int) \times Int \to Int}}
@#,elem{@bold{用法} : 若 @${0 \leq n < length(v)}，则 @mp{@tt{(partial-vector-sum @m{v} @m{n}) = @m{\sum_{i=0}^{i=n} v_i}}}}
(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1))))))
]

由于 @${n} 一定会递减至零，证明此程序的正确性需要用归纳法处理 @${n}。由 @${0
\leq n} 且 @${n \neq 0}，可得 @${0 \leq (n - 1)}，所以递归调用过程
@tt{partial-vector-sum} 仍然满足其合约。

现在，要解决原问题就简单多了。因为向量长度为0时无法使用过程
@tt{partial-vector-sum}，所以得另行处理这种情况。

@; racketblock with contracts and usage
@racketblock[
@#,elem{@bold{@tt{vector-sum}} : @m{Vectorof(Int) \to Int}}
@#,elem{@bold{用法} : @tt{(vector-sum @m{v}) = @m{\sum\limits_{i=0}^{i=length(v)-1} v_i}}}
(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n)
          0
          (partial-vector-sum v (- n 1))))))
]

还有许多情况下，引入辅助变量或过程来解决问题会有帮助，甚至必不可少。只要能对新过
程做什么给出独立的定义，尽可以如此。

@exercise[#:level 2 #:tag "ex1.14"]{
 若 @m{0 \leq n < length(v)}，证明 @tt{partial-vector-sum} 的正确性。

}

@section[#:tag "ex"]{练习}

学写递归程序需要练习，那么我们拿几道习题结束本章。

每道习题都假定 @tt{s} 是一个符号，@tt{n} 是一个非负整数，@tt{lst} 是一个列表，
@tt{loi} 是一个整数列表，@tt{los} 是一个符号列表，@tt{slist} 是一个 s-list，
@tt{x} 是任意 Scheme 值；类似地，@tt{s1} 是一个符号，@tt{los2} 是一个符号列表，
@tt{x1} 是一个 Scheme 值，等等。还假定 @tt{pred} 是一个@emph{谓词}
(@emph{predicate})，即一个过程，取任意 Scheme 值，返回 @tt{#t} 或者 @tt{#f}。除
非某个具体问题另有限制，不要对数据作其他假设。在这些习题中，不需要检查输入是否符
合合约；对每个过程，都假定输入值是指定集合的成员。

定义，测试和调试每个过程。你的定义应当有合约和用法注释，像本章这样。可以随便定义
辅助过程，但是你定义的每个辅助过程都应该有其说明，如同 1.3 节那样。

测试这些程序时，先试试所有给出的例子，然后用其他例子测试，因为给定的例子不足以涵
盖所有可能的错误。

@(define duple-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
     '(define duple
        (lambda (n x)
          (if (eq? n 0)
              '()
              (cons x (duple (- n 1) x))))))))
@exercise[#:level 1 #:tag "ex1.15"]{
 @tt{(duple n x)} 返回包含 @tt{n} 个 @tt{x} 的列表。

 @examples[#:eval duple-eval
           #:label #f
           (duple 2 3)
           (duple 4 '(ha ha))
           (duple 0 '(blah))]
}

@(define invert-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
     '(define invert
        (lambda (lst)
          (if (null? lst)
              '()
              (cons (invert-2-lst (car lst))
                    (invert (cdr lst))))))
     '(define invert-2-lst
        (lambda (2-lst)
          (list (cadr 2-lst) (car 2-lst)))))))
@exercise[#:level 1 #:tag "ex1.16"]{
 @tt{lst} 是由二元列表（长度为2的列表）组成的列表，@tt{(invert lst)} 返回一列表，
 把每个二元列表反转。

 @examples[#:eval invert-eval
           #:label #f
 (invert '((a 1) (a 2) (1 b) (2 b)))]
}

@(define down-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst))
              (down (cdr lst)))))))))
@exercise[#:level 1 #:tag "ex1.17"]{
 @tt{(down lst)} 给 @tt{lst} 的每个顶层元素加上一对括号。

 @examples[#:eval down-eval
          #:label #f
 (down '(1 2 3))
 (down '((a) (fine) (idea)))
 (down '(a (more (complicated)) object))]

}

@(define swapper-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define swapper
  (lambda (s1 s2 slist)
    (cond ((null? slist) '())
          ((eq? s1 (car slist))
           (cons s2 (swapper s1 s2 (cdr slist))))
          ((eq? s2 (car slist))
           (cons s1 (swapper s1 s2 (cdr slist))))
          ((symbol? (car slist))
           (cons (car slist) (swapper s1 s2 (cdr slist))))
          (else
           (cons (swapper s1 s2 (car slist))
                 (swapper s1 s2 (cdr slist))))))))))
@exercise[#:level 1 #:tag "ex1.18"]{
 @tt{(swapper s1 s2 slist)} 返回一列表，将 @tt{slist} 中出现的所有 @tt{s1} 替换
 为 @tt{s2}，所有 @tt{s2} 替换为 @tt{s1}。

 @examples[#:eval swapper-eval
           #:label #f
 (swapper 'a 'd '(a b c d))
 (swapper 'a 'd '(a d () c d))
 (swapper 'x 'y '((x) y (z (x))))]

}

@(define list-set-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define list-set
  (lambda (lst n x)
    (if (null? lst)
        '()
        (if (eq? n 0)
            (cons x (cdr lst))
            (cons (car lst)
                  (list-set (cdr lst) (- n 1) x)))))))))
@exercise[#:level 2 #:tag "ex1.19"]{
 @tt{(list-set lst n x)} 返回一列表，除第 @tt{n} 个元素 （从零开始计数）为
 @tt{x} 外，与 @tt{lst} 相同。

 @examples[#:eval list-set-eval
           #:label #f
 (list-set '(a b c d) 2 '(1 2))
 (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)]

}

@(define count-occurrences-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define count-occurrences
  (lambda (s slist)
    (cond ((null? slist) 0)
          ((eq? s (car slist))
           (+ 1 (count-occurrences s (cdr slist))))
          ((symbol? (car slist))
           (count-occurrences s (cdr slist)))
          (else
           (+ (count-occurrences s (car slist))
              (count-occurrences s (cdr slist))))))))))
@exercise[#:level 1 #:tag "ex1.20"]{
 @tt{(count-occurrences s slist)} 返回 @tt{slist} 中出现的 @tt{s} 个数。

 @examples[#:eval count-occurrences-eval
           #:label #f
 (count-occurrences 'x '((f x) y (((x z) x))))
 (count-occurrences 'x '((f x) y (((x z) () x))))
 (count-occurrences 'w '((f x) y (((x z) x))))]

}

@(define product-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define product
  (lambda (sos1 sos2)
    (cond ((or (null? sos1) (null? sos2))
           '())
          (else
           (append (s-product (car sos1) sos2)
                   (product (cdr sos1) sos2))))))
'(define s-product
  (lambda (s sos)
    (if (null? sos)
        '()
        (cons (list s (car sos))
              (s-product s (cdr sos)))))))))
@exercise[#:level 2 #:tag "ex1.21"]{
 @tt{sos1} 和 @tt{sos2} 是两个没有重复元素的符号列表，@tt{(product sos1 sos2)}返
 回二元列表的列表，代表 @tt{sos1} 和 @tt{sos2} 的笛卡尔积。二元列表排列顺序不限。

 @examples[#:eval product-eval
           #:label #f
 (product '(a b c) '(x y))]

}

@(define filter-in-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst)
                  (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst)))))))))
@exercise[#:level 2 #:tag "ex1.22"]{
 @tt{(filter-in pred lst)} 返回的列表，由 @tt{lst} 中满足谓词 @tt{pred} 的元素组
 成。

 @examples[#:eval filter-in-eval
           #:label #f
 (filter-in number? '(a 2 (1 3) b 7))
 (filter-in symbol?  '(a (b c) 17 foo))]

}

@(define list-index-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define list-index
  (lambda (pred lst)
    (list-index-iter pred lst 0)))
'(define list-index-iter
  (lambda (pred lst n)
    (if (null? lst)
        #f
        (if (pred (car lst))
            n
            (list-index-iter pred (cdr lst) (+ n 1)))))))))
@exercise[#:level 2 #:tag "ex1.23"]{
 @tt{(list-index pred lst)} 返回 @tt{lst} 中第一个满足谓词 @tt{pred} 的元素位置，
     从零开始计数。如果 @tt{lst} 中没有元素满足谓词，@tt{list-index} 返回 @tt{#f}。

 @examples[#:eval list-index-eval
          #:label #f
 (list-index number? '(a 2 (1 3) b 7))
 (list-index symbol?  '(a (b c) 17 foo))
 (list-index symbol?  '(1 2 (a b) 3))]

}

@(define every?-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define every?
  (lambda (pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f)))))))
@exercise[#:level 2 #:tag "ex1.24"]{
 若 @tt{lst} 中的任何元素不满足 @tt{pred}，@tt{(every? pred lst)} 返回 @tt{#f}，
 否则返回 @tt{#t}。

 @examples[#:eval every?-eval
           #:label #f
 (every? number? '(a b c 3 e))
 (every? number? '(1 2 3 4 5))]

}

@(define exists?-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define exists?
  (lambda (pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst)))))))))
@exercise[#:level 2 #:tag "ex1.25"]{
 若 @tt{lst} 中的任何元素满足 @tt{pred}，@tt{(exists? pred lst)} 返回 @tt{#t}，否则返回 @tt{#f}。

 @examples[#:eval exists?-eval
           #:label #f
 (exists? number? '(a b c 3 e))
 (exists? number? '(a b c d e))]

}

@(define up-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (if (list? (car lst))
            (append (car lst)
                    (up (cdr lst)))
            (cons (car lst)
                  (up (cdr lst))))))))))
@exercise[#:level 2 #:tag "ex1.26"]{
 @tt{(up lst)} 移除 @tt{lst} 中每个顶层元素周围的一对括号。如果顶层元素不是列表，
 则照原样放入结果中。@tt{(up (down lst))} 的结果与 @tt{lst} 相同，但 @tt{(down
 (up lst))} 不一定是列表（参见练习 1.17）。

 @examples[#:eval up-eval
           #:label #f
 (up '((1 2) (3 4)))
 (up '((x (y)) z))]

}

@(define flatten-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (if (list? (car slist))
            (append (flatten (car slist))
                    (flatten (cdr slist)))
            (cons (car slist)
                  (flatten (cdr slist))))))))))
@exercise[#:level 2 #:tag "ex1.27"]{
 @tt{(flatten slist)} 返回一列表，由 @tt{slist} 中的符号按出现顺序组成。直觉上，@tt{flatten} 移除参数内的所有内层括号。

 @examples[#:eval flatten-eval
           #:label #f
 (flatten '(a b c))
 (flatten '((a) () (b ()) () (c)))
 (flatten '((a b) c (((d)) e)))
 (flatten '(a b (() (c))))]

}

@(define merge-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define merge
  (lambda (loi1 loi2)
    (cond ((null? loi2)
           loi1)
          ((null? loi1)
           loi2)
          ((< (car loi1) (car loi2))
           (cons (car loi1)
                 (merge (cdr loi1) loi2)))
          (else
           (cons (car loi2)
                 (merge loi1 (cdr loi2)))))))
'(define sort
  (lambda (loi)
    (sort-iter '() loi)))
'(define sort-iter
  (lambda (loi1 loi2)
    (if (null? loi2)
        loi1
        (sort-iter (merge loi1 (list (car loi2)))
                   (cdr loi2))))))))
@exercise[#:level 2 #:tag "ex1.28"]{
 @tt{loi1} 和 @tt{loi2} 是元素按照升序排列的整数列表，@tt{(merge loi1 loi2)} 返
 回 @tt{loi1} 和 @tt{loi2} 中所有整数组成的的有序列表。

 @examples[#:eval merge-eval
           #:label #f
 (merge '(1 4) '(1 2 8))
 (merge '(35 62 81 90 91) '(3 83 85 90))]

}

@exercise[#:level 2 #:tag "ex1.29"]{
 @tt{(sort loi)} 返回一列表，将 @tt{loi} 中的元素按照升序排列。

 @examples[#:eval merge-eval
           #:label #f
 (sort '(8 2 5 2 3))]

}

@(define sort/predicate-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define sort/predicate
  (lambda (pred loi)
    (sort-iter/predicate pred '() loi)))
'(define sort-iter/predicate
  (lambda (pred loi1 loi2)
    (if (null? loi2)
        loi1
        (sort-iter/predicate
         pred
         (merge/predicate pred loi1 (list (car loi2)))
         (cdr loi2)))))
'(define merge/predicate
  (lambda (pred loi1 loi2)
    (cond ((null? loi2)
           loi1)
          ((null? loi1)
           loi2)
          ((pred (car loi1) (car loi2))
           (cons (car loi1)
                 (merge/predicate pred (cdr loi1) loi2)))
          (else
           (cons (car loi2)
                 (merge/predicate pred loi1 (cdr loi2))))))))))
@exercise[#:level 2 #:tag "ex1.30"]{
 @tt{(sort/predicate pred loi)} 返回一列表，将 @tt{loi} 的元素按照谓词指定的顺序
 排列。

 @examples[#:eval sort/predicate-eval
           #:label #f
 (sort/predicate < '(8 2 5 2 3))
 (sort/predicate > '(8 2 5 2 3))]

}

@exercise[#:level 1 #:tag "ex1.31"]{
 写出如下过程，对二叉树（定义 1.1.7）进行运算：@tt{leaf} 和 @tt{interior-node}
 生成二叉树，@tt{leaf?} 检查二叉树是否是一片叶子，@tt{lson}、@tt{rson}和
 @tt{contents-of} 取出一个节点的各部分。@tt{contents-of} 应对叶子和内部节点都适
 用。

}

@exercise[#:level 1 #:tag "ex1.32"]{
 写出过程 @tt{double-tree}，它取一棵二叉树，形如定义 1.1.7，生成另一棵二叉树，把
 原二叉树中的所有整数翻倍。

}

@exercise[#:level 2 #:tag "ex1.33"]{
 写出过程 @tt{mark-leaves-with-red-depth}，它取一棵二叉树（定义 1.1.7），生成与
 原树形状相同的另一棵二叉树，但在新的二叉树中，每个叶子中的整数表示它和树根之间
 含有 @tt{red} 符号的节点数。例如，表达式
 @nested{
  @codeblock{
  (mark-leaves-with-red-depth
   (interior-node 'red
    (interior-node 'bar
     (leaf 26)
     (leaf 12))
    (interior-node 'red
     (leaf 11)
     (interior-node 'quux
      (leaf 117)
      (leaf 14)))))
  }}
 使用练习 1.31 中定义的过程，应返回二叉树

 @codeblock{
 (red
  (bar 1 1)
  (red 2 (quux 2 2)))
 }

}

@(define path-eval
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 50])
    (make-evaluator
     'racket/base
'(define bst-leaf '())
'(define bst-node
  (lambda (n lbst rbst)
    (list n lbst rbst)))
'(define bst-leaf? null?)
'(define bst-lson cadr)
'(define bst-rson caddr)
'(define bst-contents
  (lambda (bst)
    (if (bst-leaf? bst)
        (bst-leaf)
        (car bst))))
'(define path
  (lambda (n bst)
    (cond ((bst-leaf? bst)
           '())
          ((< (bst-contents bst) n)
           (cons 'right
                 (path n (bst-rson bst))))
          ((> (bst-contents bst) n)
           (cons 'left
                 (path n (bst-lson bst))))
          (else
           '())))))))
@exercise[#:level 3 #:tag "ex1.34"]{
 写出过程 @tt{path}，它取一个整数 @tt{n} 和一棵含有整数 @tt{n} 的二叉搜索树（第
 @elem[#:style question]{10} 页）@tt{bst}，返回由 @tt{left} 和 @tt{right} 组成的
 列表，表示如何找到包含 @tt{n} 的节点。如果在树根处发现 @tt{n}，它返回空列表。

 @examples[#:eval path-eval
           #:label #f
 (path 17 '(14 (7 () (12 () ()))
               (26 (20 (17 () ())
                       ())
                   (31 () ()))))]

}

@exercise[#:level 3 #:tag "ex1.35"]{
 写出过程 @tt{number-leaves}，它取一棵二叉树，生成与原树形状相同的二叉树，但叶子
 的内容从 0 开始计的整数。例如，
 @nested{
  @codeblock{
  (number-leaves
   (interior-node 'foo
    (interior-node 'bar
     (leaf 26)
     (leaf 12))
    (interior-node 'baz
     (leaf 11)
     (interior-node 'quux
      (leaf 117)
      (leaf 14)))))
  }}
 应返回

 @codeblock{
 (foo
  (bar 0 1)
  (baz
   2
   (quux 3 4)))
 }

}

@exercise[#:level 3 #:tag "ex1.36"]{
 写出过程 @tt{g}，则第 @elem[#:style question]{23} 页的 @tt{number-elements} 可
 以定义为：

 @codeblock{
 (define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))
 }
}
