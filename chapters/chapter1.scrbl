#lang scribble/book
@(require "style.rkt"
          (except-in latex-utils/scribble/theorem definition-ref theorem-ref)
          latex-utils/scribble/math
          latex-utils/scribble/utils
          scribble/manual
          scribble-math
          scribble/example
          scribble/core
          scribble/example
          scriblib/footnote
          racket/sandbox)

@mainmatter

@title[#:style part-title-style-numbered #:tag "isd"]{归纳式数据集}

解释器与检查器一类的程序是编程语言处理器的核心，本章介绍写这些用到的基本编程工具。


因为编程语言的语法通常为嵌套或者树状结构，递归将是我们的主要技巧。
@secref{s1.1}和@secref{s1.2}介绍归纳定义数据结构的方法，并展示如何用这类定义指导
递归程序的编写。@secref{s1.3}展示如何将这些技巧推广到更为复杂的程序。本章以大量
练习作结。这些练习是本章的核心。欲掌握本书余下部分依赖的递归编程技巧，得自它们的
经验不可或缺。

@section[#:style section-title-style-numbered #:tag "s1.1"]{递推定义的数据}

@eopl-index[#:range-mark 'start "Recursive data types" "specifying"]
编写过程代码时，必须明确知道什么样的值能作为过程的参数，什么样的值是过程的合法返
回值。这些值的集合通常很复杂。本节介绍定义值集合的形式化技术。

@subsection[#:style section-title-style-numbered #:tag "s1.1.1"]{归纳定义法}

@eopl-index[#:range-mark 'start "Inductive specifications"]
归纳定义法是定义值集合的有效方法。为解释这一方法，我们用它来描述自然数 @${N =
{0,1,2,...}} 的某一子集 @${S}。

@; definition: (def #:title title #:tag tag pre-flow ...)
@definition[#:title #f #:tag "d1.1.1"]{@eopl-index["Top-down definition"]

自然数 @${n} 属于 @${S}，当且仅当：
 @itemlist[#:style 'ordered
  @item{@${n = 0}，或}

  @item{@${n - 3 \in S}}
 ]
}


来看看如何用这一定义判断哪些自然数属于 @${S}。已知 @${0 \in S}，因此 @${3 \in S}，
因为 @${(3 - 3) = 0}，且 @${0 \in S}。同样地，@${6 \in S}，因为 @${(6 - 3) = 3}，
且 @${3 \in S}。依此类推，可得结论：所有 @${3} 的整数倍都属于 @${S}。

其他自然数呢？@${1 \in S} 吗？已知 @${1 \ne 0}，所以条件一不满足。此外，@${(1 -
3) = -2}，不是自然数，故不是 @${S} 的元素，因此条件二不满足。因为 @${1} 不满足任
一条件，所以 @${1 \notin S}。同样地，@${2 \notin S}。@${4}呢？仅当 @${1 \in S}
时 @${4 \in S}。但 @${1 \notin S}，所以 @${4 \notin S}。同理可得，如果 @${n} 是
自然数且不是 @${3} 的整数倍，则 @${n \notin S}。

据此推论，可得 @${S} 是 @${3} 的整数倍自然数集合。

可以用该定义编写一个函数，判断一个自然数 @${n} 是否属于 @${S}。

@; codeblock with contracts and usage
@eopl-code{
@racketblock[
@#,eopl-index[#:delayed #f @eopl-index-entry[@elem{@bold{@tt{is-S?}}} "inS"]]
@#,elem{@bold{@tt{in-S?}} : @${\mathit{N} \to \mathit{Bool}}}
@#,elem{@bold{用法} : @tt{(in-S? n) = #t 若 n 属于 S，否则 #f}}
(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))
]
}

这里根据定义，我们用Scheme编写了一个递归过程。符号 @racket[in-S? :
@#,elem{@${\mathit{N} \to \mathit{Bool}}}] 是一条注释，称为该函数
@eopl-index["Contract"]
的@term["contract"]{合约}。它表示 @racket[in-S?] 应为一过程，取一自然数，产生一
布尔值。这样的注释对阅读和编写代码很有帮助。

要判断是否 @${n \in S}，先判断是否 @${n = 0}。如果是，那么答案为真。否则，判断是
否 @${n - 3 \in S}。欲知此，首先判断是否 @${(n - 3) \geqslant 0}。如果是，那么可
以用我们的过程判断它是否属于 @${S}。如果不是，那么 @${n} 不可能属于 @${S}。


@${S} 又能够定义为：

@definition[#:title #f #:tag "d1.1.2"]{
 集合 @${S} 为 @${N} 所包含的集合中，满足如下两条性质的最小集合：@eopl-index["Smallest set"]

 @itemlist[#:style 'ordered
  @item{@${0 \in S}，且}
  @item{若 @${n \in S}，则 @${n + 3 \in S}。}
 ]
}

@exact-elem{“}最小集合@exact-elem{”}是指该集合满足性质 1 和 2，并且是其他任何
满足性质 1 和 2 的集合的子集。易知只能有一个这样的集合：如果 @${S_1} 和 @${S_2}
都满足性质 1 和 2，并且都为最小，那么 @${S_1 \subseteq S_2}（因为 @${S_1} 最小）
且 @${S_2 \subseteq S_1}（因为 @${S_2} 最小），因此 @${S_1 = S_2}。之所以需要这
一额外条件，是因为否则的话将有许多集合满足其他两个条件（见@exercise-ref{ex1.3}）。

该定义还能表示为：

@; infer should be implemented in scribble rather than use \infer directly
@; infer: (infer conclusion hypothesis ...)
@; @infer[${0 \in S}]
@$${\infer{0 \in S}{}}

@; @infer[${(n + 3) \in S}]{$@{n \in S}}
@$${\infer{(n + 3) \in S}{n \in S}}

@eopl-index["Rules of inference"]
这只是前一定义的简便表示。每个条目称为一条@term["rule of inference"]{推理规则}，
或称@term["rule"]{规则}；水平线读作@exact-elem{“}若-则@exact-elem{”}。线上部分
称作@term["hypothesis"]{假设}或者@eopl-index{Antecedent}@term["antecedent"]{前件}；
@eopl-index["Conclusion"]
@eopl-index["Consequent"]
@eopl-index["Hypothesis"]
线下部分称作@term["conclusion"]{结论} 或者@term["consequent"]{后件}。罗列两个或
更多假设时，它们以隐含的@exact-elem{“}与@exact-elem{”}连接（见@definition-ref{d1.1.5}）。
没有假设的规则称作@eopl-index{Axiom}@term["axiom"]{公理}。写公理时通常不加水平
线，如：

@$${0 \in S}

该规则意为，自然数 @${n} 属于 @${S}，当且仅当能用有限次推理规则，从公理推得陈述
@exact-elem{“}@${n \in S}@exact-elem{”}。这一解释自然使 @${S} 成为闭合于该规则
的最小集合。

@eopl-index[(eopl-index-entry "Bottom-up definition" "Bottomupdefinition")]
@eopl-index["Top-down definition"]
这些定义意思相同。我们把版本一称作@term["top-down"]{自顶向下} 的定义，版本二
称作@term["bottom-up"]{自底向上} 的定义，版本三称作@term[#f]{推理规则}定义。
@eopl-index["Rules-of-inference definition"]

再来看几个运用这些的例子。

@eopl-index[#:range-mark 'start @eopl-index-entry[@elem{List of integers (@List-of-Int-$[])} "Listofintegers"]]
@definition[#:title "整数列表，自顶向下" #:tag "d1.1.3"]{
  Scheme列表是整数列表，当且仅当：
  @itemlist[#:style 'ordered
   @item{列表为空，或}
   @item{列表为序对，首项为整数，余项为整数列表。}
 ]
}

我们用 @Int-$[] 表示所有整数的集合，用 @List-of-Int-$[] 表示所有整数列表的集合。

@definition[#:title "整数列表，自底向上" #:tag "d1.1.4"]{

集合@List-of-Int-$[]是满足如下两条性质的最小Scheme列表集合：

 @itemlist[#:style 'ordered
  @item{@${\textnormal{@tt{()}} \in @List-of-Int-${}}，或}

  @item{若 @${n \in \mathit{Int}} 且 @${l \in @List-of-Int-${}}，则
  @${\textnormal{\texttt{(}}n\phantom{x}.\phantom{x}l\textnormal{\texttt{)}} \in
  @List-of-Int-${}}。}
 ]
 }


@eopl-index["Dot notation"]
这里，我们用中缀@exact-elem{“}@tt{.}@exact-elem{”}代表 Scheme 中 @tt{cons} 操
作的结果。式子 @tt{(@${n} . @${l})} 代表 Scheme 序对的首项为 @${n}，余项为 @${l}。

@definition[#:title "整数列表，推理规则" #:tag "d1.1.5"]{
@nested[#:style normalfont]{
@eopl-index["Rules-of-inference definition"]
@$${\infer{@tt{()} \in @List-of-Int-${}}{}}

@$${\infer{@tt{(@${n} . @${l})} \in @List-of-Int-${}}{n \in \mathit{Int} & l \in
@List-of-Int-${}}} }
@eopl-index[#:range-mark 'end @eopl-index-entry[@elem{List of integers (@List-of-Int-$[])} "Listofintegers"]]
}

这三个定义等价。来看看如何用它们生成一些 @List-of-Int-$[] 的元素。

@itemlist[#:style 'ordered

 @item{由@definition-ref{d1.1.4} 的性质 1 或@definition-ref{d1.1.5} 的规则 1，
 @tt{()} 是整数列表。}

 @item{由@definition-ref{d1.1.4} 的性质 2，@tt{(14 . ())} 是整数列表。因为
       @tt{14} 是整数，@tt{()} 是整数列表。写成 @List-of-Int-$[] 规则二的形式，
       就是

       @$${\infer{@tt{(14 . ())} \in @List-of-Int-$[]} {@tt{14} \in \mathit{Int}
           & @tt{()} \in @List-of-Int-$[]}}}

 @item{由@definition-ref{d1.1.4} 的性质 2，@tt{(3 . (14 . ()))} 是整数列表。因为
       @tt{3} 是整数，@tt{(14 . ())} 是整数列表。仍写成@List-of-Int-$[] 规则二的
       形式，是

       @$${\infer{@tt{(3 . (14 . ()))} \in @List-of-Int-$[]} {@tt{3} \in
           \mathit{Int} & @tt{(14 . ())} \in @List-of-Int-$[]}} }

 @item{由@definition-ref{d1.1.4} 的性质 2，@tt{(-7 . (3 . (14 . ())))} 是整数列
       表。因为 @tt{-7} 是整数，@tt{(3 . (14 . ()))} 是整数列表。再次写成
       @List-of-Int-$[] 规则二的形式，是

       @$${\infer{\hphantom{\texttt{x}}@tt{(-7 . (3 . (14 . ())))} \in @List-of-Int-$[]\hphantom{\texttt{x}}}
                 {@tt{-7} \in \mathit{Int} & @tt{(3 . (14 . ()))}\in @List-of-Int-$[]}}}

 @item{不按照这种方式得到的都不是整数列表。}

]

改句点表示法为列表表示法，可知 @tt{()}、 @tt{(14)}、 @tt{(3 14)} 以及 @tt{(-7 3
14)} 都是 @List-of-Int-$[] 的元素。

还可以结合各条规则来证明 @${@tt{(-7 . (3 . (14 . ())))} \in @List-of-Int-$[]}，
以见出整个推理过程。下面的树状图叫做@term[#:tag "deriv-tree" "derivation"]{推导}
或@term["deduction tree"]{推理树}。
@eopl-index["Deduction"]
@eopl-index["Derivation tree"]

@$${\infer{\hphantom{\texttt{xx}}@tt{(-7 . (3 . (14 . ())))} \in @List-of-Int-$[]\hphantom{\texttt{xx}}}
          {@tt{-7} \in \mathit{Int} \hphantom{\texttt{x}} &
           \infer{\hphantom{\texttt{x}}@tt{(3 . (14 . ()))} \in @List-of-Int-$[]\hphantom{\texttt{x}}}
                 {@tt{3} \in \mathit{Int} &
                  \infer{@tt{(14 . ())} \in @List-of-Int-$[]}
                        {@tt{14} \in \mathit{Int} & @tt{()} \in @List-of-Int-$[]}}
          }}
@eopl-index[#:range-mark 'end "Inductive specifications"]

@exercise[#:level 1 #:tag "ex1.1"]{

 写出下列集合的归纳定义。以三种方式（自顶向下，自底向上，推理规则）写出每个定
 义，并用你的规则推导出各集合的一些元素。

  @itemlist[#:style 'ordered

   @item{@m{\{ 3n + 2 \mid n \in N \}}}

   @item{@m{\{ 2n + 3m + 1 \mid n, m \in N \}}}

   @item{@m{\{ (n, 2n + 1) \mid n \in N \}}}

   @item{@m{\{ (n, n^2) \mid n \in N \}}。不要在你的规则中使用平方。提示：想一想
         方程 @m{ (n + 1) ^ 2 = n ^ 2 + 2n + 1}。}

  ]
 }

@exercise[#:level 2 #:tag "ex1.2"]{

 下面的几对规则分别定义了什么集合？给出解释。

 @itemlist[#:style 'ordered

  @item{@m{(0, 1) \in S \qquad \infer{(n + 1, k + 7) \in S}{(n, k) \in S}}}

  @item{@m{(0, 1) \in S \qquad \infer{(n + 1, 2k) \in S}{(n, k) \in S}}}

  @item{@m{(0, 0, 1) \in S \qquad \infer{(n + 1, j, i + j) \in S}{(n, i, j) \in S}}}

  @item{@m{\text{[}\mathord{\star}\mathord{\star}\mathord{\star}\text{]}}
  @m{\quad} @m{(0, 1, 0) \in S \qquad \infer{(n + 1, i + 2, i + j) \in S}{(n, i,
  j) \in S}}}

 ]

}

@exercise[#:level 1 #:tag "ex1.3"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex1.3"] "Smallest set"]
找出自然数的子集 @m{T}，满足 @m{0 \in T}，且对任何 @m{n \in T}，都有 @m{n + 3
\in T}，但 @m{T \neq S}，@m{S} 是由@definition-ref{d1.1.2} 给出的集合。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex1.3"] "Smallest set"]

}

@subsection[#:style section-title-style-numbered #:tag "s1.1.2"]{语法定义法}

@eopl-index[#:range-mark 'start "Grammars"]
前述例子较为直观，但是不难想象，描述更复杂的数据类型会有多麻烦。为了方便，我们展
示如何用@term["grammar"]{语法} 定义集合。语法通常用来指定字符串的集合，但也能用
来定义值的集合。

@eopl-index[#:range-mark 'start @eopl-index-entry[@elem{List of integers (@List-of-Int-$[])} "Listofintegers"]]
例如，集合 @List-of-Int-$[] 可用语法定义为：

@; @grammar : (grammar production ...)
@; @production : (production name expression #:code code-item)
@; @code-item : elem
@; @grammar{

@nested[#:style small]{
@envalign*{@List-of-Int-raw[] &::= @tt{()} \\[-3pt]
           @List-of-Int-raw[] &::= @tt{(@Int-m[] . @List-of-Int-m[])}
}}

@; }

这两条规则对应上述@definition-ref{d1.1.4} 中的两条性质。规则一是说空表属于
@List-of-Int-$[]；规则二是说，若 @${n} 属于 @Int-$[] 且 @${l} 属于
@List-of-Int-$[]，则 @tt{(@${n} . @${l})} 属于 @List-of-Int-$[]。这些规则
叫做@term[#f]{语法}。

来看看该定义的各个部分，其中有：

@itemlist[

  @item{@bold{非终结符}。这些是所定义的集合名。本例中只定义了一个集合，但是通常，
        可能会定义数个集合。这些集合有时称为@term["syntactic category"]{句法类别}。
        @eopl-index["Nonterminal symbols"]
        @eopl-index["Syntactic categories"]

        依照惯例，我们将非终结符和集合名的首字母大写，在文中提及它们的元素时，则
        用小写。这要比听起来容易。例如， @${Expression} 是非终结符，但我们写作
        @${e \in \mathit{Expression}} 或 @exact-elem{“}@${e} 是一个
        expression@exact-elem{”}。

        @eopl-index[(eopl-index-entry "Backus-Naur Form (BNF)"
        "BackusNaurForm")]另一常见做法，名叫@term["Backus-Naur Form"]{巴科斯-诺
        尔范式} 或@term[#f]{BNF}，是在词周围加尖括号，如
        @${\langle}expression@${\rangle}。}

  @item{@bold{终结符}。这些是集合外在表示中的字符，在本例中，是
        @exact-elem{“}@tt{.}@exact-elem{”}、
        @exact-elem{“}@tt{(}@exact-elem{”}和
        @exact-elem{“}@tt{)}@exact-elem{”}。这些常用打字机字体写出，如
        @tt{lambda}。@eopl-index["Terminal symbols"]}

  @item{@bold{生成式}。规则叫做@term["production"]{生成式}。
        @eopl-index["Production of grammar"]每个生成式的左边是一个非终结符，右边
        包含终结符和非终结符。左右两边通常用符号 @${::=}分隔，读作@term[#f]{是}
        或@term[#f]{可以是}。式子右边用其他句法类别和@term[#f]{终结符}（如左括号、
        右括号和句点）指定一种方法，用以构建当前句法类别的元素。}

]

如果某些句法类别的含义在上下文中足够清晰，在生成式中提到它们时通常不作定义，如
@Int-$[]。

语法常常简写。当一个生成式的左边与前一生成式相同时，一般会略去。根据这一惯例，我
们的语法可以写作

@nested[#:style small]{
@envalign*{
@List-of-Int-raw[] &::= @tt{()} \\[-3pt]
                   &::= @tt{(@Int-m[] . @List-of-Int-m[])}
}}

给同一句法类别编写一组规则时，也可以只写一次 @${::=} 和左边内容，随后的各个右边
内容用特殊符号@exact-elem{“}@${\mid}@exact-elem{”}（竖线，读作@term[#f]{或}@eopl-index["Or"]）
分隔。用@exact-elem{“}@${\mid}@exact-elem{”}，@List-of-Int-$[] 的语法可写成：


@$${@List-of-Int-$[] ::= @tt{()} @$${\mid} @tt{(@Int-$[] . @List-of-Int-$[])}}
@eopl-index[#:range-mark 'end @eopl-index-entry[@elem{List of integers (@List-of-Int-$[])} "Listofintegers"]]

@eopl-index["Kleene star (closure)"]
另一种简写是@term[#:tag "kleene-star" "Kleene Star"]{克莱尼星号}，写作
@${\{...\}^*}。当它出现在右边时，表示一个序列，由任意多个花括号之间的内容组成。
用克莱尼星号，@List-of-Int-$[] 的定义可以简写为

@$${@List-of-Int-$[] ::= @tt{(@${\{\mathit{Int}\}^*})}}

这也包含没有任何内容的情况。如果内容出现 0 次，得到的是空字符串。

@eopl-index["Kleene plus"]
星号的变体是@term["Kleene Plus"]{克莱尼加号} @${\{...\}^+}，表示一个或多个内容的
序列。把上例中的 @${^*} 换成 @${^+}，定义的句法类别是非空整数列表。

@eopl-index[#:range-mark 'start "Separated list notation"]
星号的另一变体是@term["separated list"]{分隔表} 表示法。例如，
@${\mathit{Int}^{*(c)}} 表示一个序列，包含任意数量的非终结符 @Int-$[] 元素，以非
空字符序列 @${c} 分隔。这也包含没有元素的情况。如果有 0 个元素，得到的是空字符串。
例如，@${\mathit{Int}^{*(,)}} 包含字符串

@nested{
@eopl-code{
@verbatim|{
8
14, 12
7, 3, 14, 16
}|
}

@${\mathit{Int}^{*(;)}} 包含字符串

@eopl-code{
@verbatim|{
8
14; 12
7; 3; 14; 16
}|
@eopl-index[#:range-mark 'end "Separated list notation"]}
}

这些简写不是必需的，总能够不用它们重写语法。

@eopl-index["Derivation, syntactic"]
@eopl-index["Syntactic derivation"]
对由语法定义的集合，可以用@term["syntactic derivation"]{句法推导} 证明给定值是其
元素。这样的推导从集合对应的非终结符开始，在由箭头@${\Rightarrow} 指示的每一步中，
如果非终结符对应的句法类别未做定义，则将其代换为该类别的已知元素，否则代换为对应
规则右边的内容。例如，前述证明@exact-elem{“}@tt{(14 . ())} 是整数列表
@exact-elem{”}，可以用句法推导化为

@nested[#:style small]{
@envalign*{
 @List-of-Int-raw[] &\Rightarrow @tt{(@Int-m[] . @List-of-Int-m[])} \\[-3pt]
                    &\Rightarrow @tt{(14 . @List-of-Int-m[])} \\[-3pt]
                    &\Rightarrow @tt{(14 . ())}}}

非终结符的替换顺序无关紧要，所以 @tt{(14 . ())} 的推导也可以写成：

@nested[#:style small]{
@envalign*{
 @List-of-Int-raw[] &\Rightarrow @tt{(@Int-m[] . @List-of-Int-m[])} \\[-3pt]
                    &\Rightarrow @tt{(@Int-m[] . ())} \\[-3pt]
                    &\Rightarrow @tt{(14 . ())}
}}

@exercise[#:level 1 #:tag "ex1.4"]{

 写出从 @List-of-Int-m[] 到 @tt{(-7 . (3 . (14 ())))} 的推导。@linebreak[]

}

再来看一些有用集合的定义。

@itemlist[#:style 'ordered

 @item{@eopl-index[#:range-mark 'start @eopl-index-entry[@elem{S-exp (@${\mathit{S\mbox{-}exp}})} "Sexp"]]
 @eopl-index[#:range-mark 'start @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
 许多符号操作过程用于处理只包含符号和具有类似限制的列表。我们把这些叫做
 @tt{s-list}，定义如下：

 @definition[#:title "s-list，s-exp" #:tag "d1.1.6"]{
 @nested[#:style normalfont]{
 @nested[#:style small]{
  @envalign*{\mathit{S\mbox{-}list} &::= @tt{(@m{\{\mathit{S\mbox{-}exp}\}^*})} \\[-3pt]
             \mathit{S\mbox{-}list} &::= \mathit{Symbol} \mid \mathit{S\mbox{-}list}}}}
 }

 @elemtag["s-list"]{s-list} 是 s-exp 的列表，s-exp 或者是 s-list，或者是一个符号。
 这里是一些 s-list。

 @eopl-code{
 @verbatim|{
 (a b c)
 (an (((s-list)) (with () lots) ((of) nesting)))
 }|
 }

 有时也使用更宽松的 s-list 定义，既允许整数，也允许符号。
 @eopl-index[#:range-mark 'end @eopl-index-entry[@elem{S-exp (@${\mathit{S\mbox{-}exp}})} "Sexp"]]
 @eopl-index[#:range-mark 'end @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
 }

 @item{使用三元素列表表示内部节点，则以数值为叶子，以符号标示内部节点的二叉树可
 用语法表示为：
 @eopl-index[(eopl-index-entry @elem{Binary tree (@${\mathit{Bintree}})} "Binarytree")]

 @definition[#:title "二叉树" #:tag "d1.1.7"]{
 @nested[#:style normalfont]{
 @nested[#:style small]{
  @$${\mathit{Bintree} ::= \mathit{Int} \mid @tt{(@${\mathit{Symbol}} @${\mathit{Bintree}} @${\mathit{Bintree}})}}}
 }
 }

 这是此类树的几个例子：

 @eopl-code{
 @verbatim|{
 1
 2
 (foo 1 2)
 (bar 1 (foo 1 2))
 (baz
  (bar 1 (foo 1 2))
  (biz 4 5))
 }|
 }}

 @item{@term["lambda calculus"]{lambda 演算} 是一种简单语言，常用于研究编程语言
 理论。这一语言只包含变量引用，单参数过程，以及过程调用，可用语法定义为：
 @eopl-index["Lambda calculus"]

 @definition[#:title "lambda 演算" #:tag "d1.1.8"]{
 @eopl-index[#:range-mark 'start "Lambda expression (LcExp)"]
 @nested[#:style normalfont]{
 @nested[#:style small]{
  @envalign*{\mathit{LcExp} &::= \mathit{Identifier} \\[-3pt]
                            &::= @tt{(lambda (@m{\mathit{Identifier}}) @m{\mathit{LcExp}})} \\[-3pt]
                            &::= @tt{(@m{\mathit{LcExp}} @m{\mathit{LcExp}})}}}}

 其中，identifier 是除 @elem[#:style normalfont]{@tt{lambda}} 之外的任何符号。
 }

 @eopl-index["Binding" "lambda"]
 @eopl-index["Bound variable"]
 @eopl-index["Variable(s)" "bound variable"]
 第二个生成式中的 identifier 是 @tt{lambda} 表达式主体内的变量名。这一变量叫做表
 达式的@term["bound variable"]{绑定变量}，因为它绑定（或称捕获）主体内出现的任何
 同名变量。出现在主体内的同名变量都指代这一个。
 @eopl-index[#:range-mark 'end "Lambda expression (LcExp)"]

 要明白这怎么用，考虑用算术操作符扩展的 lambda 演算。在这种语言里，

 @eopl-code{@codeblock{(lambda (x) (+ x 5))}}

 是一表达式，@tt{x} 是其绑定变量。这式子表示一个过程，把它的参数加5。因此，在

 @eopl-code{@codeblock{((lambda (x) (+ x 5)) (- x 7))}}

 中，最后一个出现的 @tt{x} 不是指 @tt{lambda} 表达式中绑定的 @tt{x}。
 @secref{s1.2.4}中介绍了 @tt{occurs-free?}，到时我们再讨论这个问题。

 该语法定义 @${\mathit{LcExp}} 的元素为 Scheme 值，因此很容易写出程序来处理它们。

 }

]

@eopl-index["Context-free grammar"]
这些语法叫做@term["context-free"]{上下文无关} 语法，因为一条规则定义的句法类别可
以在任何引用它的上下文中使用。有时这不够严格。考虑@elemtag["bst"]{二叉搜索树}。
其节点或者为空，或者包含一个整数和两棵子树
@eopl-index[(eopl-index-entry @elem{Binary search tree (@${\mathit{Binary\mbox{-}search\mbox{-}tree}})} "Binarysearchtree")]
@$${\mathit{Binary\mbox{-}search\mbox{-}tree} ::= @tt{()} \mid
    @tt{(@Int-$[] @${\mathit{Binary\mbox{-}search\mbox{-}tree}} @${\mathit{Binary\mbox{-}search\mbox{-}tree}})}}

这如实反映了每个节点的结构，但是忽略了二叉搜索树的一个要点：所有左子树的键值都小
于（或等于）当前节点，所有右子树的键值都大于当前节点。

@eopl-index[#:range-mark 'start "Invariant"]
因为这条额外限制，从 @${\mathit{Binary\mbox{-}search\mbox{-}tree}} 得出的句法推
导并不都是正确的二叉搜索树。要判定某个生成式能否用于特定的句法推导，必须检查生成
式用在哪种上下文。这种限制叫做@term["context-sensitive constraints"]{上下文敏感
限制}，或称@term[#:tag "invariant" "invariants"]{不变式}。
@eopl-index[#:range-mark 'start "Context-sensitive constraint"]

定义编程语言的语法也会产生上下文敏感限制。例如，在许多编程语言中变量必须在使用之
前声明。对变量使用的这一限制就对其上下文敏感。虽然可以用形式化方法定义上下文敏感
限制，但这些方法远比本章考虑的复杂。实际中，常用的方法是先定义上下文无关语法，随
后再用其他方法添加上下文敏感限制。@secref{types}展示了这种技巧的一个例子。
@eopl-index[#:range-mark 'end "Context-sensitive constraint"]
@eopl-index[#:range-mark 'end "Grammars"]
@eopl-index[#:range-mark 'end "Invariant"]
@eopl-index[#:range-mark 'end "Recursive data types" "specifying"]

@subsection[#:style section-title-style-numbered #:tag "s1.1.3"]{归纳法}

@eopl-index[#:range-mark 'start "Recursive data types" "proving properties of"]
用归纳法描述的集合，其定义有两种用法：证明关于集合元素的定理，写出操作集合元素的
程序。这里给出一个此类证明的例子，写程序留作下节的主题。

@theorem[#:tag "t1.1.1"]{
 @eopl-index[(eopl-index-entry @elem{Binary tree (@${\mathit{Bintree}})} "Binarytree")]
 令 t 为二叉树，形如@definition-ref{d1.1.7}，则 t 包含奇数个节点。
}

@; @proof
@; @{
@parprf{

 @eopl-index["Induction hypothesis"]
 @eopl-index[#:range-mark 'start "Induction, proof by"]
 用归纳法证明 @${t} 的大小。令 @${t} 的大小等于 @${t} 中节点的个数。归纳假设为
 @${\mathit{IH}(k)}：树的大小@${\leq k}时有奇数个节点。依照归纳法的惯例：先证明
 @${\mathit{IH}(0)}为真，然后证明若对任一整数 @${k}，@${\mathit{IH}} 为真，则对
 @${k + 1}，@${\mathit{IH}} 也为真。

 @itemlist[#:style 'ordered

  @item{没有哪棵树只有 @${0} 个节点，所以 @${\mathit{IH}(0)} 显然成立。}

  @item{设 @${k} 为整数时，@${\mathit{IH}(k)} 成立，即，任何树的节点数 @${\leq
  k} 时，其实际数目为奇数。需证明 @${\mathit{IH}(k + 1)} 也成立：任何树的节点数
  @${\leq k + 1} 时，节点数为奇数。若 @${t} 有 @${\leq k + 1} 个节点，根据二叉树
  的定义，只有两种可能：

  @itemlist[#:style 'ordered

   @item{@${t} 形如 @${n}，@${n} 为整数。此时 @${t} 只有一个节点，1为奇数。}

   @item{@${t} 形如 @${@tt{(@${sym} @${t_1} @${t_2})}}，其中，@${sym} 是一符号，
   @${t_1} 和 @${t_2} 是树。此时 @${t_1} 和 @${t_2} 节点数少于 @${t}。因为 @${t}
   有 @${\leq k + 1}个节点，@${t_1} 和 @${t_2} 一定有 @${\leq k} 个节点。因此它
   们符合 @${\mathit{IH}(k)}，一定各有奇数个节点，不妨分别设为@${2n_1 + 1} 和
   @${2n_2 + 1}。则算上两棵子树和根，原树中的节点总数为

   @$${(2n_1 + 1) + (2n_2 + 1) + 1 = 2(n_1 + n_2 + 1) + 1}

   也是一个奇数。}]}]

 陈述@exact-elem{“}@${\mathit{IH}(k + 1)} 成立@exact-elem{”}证毕，归纳完成。
 @eopl-index[#:range-mark 'end "Induction, proof by"]
 }
@; @}

证明的关键是树 @${t} 的子结构总是比 @${t} 自身小。这种证明模式
叫做@term["structural induction"]{结构化归纳法}。

@nested[#:style tip]{
 @centered{@bold{结构化归纳证明}}

 @para[#:style tip-content]{欲证明假设 @${\mathit{IH}(s)} 对所有结构 @${s} 为真，
 需证明：}

 @itemlist[#:style 'ordered
   @item{@${\mathit{IH}} 对简单结构（没有子结构）为真。}

   @item{若 @${\mathit{IH}} 对 @${s} 的子结构为真，则对 @${s} 本身也为真。
   @eopl-index[#:range-mark 'end "Recursive data types" "proving properties of"]}
 ]
}

@exercise[#:level 2 #:tag "ex1.5"]{
 @eopl-index[#:suffix @exer-ref-range["ex1.5"] "Lambda expression (LcExp)"]
 证明若 @m{e \in \mathit{LcExp}}，则 @m{e} 中的左右括号数量相等。

}

@section[#:style section-title-style-numbered #:tag "s1.2"]{推导递归程序}

@eopl-index[#:range-mark 'start "Follow the Grammar" "examples of"]
@eopl-index[#:range-mark 'start "Inductive specifications" "recursive procedures based on"]
@eopl-index[#:range-mark 'start "Recursive data types" "programs that manipulate"]
@eopl-index[#:range-mark 'start "Recursive programs" "deriving"]
@eopl-index[#:range-mark 'start "Recursive programs" "examples of"]
我们已经用归纳定义法描述了复杂集合。我们能够分析归纳式集合的元素，观察如何从较小
元素构建集合。我们用这一想法写出了过程 @tt{in-S?}，用以判断自然数是否属于集合
@${S}。现在，我们用同样的想法定义更通用的过程，以便对归纳式集合做运算。

@eopl-index[#:range-mark 'start "Smaller-Subproblem Principle"]
递归过程依赖于一条重要原则：

@nested[#:style tip]{
 @centered{@bold{较小子问题原则}}

 @para[#:style tip-content]{若能化问题为较小子问题，则能调用解决原问题的过程解决
 子问题。}}

已求得的子问题解随后可用来求解原问题。这可行，因为每次过程调用都是针对较小的子问
题，直至最终调用，针对一个可以直接求解的问题，不需再次调用自身。
@eopl-index[#:range-mark 'end "Recursive data types" "programs that manipulate"]
@eopl-index[#:range-mark 'end "Recursive programs" "deriving"]
@eopl-index[#:range-mark 'end "Smaller-Subproblem Principle"]

我们用一些例子解释这一想法。

@subsection[#:style section-title-style-numbered #:tag "s1.2.1"]{@tt{list-length}}

@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{list-length}} "listlength"]]
@eopl-index[#:range-mark 'start @eopl-index-entry[@elem{List (@${\mathit{List}})} "Listlist"]]
标准的 Scheme 程序 @tt{length} 求出列表中的元素个数。

@eopl-code{
@examples[#:label #f (length '(a b c))
                     (length '((x) ()))]}

我们来写出自己的过程 @tt{list-length}，做同样的事。

@eopl-index["Contract"]
先来写出过程的@term[#f]{合约}。合约指定了过程可取参数和可能返回值的集合。合约也
可以包含过程的期望用法或行为。这有助于我们在编写时及以后追踪我们的意图。在代码中，
这是一条注释，我们用打字机字体示之，以便阅读。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{list-length}} : @${\mathit{List} \to \mathit{Int}}}
@#,elem{@bold{用法} : @tt{(list-length @${l}) = @${l}@tt{的长度}}}
(define list-length
  (lambda (lst)
    ...))
]
}

列表的集合定义为

@$${\mathit{List} ::= @tt{()} \mid @tt{(@${Scheme \; value}
. @${\mathit{List}})}}

因此，考虑列表的每种情况。若列表为空，则长度为0。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{list-length}} : @${\mathit{List} \to \mathit{Int}}}
@#,elem{@bold{用法} : @tt{(list-length @${l}) = @${l} 的长度}}
(define list-length
  (lambda (lst)
@#,exact-elem{\begin{mdframed}[style=codediff]}
    (if (null? lst)
        0
@#,exact-elem{\end{mdframed}}
        ...)))
]
}

若列表非空，则其长度比其余项长度多1。这就给出了完整定义。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{list-length}} : @${\mathit{List} \to \mathit{Int}}}
@#,elem{@bold{用法} : @tt{(list-length @${l}) = @${l} 的长度}}
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
@#,exact-elem{\begin{mdframed}[style=codediff]}
        (+ 1 (list-length (cdr lst))))))
@#,exact-elem{\end{mdframed}}
]
}

通过 @tt{list-length} 的定义，我们可以看到它的运算过程。

@eopl-code{
@verbatim|{
  (list-length '(a (b c) d))
= (+ 1 (list-length '((b c) d)))
= (+ 1 (+ 1 (list-length '(d))))
= (+ 1 (+ 1 (+ 1 (list-length '()))))
= (+ 1 (+ 1 (+ 1 0)))
= 3
}|
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{list-length}} "listlength"]]
}

@subsection[#:style section-title-style-numbered #:tag "s1.2.2"]{@tt{nth-element}}

@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{nth-element}} "nth-element"]]
标准的 Scheme 过程 @tt{list-ref} 取一列表 @tt{lst} 和从 0 开始计数的索引 @tt{n}，
返回 @tt{lst} 的第 @tt{n} 个元素。

@eopl-code{
@examples[#:label #f (list-ref '(a b c) 1)]}

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

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{nth-element}} : @${\mathit{List} \times \mathit{Int} \to \mathit{SchemeVal}}}
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
      "List too short by ~s elements.~%" (+ n 1))))
]
}

这里的注释 @tt{@bold{@tt{nth-element}} : @${\mathit{List} \times \mathit{Int}
\to \mathit{SchemeVal}}} 表示 @bold{@tt{nth-element}} 是一个过程，取两个参数，一
个为列表，一个为整数，返回一个Scheme 值。这与数学中的表示 @${f : A \times B \to
C} 相同。

@eopl-index[(eopl-index-entry @elem{@tt{eopl:error} procedure} "eoplerrorprocedure")]
@eopl-index[#:range-mark 'start "Error handling"]
@eopl-index[@eopl-index-entry[@elem{@tt{report-} procedures} "reportprocedure"]]
过程 @tt{report-list-too-short} 调用 @tt{eopl:error} 来报告错误，后者会终止计算。
它的首个参数是一符号，用于在错误信息中指示调用 @tt{eopl:error} 的过程。第二个参
数是一个字符串，会打印为错误信息。对应于字符串中的每个字符序列 @tt{~s}，都必须有
一个额外参数。打印字符串时，这些参数的值会替换对应的 @tt{~s} 。@tt{~%}代表换行。
错误信息打印后，计算终结。过程 @tt{eopl:error} 并非标准 Scheme 的一部分，但大多
数 Scheme 实现提供这样的组件。在本书中，我们以类似方式，用名字含 @tt{report-} 的
过程报告错误。
@eopl-index[#:range-mark 'end "Error handling"]

来看看 @tt{nth-element} 如何算出答案：

@nested{
@eopl-code{
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

如果排除错误检查，我们得靠 @tt{car} 和 @tt{cdr} 报错来获知传递了空列表，但它们的
错误信息无甚帮助。例如，当我们收到 @tt{car} 的错误信息，可能得找遍整个程序中使用
@tt{car} 的地方。
@eopl-index[#:range-mark 'end "Inductive specifications" "recursive procedures based on"]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{nth-element}} "nth-element"]]

@exercise[#:level 1 #:tag "ex1.6"]{
 如果调换 @tt{nth-element} 中两个检测的顺序，会有什么问题？

}

@exercise[#:level 2 #:tag "ex1.7"]{
 @tt{nth-element} 的错误信息不够详尽。重写 @tt{nth-element}，给出更详细的错误信
 息，像是 @exact-elem{“}@tt{(a b c)} 不足 8 个元素@exact-elem{”}。

}

@subsection[#:style section-title-style-numbered #:tag "s1.2.3"]{@tt{remove-first}}

@eopl-index["List of symbols (List-of-Symbol)"]
@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{remove-first}} "removefirst"]]
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

@eopl-code{
@examples[#:eval remove-first-eval
          #:label #f
          (remove-first 'a '(a b c))
          (remove-first 'b '(e f g))
          (remove-first 'a4 '(c1 a4 c1 a4))
          (remove-first 'x '())]}

写出此过程之前，我们先要定义符号列表集合
@${\mathit{List\mbox{-}of\mbox{-}Symbol}} ，以便给出问题的完整描述。不像上一节介
绍的 s-lists，符号列表不包含子列表。

@nested{
@$${\mathit{List\mbox{-}of\mbox{-}Symbol} ::= @tt{()} \mid
@tt{(@${\mathit{Symbol}} . @${\mathit{List\mbox{-}of\mbox{-}Symbol}})}}

符号列表或者是空列表，或者是首项为符号，余项为符号列表。}

如果列表为空，不需要移除 @${s}，则答案为空列表。

@eopl-code{
@racketblock[
@#,elem{@elemtag["remove-first"]{@bold{@tt{remove-first}}} : @${\mathit{Sym} \times \mathit{Listof}(\mathit{Sym}) \to \mathit{Listof}(\mathit{Sym})}}
@#,elem{@bold{用法} : @tt{(remove-first @${s} @${los}) 返回一列表，除了不含第一个出现在 @${los} 中的符号 @${s} 外，元素及其排列顺序与 @${los} 相同。}}
(define remove-first
  (lambda (s lst)
    (if (null? lst)
        '()
        ...)))
]
}

写合约时，我们用 @${\mathit{Listof}(\mathit{Sym})} 而不是
@${\mathit{List\mbox{-}of\mbox{-}Symbol}}。用这种写法可以免除许多上面那样的定义。

如果 @${los} 非空，有没有哪种情况可以立刻得出答案？如果 @${los} 的第一个元素是
@${s}，比如 @${los = @tt{(@${s} @${s_1} @${...} @${s_{n-1}})}}，@${s} 首次出现时
是 @${los} 的第一个元素，那么把它删除之后的结果是 @tt{(@${s_1} @${...}
@${s_{n-1}})}。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{remove-first}} : @${\mathit{Sym} \times \mathit{Listof}(\mathit{Sym}) \to \mathit{Listof}(\mathit{Sym})}}
(define remove-first
  (lambda (s lst)
    (if (null? lst)
        '()
@#,exact-elem{\begin{mdframed}[style=codediff]}
        (if (eqv? (car los) s)
            (cdr los)
            ...))))
@#,exact-elem{\end{mdframed}}
]
}

如果 @${los} 的第一个元素不是 @${s}，比如 @${los = @tt{(@${s_0} @${s_1} @${...}
@${s_{n-1}})}}，可知 @${s_0} 不是第一个出现的 @${s}，因此答案中的第一个元素一定
是@${s_0}，即表达式 @tt{(car los)} 的值。而且，@${los} 中的首个 @${s} 一定在
@tt{(@${s_1} @${...} @${s_{n-1}})} 中。所以答案的余下部分一定是移除 @${los} 余项
中首个 @${s} 的结果。因为 @${los} 的余项比 @${los} 短，我们可以递归调用
@tt{remove-first}，从 @${los} 的余项中移除 @${s}，即答案的余项可用
@tt{(remove-first s (cdr los))} 求得。已知如何找出答案的首项和余项，可以用
@tt{cons} 结合二者，通过表达式 @tt{(cons (car los) (remove-first s (cdr los)))}
求得整个答案。由此，@tt{remove-first} 的完整定义为

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{remove-first}} : @${\mathit{Sym} \times \mathit{Listof}(\mathit{Sym}) \to \mathit{Listof}(\mathit{Sym})}}
(define remove-first
  (lambda (s lst)
    (if (null? lst)
        '()
        (if (eqv? (car los) s)
            (cdr los)
@#,exact-elem{\begin{mdframed}[style=codediff]}
            (cons (car los) (remove-first s (cdr los)))))))
@#,exact-elem{\end{mdframed}}
]
@eopl-index[#:range-mark 'end @eopl-index-entry[@elem{List (@${\mathit{List}})} "Listlist"]]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{remove-first}} "removefirst"]]
}

@exercise[#:level 1 #:tag "ex1.8"]{

 如果把 @tt{remove-first} 定义中的最后一行改为 @tt{(remove-first s (cdr los))}，
 得到的过程做什么运算？对修改后的版本，给出合约，包括用法。

}

@exercise[#:level 2 #:tag "ex1.9"]{

 定义 @tt{remove}。它类似于 @tt{remove-first}，但会从符号列表中移除出现的所有给
 定符号，而不只是第一个。

}

@subsection[#:style section-title-style-numbered #:tag "s1.2.4"]{@tt{occurs-free?}}

@eopl-index[#:range-mark 'start "Binding" "lambda"]
@eopl-index[#:range-mark 'start "Lambda expression (LcExp)"]
@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{occurs-free?}} "occursfree"]]
过程 @tt{occurs-free?} 取一个变量 @${var}，由 Scheme 符号表示；一个 lambda 演算
表达式 @${exp}，形如@definition-ref{d1.1.8}；判断 @${var} 是否自由出现于 @${exp}。
如果一个变量出现于表达式 @${exp} 中，但不在某一 @tt{lambda} 绑定之内，我们说该变
量@term["occur free"]{自由出现} 于表达式 @${exp} 中。例如，
@eopl-index["Free occurrence of variable"]
@eopl-index["Variable(s)" "free"]

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

@eopl-code{
@examples[#:eval occurs-free?-eval
          #:label #f
          (occurs-free? 'x 'x)
          (occurs-free? 'x 'y)
          (occurs-free? 'x '(lambda (x) (x y)))
          (occurs-free? 'x '(lambda (y) (x y)))
          (occurs-free? 'x '((lambda (x) x) (x y)))
          (occurs-free? 'x '(lambda (y) (lambda (z) (x (y z)))))]}

我们可以遵照 lambda 演算表达式的语法解决此问题：

@nested[#:style normalfont]{
@envalign*{\mathit{LcExp} &::= @m{\mathit{Identifier}} \\
                          &::= @tt{(lambda (@m{\mathit{Identifier}}) @m{\mathit{LcExp}})} \\
                          &::= @tt{(@m{\mathit{LcExp}} @m{\mathit{LcExp}})}}
}

我们可以总结出规则的各种情况：

@itemlist[

 @item{若表达式 @${e} 是一变量，则当且仅当 @${x} 与 @${e} 相同时，变量 @${x} 自
 由出现于 @${e}。}

 @item{若表达式 @${e} 形如 @tt{(@${lambda} (@${y}) @${e'})}，则当且仅当 @${y} 不
 同于 @${x} 且 @${x} 自由出现于 @${e'} 时，变量 @${x} 自由出现于 @${e}。}

 @item{若表达式 @${e} 形如 @tt{(@${e_1} @${e_2})}，则当且仅当 @${x} 自由出现于
 @${e_1} 或 @${e_2} 时，@${x} 自由出现于 @${e}。这里的@exact-elem{“}或
 @exact-elem{”}表示@term["inclusive or"]{涵盖或}，意为它包含 @${x} 同时自由出现
 于 @${e_1} 和 @${e_2} 的情况。我们通常用@exact-elem{“}或@exact-elem{”}表示这
 种意思。
 @eopl-index["Inclusive or"]
 @eopl-index["Or"]}

]

你可以说服自己，这些规则涵盖了@exact-elem{“}@${x} 不在某一 lambda 绑定之中
@exact-elem{”}表示的所有意思。
@eopl-index[#:range-mark 'end "Lambda expression (LcExp)"]

@exercise[#:level 1 #:tag "ex1.10"]{

 我们常用@exact-elem{“}或@exact-elem{”}表示@exact-elem{“}涵盖或
 @exact-elem{”}。@exact-elem{“}或@exact-elem{”}还有什么含义？@linebreak[]

}

然后，定义 @tt{occurs-free?} 就很容易了。因为有三种情况要检查，我们不用 Scheme
的 @tt{if}，而是用 @tt{cond}。在 Scheme 中，若 @${exp_1} 或 @${exp_2} 返回真值，
则 @tt{(or @${exp_1} @${exp_2})} 返回真值。

@eopl-code{
@racketblock[
@#,elem{@elemtag["occurs-free-1?"]{@bold{@tt{occurs-free?}}} : @${\mathit{Sym} \times \mathit{LcExp} \to \mathit{Bool}}}
@#,elem{@bold{用法} : @tt{若符号 @${var} 自由出现于 @${exp}，返回 #t，否则返回 #f}}
(define occurs-free?
  (lambda (var exp)
    (cond
      ((symbol? exp) (eqv? var exp))
      ((eqv? (car exp) 'lambda)
        (and
          (not (eqv? var (car (cadr exp))))
          (occurs-free? var (caddr exp))))
      (else
        (or
          (occurs-free? var (car exp))
          (occurs-free? var (cadr exp)))))))
]
}

这一过程略显晦涩。比如，很难弄明白 @tt{(car (cadr exp))} 指代 @tt{lambda} 表达式
中的变量声明，或者 @tt{(caddr exp)} 指代 @tt{lambda} 表达式的主体。在
@secref{s2.5}，我们展示如何显著改善这种情况。
@eopl-index[#:range-mark 'end "Binding" "lambda"]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{occurs-free?}} "occursfree"]]

@subsection[#:style section-title-style-numbered #:tag "s1.2.5"]{@tt{subst}}

@eopl-index[#:range-mark 'start "Mutual recursion"]
@eopl-index[#:range-mark 'start "Nonterminal symbols"]
@eopl-index[#:range-mark 'start "Recursive programs" "mutual recursion"]
@eopl-index[#:range-mark 'start @eopl-index-entry[@elem{S-exp (@${\mathit{S\mbox{-}exp}})} "Sexp"]]
@eopl-index[#:range-mark 'start @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{subst}} "subst"]]
@eopl-index[#:range-mark 'start "Substitution" "in s-lists"]
@eopl-index[#:range-mark 'start "Syntactic categories"]
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

@eopl-code{
@examples[#:eval subst-eval
          #:label #f
          (subst 'a 'b '((b c) (b () d)))]}

因为 @tt{subst} 定义于 s-list 上，它的结构应当反映 s-list 的定义（@definition-ref{d1.1.6}）：

@nested[#:style normalfont]{
@envalign*{\mathit{S\mbox{-}list} &::= @tt{(@m{\{\mathit{S\mbox{-}exp}\}^*})} \\
           \mathit{S\mbox{-}list} &::= \mathit{Symbol} \mid \mathit{S\mbox{-}list}}
}

克莱尼星号简洁地描述了集合 s-list，但对写程序没什么用。因此我们的第一步是抛开克
莱尼星号重写语法。得出的语法表明，我们的过程应当该递归处理 s-list 的首项和余项。

@nested[#:style normalfont]{
@envalign*{\mathit{S\mbox{-}list} &::= @tt{()} \\
                                  &::= @tt{(@m{\mathit{S\mbox{-}exp}} . @m{\mathit{S\mbox{-}list}})} \\
            \mathit{S\mbox{-}exp} &::= \mathit{Symbol} \mid \mathit{S\mbox{-}list}}
}

这一例子比之前的复杂，因为它的语法输入包含两个非终结符，@${S\mbox{-}list} 和
@${S\mbox{-}exp}。因此，我们需要两个过程，一个处理 @${S\mbox{-}list}，另一个处理
@${S\mbox{-}exp}。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{subst}} : @m{\mathit{Sym} \times \mathit{Sym} \times \mathit{S\mbox{-}list} \to \mathit{S\mbox{-}list}}}
(define subst
  (lambda (new old slist)
    ...))

@#,elem{@bold{@tt{subst-in-s-exp}} : @m{\mathit{Sym} \times \mathit{Sym} \times \mathit{S\mbox{-}exp} \to \mathit{S\mbox{-}exp}}}
(define subst-in-s-exp
  (lambda (new old sexp)
    ...))
]}

我们首先处理 @tt{subst}。如果列表为空，不需要替换 @tt{old}。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{subst}} : @m{\mathit{Sym} \times \mathit{Sym} \times \mathit{S\mbox{-}list} \to \mathit{S\mbox{-}list}}}
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
        ...)))
]
}

如果 @tt{slist} 非空，它的首项是一个 @${S\mbox{-}exp}，余项是另一 s-list。这时，
答案应当是一个列表，它的首项是把 @tt{slist} 首项中的 @tt{old} 替换为 @tt{new} 的
结果，它的余项是把 @tt{slist} 余项中的 @tt{old} 替换为 @tt{new} 的结果。因为
@tt{slist} 的首项是 @${S\mbox{-}exp} 的元素，我们用 @tt{subst-in-s-exp}解决这一
子问题。因为 @tt{slist} 的余项是 @${S\mbox{-}list} 的元素，我们递归调用
@tt{subst} 处理它。

@eopl-code{
@racketblock[
@#,elem{@elemtag["subst"]{@bold{@tt{subst}}} : @m{\mathit{Sym} \times \mathit{Sym} \times \mathit{S\mbox{-}list} \to \mathit{S\mbox{-}list}}}
(define subst
  (lambda (new old slist)
    (if (null? slist)
        '()
@#,exact-elem{\begin{mdframed}[style=codediff]}
        (cons
         (subst-in-s-exp new old (car slist))
         (subst new old (cdr slist))))))
@#,exact-elem{\end{mdframed}}
]
}

现在来处理 @tt{subst-in-s-exp}。由语法，可知符号表达式 @tt{sexp} 或者是符号，或
者是 s-list。如果它是符号，那么得检查它与符号 @tt{old} 是否相同。如果是，答案为
@tt{new}；否则，答案还是 @tt{sexp}。如果 @tt{sexp} 是一个 s-list，那么我们递归调
用 @tt{subst} 找出答案。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{subst-in-s-exp}} : @m{\mathit{Sym} \times \mathit{Sym} \times \mathit{S\mbox{-}exp} \to \mathit{S\mbox{-}exp}}}
(define subst-in-s-exp
  (lambda (new old sexp)
@#,exact-elem{\begin{mdframed}[style=codediff]}
    (if (symbol? sexp)
        (if (eqv? sexp old) new sexp)
        (subst new old sexp))))
@#,exact-elem{\end{mdframed}}
]
}

因为我们严格依照 @${\mathit{S\mbox{-}list}} 和 @${\mathit{S\mbox{-}exp}} 的定义，
这个递归一定会终止。因为 @tt{subst} 和 @tt{subst-in-s-exp} 递归调用彼此，我们称
之为@term["mutually recursive"]{互递归}。
@eopl-index[#:range-mark 'end "Recursive programs" "mutual recursion"]

把 @tt{subst} 拆解为两个过程——每个处理一种句法类别——是个重要技巧。对更为复杂的程
序，我们得以每次考虑一个句法类别，从而化繁为简。
@eopl-index[#:range-mark 'end "Follow the Grammar" "examples of"]
@eopl-index[#:range-mark 'end "Mutual recursion"]
@eopl-index[#:range-mark 'end "Nonterminal symbols"]
@eopl-index[#:range-mark 'end "Recursive programs" "examples of"]
@eopl-index[#:range-mark 'end @eopl-index-entry[@elem{S-exp (@${\mathit{S\mbox{-}exp}})} "Sexp"]]
@eopl-index[#:range-mark 'end @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{subst}} "subst"]]
@eopl-index[#:range-mark 'end "Substitution" "in s-lists"]
@eopl-index[#:range-mark 'end "Syntactic categories"]

@exercise[#:level 1 #:tag "ex1.11"]{

 @tt{subst-in-s-exp} 的最后一行中，递归是针对 @tt{sexp} 而非更小的子结构，为什
 么一定能终止？

}

@exercise[#:level 1 #:tag "ex1.12"]{

 @eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex1.12"] "Inlining"]
 用 @tt{subst-in-s-exp} 的定义替换 @tt{subst} 中的调用，从而排除这次调用，然后简
 化得到的过程。结果中的 @tt{subst} 应当不需要 @tt{subst-in-s-exp}。这种技巧
 叫做@term["inlining"]{内联}，用于优化编译器。
 @eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex1.12"] "Inlining"]

}

@exercise[#:level 2 #:tag "ex1.13"]{

 在我们的例子中，我们从排除 @m{S\mbox{-}list} 语法内的克莱尼星号开始。依照原本的
 语法，用 @tt{map} 重写 @tt{subst}。@linebreak[]

}

@eopl-index[#:range-mark 'start "Recursive programs" "deriving"]
@eopl-index[#:range-mark 'start "Syntactic categories"]
现在，我们有了编写过程处理归纳数据集的窍门，来把它总结成一句口诀。

@nested[#:style tip]{
 @centered{@bold{遵循语法！}@eopl-index["Follow the Grammar"]}

 @para[#:style tip-content]{定义过程处理归纳式数据时，程序的结构应当反映数据的结
 构。}}

更准确地说：

@itemlist[

 @item{@eopl-index[#:range-mark 'start "Nonterminal symbols"]
 为语法中的每个非终结符编写一个过程。这一过程负责处理相应非终结符的数据，
 不做其他。}

 @item{在每个过程中，为相应非终结符的每一生成式写一分支。你可能需要额外的分支结
 构，但这样才能起步。对生成式右边出现的每个非终结符，递归调用相应的过程。
 @eopl-index[#:range-mark 'end "Nonterminal symbols"]
 @eopl-index[#:range-mark 'end "Recursive programs" "deriving"]
 @eopl-index[#:range-mark 'end "Syntactic categories"]}

]

@section[#:style section-title-style-numbered #:tag "s1.3"]{辅助过程和上下文参数}

@eopl-index[#:range-mark 'start "Auxiliary procedures"]
@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{number-elements}} "number-elements"]]
@eopl-index[#:range-mark 'start "Recursive data types" "programs that manipulate"]
窍门@term[#f]{遵循语法}很有效，有时却还是不够。考虑过程 @tt{number-elements}。这
一过程取任何列表 @tt{(@${v_0} @${v_1} @${v_2} ...)} ，返回一列表 @tt{((0
@${v_0}) (1 @${v_1}) (2 @${v_2}) ...)}。

我们用过的那种直拆法不凑效，因为没有明显的方法能从 @tt{(number-elements (cdr
lst))} 得出 @tt{(number-elements lst)} （但是，看看@exercise-ref{ex1.36}）。

@eopl-index[#:range-mark 'start "Generalization"]
要解决这个问题，我们@term["generalize"]{放宽} 问题。我们写一个过程
@tt{number-elements-from} ，它取一个额外参数 @${n}，指定起始编号。用递归处理列表，
这个过程很容易写。
@eopl-index[#:range-mark 'end "Generalization"]

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{number-elements-from}} : @m{\mathit{Listof}(\mathit{SchemeVal}) \times \mathit{Int} \to \mathit{Listof}(\mathit{List}(\mathit{Int}, \mathit{SchemeVal}))}}
@#,elem{@${\begin{alignedat}{-1}@bold{用法} : &@tt{(number-elements-from '(@${v_0} @${v_1} @${v_2} ...) n)} \\ &\hphantom{x}= @tt{((@${n} @${v_0}) (@${n + 1} @${v_1}) (@${n + 2} @${v_2}) ...)}\end{alignedat}}}
(define number-elements-from
  (lambda (lst n)
    (if (null? lst) '()
        (cons
         (list n (car lst))
         (number-elements-from (cdr lst) (+ n 1))))))
]
}

合约的标题告诉我们这个过程取两个参数，一个列表（包含任意 Scheme 值）和一个整数，
返回一个列表，列表中的每个元素是包含两个元素的列表：一个整数，一个 Scheme 值。

一旦我们定义了 @tt{number-elements-from}，很容易写出所需的过程。

@eopl-code{
@racketblock[
@#,elem{@elemtag["n-e"]{@bold{@tt{number-elements}}} : @m{\mathit{Listof}(\mathit{SchemeVal}) \to \mathit{Listof}(\mathit{List}(\mathit{Int}, \mathit{SchemeVal}))}}
(define number-elements
  (lambda (lst n)
    (number-elements-from lst 0)))
]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{number-elements}} "number-elements"]]
}

这里有两个要点。首先，过程 @tt{number-elements-from} 的定义独立于
@tt{number-elements}。程序员经常要写一些过程，只调用一些传递额外常量参数的辅助过
程。除非我们理解辅助过程对参数的@emph{每个}值做什么，我们很难理解调用它的过程做
什么。这给了我们一条口诀：

@nested[#:style tip]{
 @centered{@elemtag["no-myth"]{@bold{避免神秘小工具！}}
 @eopl-index["No Mysterious Auxiliaries"]}

 @para[#:style tip-content]{定义辅助过程时，总是指明它对所有参数值做什么，而不只
 是初始值。}}

@eopl-index[#:range-mark 'start "Context argument"]
其次，@tt{number-elements-from} 的两个参数各有作用。第一个参数是我们要处理的列表，
随每一次递归调用而减小。而第二个参数，则是对我们当前任务@term["context"]{上下文}
的抽象。在本例中，当调用 @tt{number-elements} 时，我们最终调用
@tt{number-elements-from} 处理原列表的每个子列表。第二个参数告知我们子列表在原列
表中的位置。随递归调用，它不减反增，因为我们每次经过原列表的一个元素。有时我们称
之为@term["context argument"]{上下文参数}，或者@term["inherited attribute"]{继承
属性}。
@eopl-index[#:range-mark 'end "Context argument"]

另一个例子是向量求和。

要求列表中各项的和，我们可以遵循语法，递归处理列表的余项。那么我们的过程看起来像
是：

@eopl-code{
@eopl-index[@eopl-index-entry[@bold{@tt{list-sum}} "listsum"]]
@racketblock[
@#,elem{@elemtag["list-sum"]{@bold{@tt{list-sum}}} : @m{\mathit{Listof}(\mathit{Int}) \to \mathit{Int}}}
(define list-sum
  (lambda (loi)
    (if (null? loi)
        0
        (+ (car loi)
           (list-sum (cdr loi))))))
]
}

但是无法按照这种方式处理向量，因为它们不能够很方便地拆解。

@eopl-index[#:range-mark 'start "Generalization"]
因为我们无法拆解向量，我们放宽问题，为向量某一部分求和。问题定义为，计算
@nested{
@$${\sum_{i=0}^{i=length(v)-1} v_i}
其中，@${v} 是整数向量。通过把上界改为一个参数 @${n}，我们放宽了原问题，所以新的
任务是计算
@$${\sum_{i=0}^{i=n} v_i}
其中，@${0 \leq n < length(v)}。
@eopl-index[#:range-mark 'end "Generalization"]
}

@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{partial-vector-sum}} "partialvectorsum"]]
@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{vector-sum}} "vectorsum"]]
按照定义，用归纳法处理第二个参数 @${n}，可以直接写出此过程。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{partial-vector-sum}} : @${\mathit{Vectorof}(\mathit{Int}) \times \mathit{Int} \to \mathit{Int}}}
@#,elem{@bold{用法} : @tt{若 @${0 \leq n < length(v)}，则 @mp{@tt{(partial-vector-sum @m{v} @m{n}) = @m{\sum_{i=0}^{i=n} v_i}}}}}
(define partial-vector-sum
  (lambda (v n)
    (if (zero? n)
        (vector-ref v 0)
        (+ (vector-ref v n)
           (partial-vector-sum v (- n 1))))))
]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{partial-vector-sum}} "partialvectorsum"]]}

由于 @${n} 一定会递减至零，证明此程序的正确性需要用归纳法处理 @${n}。由 @${0
\leq n} 且 @${n \neq 0}，可得 @${0 \leq (n - 1)}，所以递归调用过程
@tt{partial-vector-sum} 仍然满足其合约。

现在，要解决原问题就简单多了。因为向量长度为0时无法使用过程
@tt{partial-vector-sum}，所以得另行处理这种情况。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{vector-sum}} : @m{\mathit{Vectorof}(\mathit{Int}) \to \mathit{Int}}}
@#,elem{@bold{用法} : @tt{(vector-sum @m{v}) = @m{\sum\limits_{i=0}^{i=length(v)-1} v_i}}}
(define vector-sum
  (lambda (v)
    (let ((n (vector-length v)))
      (if (zero? n)
          0
          (partial-vector-sum v (- n 1))))))
]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{vector-sum}} "vectorsum"]]}

还有许多情况下，引入辅助变量或过程来解决问题会有帮助，甚至必不可少。只要能对新过
程做什么给出独立的定义，尽可以如此。
@eopl-index[#:range-mark 'end "Auxiliary procedures"]
@eopl-index[#:range-mark 'end "Recursive data types" "programs that manipulate"]

@exercise[#:level 2 #:tag "ex1.14"]{

 @eopl-index[#:suffix @exer-ref-range["ex1.14"] "Induction, proof by"]
 若 @m{0 \leq n < length(v)}，证明 @tt{partial-vector-sum} 的正确性。

}

@section[#:style section-title-style-numbered #:tag "s1.4"]{练习}

@eopl-index[#:range-mark 'start "Recursive programs" "examples of"]
学写递归程序需要练习，那么我们拿几道习题结束本章。

每道习题都假定 @tt{s} 是一个符号，@tt{n} 是一个非负整数，@tt{lst} 是一个列表，
@tt{loi} 是一个整数列表，@tt{los} 是一个符号列表，@tt{slist} 是一个 s-list，
@tt{x} 是任意 Scheme 值；类似地，@tt{s1} 是一个符号，@tt{los2} 是一个符号列表，
@tt{x1} 是一个 Scheme 值，等等。还假定 @tt{pred} 是一个@term["predicate"]{谓词}，
即一个过程，取任意 Scheme 值，返回 @tt{#t} 或者 @tt{#f}。除非某个具体问题另有限
制，不要对数据作其他假设。在这些习题中，不需要检查输入是否符合描述；对每个过程，
都假定输入值是指定集合的成员。

定义，测试和调试每个过程。你的定义应当有本章这种合约和用法注释。可以随便定义辅助
过程，但是你定义的每个辅助过程都应该有其说明，如同@secref{s1.3}那样。

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
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex1.15" "ex1.27"] @eopl-index-entry[@elem{List (@${\mathit{List}})} "Listlist"]]
 @tt{(duple n x)} 返回包含 @tt{n} 个 @tt{x} 的列表。

@eopl-code{
@examples[#:eval duple-eval
           #:label #f
           (duple 2 3)
           (duple 4 '(ha ha))
           (duple 0 '(blah))]}
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

@eopl-code{
@examples[#:eval invert-eval
           #:label #f
 (invert '((a 1) (a 2) (1 b) (2 b)))]}
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

@eopl-code{
@examples[#:eval down-eval
          #:label #f
 (down '(1 2 3))
 (down '((a) (fine) (idea)))
 (down '(a (more (complicated)) object))]}

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
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex1.18"] @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
@tt{(swapper s1 s2 slist)} 返回一列表，将 @tt{slist} 中出现的所有 @tt{s1} 替换为
@tt{s2}，所有 @tt{s2} 替换为 @tt{s1}。

@eopl-code{
@examples[#:eval swapper-eval
           #:label #f
 (swapper 'a 'd '(a b c d))
 (swapper 'a 'd '(a d () c d))
 (swapper 'x 'y '((x) y (z (x))))]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex1.18"] @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
}}

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

@eopl-code{
@examples[#:eval list-set-eval
           #:label #f
 (list-set '(a b c d) 2 '(1 2))
 (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)]}

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

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex1.20"] @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
@tt{(count-occurrences s slist)} 返回 @tt{slist} 中出现的 @tt{s} 个数。

@eopl-code{
@examples[#:eval count-occurrences-eval
           #:label #f
 (count-occurrences 'x '((f x) y (((x z) x))))
 (count-occurrences 'x '((f x) y (((x z) () x))))
 (count-occurrences 'w '((f x) y (((x z) x))))]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex1.20"] @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
}}

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

@eopl-code{
@examples[#:eval product-eval
           #:label #f
 (product '(a b c) '(x y))]}

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

@eopl-code{
@examples[#:eval filter-in-eval
           #:label #f
 (filter-in number? '(a 2 (1 3) b 7))
 (filter-in symbol?  '(a (b c) 17 foo))]}

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

@eopl-code{
@examples[#:eval list-index-eval
          #:label #f
 (list-index number? '(a 2 (1 3) b 7))
 (list-index symbol?  '(a (b c) 17 foo))
 (list-index symbol?  '(1 2 (a b) 3))]}

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

@eopl-code{
@examples[#:eval every?-eval
           #:label #f
 (every? number? '(a b c 3 e))
 (every? number? '(1 2 3 4 5))]}

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

@eopl-code{
@examples[#:eval exists?-eval
           #:label #f
 (exists? number? '(a b c 3 e))
 (exists? number? '(a b c d e))]}

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
 (up lst))} 不一定是 @tt{lst}（参见@exercise-ref{ex1.17}）。

@eopl-code{
@examples[#:eval up-eval
           #:label #f
 (up '((1 2) (3 4)))
 (up '((x (y)) z))]}

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
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex1.27"] @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
@tt{(flatten slist)} 返回一列表，由 @tt{slist} 中的符号按出现顺序组成。直观上，
@tt{flatten} 移除参数内的所有内层括号。

@eopl-code{
@examples[#:eval flatten-eval
           #:label #f
 (flatten '(a b c))
 (flatten '((a) () (b ()) () (c)))
 (flatten '((a b) c (((d)) e)))
 (flatten '(a b (() (c))))]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex1.15" "ex1.27"] @eopl-index-entry[@elem{List (@${\mathit{List}})} "Listlist"]]}
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex1.27"] @eopl-index-entry[@elem{S-list (@${\mathit{S\mbox{-}list}})} "Slist"]]
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
 @eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex1.28" "ex1.29" "ex1.30"]
             @eopl-index-entry[@elem{List of integers (@List-of-Int-$[])} "Listofintegers"]]
 @tt{loi1} 和 @tt{loi2} 是元素按照升序排列的整数列表，@tt{(merge loi1 loi2)} 返
 回 @tt{loi1} 和 @tt{loi2} 中所有整数组成的的有序列表。

@eopl-code{
@examples[#:eval merge-eval
           #:label #f
 (merge '(1 4) '(1 2 8))
 (merge '(35 62 81 90 91) '(3 83 85 90))]}

}

@exercise[#:level 2 #:tag "ex1.29"]{
 @tt{(sort loi)} 返回一列表，将 @tt{loi} 中的元素按照升序排列。

@eopl-code{
@examples[#:eval merge-eval
           #:label #f
 (sort '(8 2 5 2 3))]}

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

@eopl-code{
@examples[#:eval sort/predicate-eval
           #:label #f
 (sort/predicate < '(8 2 5 2 3))
 (sort/predicate > '(8 2 5 2 3))]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex1.28" "ex1.29" "ex1.30"]
            @eopl-index-entry[@elem{List of integers (@List-of-Int-$[])} "Listofintegers"]]}

}

@exercise[#:level 1 #:tag "ex1.31"]{
 @eopl-index[#:suffix @exer-ref-range["ex1.31" "ex1.33"] (eopl-index-entry @elem{Binary tree (@${\mathit{Bintree}})} "Binarytree")]
 写出如下过程，对二叉树（@definition-ref{d1.1.7}）做运算：@tt{leaf} 和 @tt{interior-node}
 生成二叉树，@tt{leaf?} 检查二叉树是否是一片叶子，@tt{lson}、@tt{rson}和
 @tt{contents-of} 取出一个节点的各部分。@tt{contents-of} 应对叶子和内部节点都适
 用。

}

@exercise[#:level 1 #:tag "ex1.32"]{
 写出过程 @tt{double-tree}，它取一棵二叉树，形如@definition-ref{d1.1.7}，生成另一棵二叉树，把
 原二叉树中的所有整数翻倍。

}

@exercise[#:level 2 #:tag "ex1.33"]{
 @eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex1.33"]
             @eopl-index-entry[@elem{Red-blue trees (@${\mathit{Red\mbox{-}blue\mbox{-}tree}})} "Redbluetrees"]]
 写出过程 @tt{mark-leaves-with-red-depth}，它取一棵二叉树（@definition-ref{d1.1.7}），生成与
 原树形状相同的另一棵二叉树，但在新的二叉树中，每个叶子中的整数表示它和树根之间
 含有 @tt{red} 符号的节点数。例如，表达式

 @eopl-code{
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

 使用@exercise-ref{ex1.31} 中定义的过程，应返回二叉树

 @eopl-code{
 @codeblock{
 (red
  (bar 1 1)
  (red 2 (quux 2 2)))
 }
 @eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex1.33"]
             @eopl-index-entry[@elem{Red-blue trees (@${\mathit{Red\mbox{-}blue\mbox{-}tree}})} "Redbluetrees"]]}

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

 @eopl-index[#:suffix @exer-ref-range["ex1.34"] (eopl-index-entry @elem{Binary search tree (@${\mathit{Binary\mbox{-}search\mbox{-}tree}})} "Binarysearchtree")]
 写出过程 @tt{path}，它取一个整数 @tt{n} 和一棵含有整数 @tt{n} 的二叉搜索树
 （@pageref{bst}）@tt{bst}，返回由 @tt{left} 和 @tt{right} 组成的列表，表示如何
 找到包含 @tt{n} 的节点。如果在树根处发现 @tt{n}，它返回空列表。

@eopl-code{
@examples[#:eval path-eval
           #:label #f
 (path 17 '(14 (7 () (12 () ()))
               (26 (20 (17 () ())
                       ())
                   (31 () ()))))]}

}

@exercise[#:level 3 #:tag "ex1.35"]{
 @eopl-index[#:suffix @exer-ref-range["ex1.35"] (eopl-index-entry @elem{Binary tree (@${\mathit{Bintree}})} "Binarytree")]
 写出过程 @tt{number-leaves}，它取一棵二叉树，生成与原树形状相同的二叉树，但叶子
 的内容从 0 开始计的整数。例如，

 @eopl-code{
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

 @eopl-code{
 @codeblock{
 (foo
  (bar 0 1)
  (baz
   2
   (quux 3 4)))
 }}

}

@exercise[#:level 3 #:tag "ex1.36"]{
 @eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex1.36"] @eopl-index-entry[@bold{@tt{number-elements}} "number-elements"]]
 写出过程 @tt{g}，则@pageref{n-e}的 @tt{number-elements} 可以定义为：

 @eopl-code{
 @codeblock{
 (define number-elements
   (lambda (lst)
     (if (null? lst) '()
         (g (list 0 (car lst))
            (number-elements (cdr lst))))))
 }
 @eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex1.36"] @eopl-index-entry[@bold{@tt{number-elements}} "number-elements"]]
 @eopl-index[#:range-mark 'end "Recursive programs" "examples of"]}
}
