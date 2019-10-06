#lang scribble/book
@(require "style.rkt")
@(require latex-utils/scribble/theorem)
@(require scribble/manual)
@(require scribble-math)
@(define (List-of-Int) ($ "List\\mbox{-}of\\mbox{-}Int"))

@title[#:style 'numbered #:tag "isd"]{归纳式数据集}

解释器，检查器以及类似程序是程序语言处理器的核心，本章介绍编写它们的基本编程工
具。

因为程序语言的语法通常为嵌套或者树状结构，递归将是我们的主要技巧。@secref{rd}和
1.2节介绍归纳式定义数据结构的方法，并展示此种定义如何用来指导递归程序的编写。1.3
节展示如何将这些技巧推广到更为复杂的程序。本章以大量练习作结。这些练习是本章的核
心。欲掌握本书余下部分依赖的递归编程技巧，得自它们的经验不可或缺。

@section[#:tag "rd"]{递推定义的数据}

编写函数代码时，我们必须明确知道什么样的值能作为函数的参数，什么样的值是函数的合
法返回值。通常这些值的集合很复杂。本节介绍定义值集合的形式化技术。

@subsection[#:tag "is"]{归纳式定义法}

归纳式定义法是定义值集合的有力方法。为解释这一方法，我们用它来描述自然数 @${N =
{0,1,2,...}} 的某一子集@${S}。

@; definition: (def #:title title #:tag tag)
@; @def {

 自然数@${n}属于@${S}，当且仅当：

 @itemlist[#:style 'ordered

  @item{@${{n = 0}}，或}

  @item{@${n - 3 \in S}。

 }

]

@; }

让我们看看如何用这一定义判断哪些自然数属于@${S}。我们知道@${0 \in S}。因此@${3
\in S}，因为@${(3 - 3) = 0}，而@${0 \in S}。同样地，@${6 \in S}，因为@${(6 -
3) = 3}，而@${3 \in S}。如此继续，我们可以得出结论，所有@${3}的整数倍都属于
@${S}。

其他自然数呢？@${1 \in S}吗？我们知道@${1 \ne 0}，所以第一个条件不满足。而且，
@${(1 - 3) = -2}，不是自然数，故不是@${S}的元素，因此第二个条件不满足。因为@${1}
不满足任一条件，所以@${1 \notin S}。同样地，@${2 \notin S}。@${4}呢？仅当@${1
\in S}时@${4 \in S}。但@${1 \notin S}，所以@${4 \notin S}。同样地，我们可以得出
结论，如果@${n}是自然数且不是@${3}的整数倍，则@${n \notin S}。

据此推论，可得@${S}是@${3}的整数倍自然数集合。

可以用该定义编写一个函数，判断一个自然数@${n}是否属于@${S}。


@; codeblock with contracts and usage
@codeblock{
@; contracts
in-S? : N -> Bool
@; usage
用法 : (in-S? n) = #t 若 n 属于 S, 否则 #f
(define in-S?
  (lambda (n)
    (if (zero? n) #t
        (if (>= (- n 3) 0)
            (in-S? (- n 3))
            #f))))
}
@;

此处根据定义，我们用Scheme编写了一个递归函数。符号 @racket[in-S? : N -> Bool] @;contract
是一条注释，称为该函数的@emph{合约} 。它表示
@racket[in-S?] 应为一函数，取一自然数，产生一布尔值。这样的注释对阅读和编写代码
很有帮助。

要确定是否@${n \in S}，我们首先发问是否@${n = 0}。如果是，那么答案为真。否则，我
们需要查看是否@${n - 3 \in S}。欲知此，我们首先判断是否@${(n - 3) \geqslant 0}。
如果是，那么可以用我们的函数来查看它是否属于@${S}。如果不是，那么@${n}不可能属于
@${S}。

这里是定义@${S}的另一种方式。

@; @def {

 定义集合@${S}为@${N}所包含的最小集合，满足如下两条性质：

 @itemlist[#:style 'ordered

  @item{@${0 \in S}，且}

  @item{若@${n \in S}，则@${n + 3 \in S}。}

 ]

@; }

“最小集合”是指该集合满足性质1和2，并且是其他任何满足性质1和2的集合的子集。易知
只能有一个这样的集合：如果@${S_1}和@${S_2}都满足性质1和2，并且都为最小，那么
@${S_1 \subseteq S_2}（因为@${S_1}最小）并且@${S_2 \subseteq S_1}（因为@${S_2}最
小），因此@${S_1 = S_2}。我们需要这一额外条件，因为否则的话将有许多集合满足其他
两个条件（见练习1.3）。

这里是另一种表示定义的方式：

@; infer should be implemented in scribble rather than use \infer directly
@; infer: (infer conclusion hypothesis ...)
@; @infer[${0 \in S}]
@$${\infer{0 \in S}{}}

@; @infer[${(n + 3) \in S}]{$@{n \in S}}
@$${\infer{(n + 3) \in S}{n \in S}}

这只是前一种定义的简便表示。每个条目称为一条@emph{推理规则}，或称@emph{规则}；水
平线读作“若-则”。线上部分称作@emph{假设} 或者@emph{前件} ；线下部分称作@emph{结
论} 或者@emph{后件}。要罗列两个或更多假设，用“和”连接（见定义1.1.5）。没有假设
的规则称作@emph{公理}。我们写公理时通常不加水平线，如：

@$${0 \in S}

该规则意为，自然数@${n}属于@${S}，当且仅当能用有限次推理规则，从公理推得陈述
“@${n \in S}”。这一解释自动使@${S}成为闭合于规则的最小集合。

这些定义说的都是同一件事。我们把第一种版本称作@emph{自顶向下} 的定义，第二种称作
@emph{自底向上} 的定义，第三种称作@emph{推理规则}定义。

让我们看些例子，如何使用这些规则。

@; @def[ #:title
（整数列表，自顶向下）
@; ]
@; {
 Scheme列表是整数列表，当且仅当

 @itemlist[#:style 'ordered

  @item{列表为空，或}

  @item{列表为序对，首项为整数，余项为整数列表。}

]

@; }

我们用@${Int}表示所有整数的集合，用@List-of-Int[]表示所有整数列表
的集合。

@; @def[ #:title
（整数列表，自底向上）
@; ]
@; {

 集合@List-of-Int[]是满足如下两条性质的最小Scheme列表集合：

 @itemlist[#:style 'ordered

  @item{@${() \in @List-of-Int{}}，或}

  @item{若@${n \in Int}且@${l \in @List-of-Int{}}，则 @${(n . l) \in
        @List-of-Int{}}。}

 ]

@; }


这里，我们用中缀“.”代表Scheme中 @racket[cons] 操作的结果。式子@${(n . l)}代表
Scheme序对的首项为@${n}，余项为@${l}。

@; @def[ #:title
（整数列表，推理规则）
@; ]
@; {

 @$${\infer{() \in @List-of-Int{}}{}}

 @$${\infer{(n . l) \in @List-of-Int{}}{n \in Int & l \in @List-of-Int{}}}

@; }

这三个定义等价。来看看如何用它们生成一些@List-of-Int[]的元素。

@itemlist[#:style 'ordered

 @item{由定义1.1.4，性质1，或定义1.1.5，规则1，@tt{()}是整数列表。}

 @item{由定义1.1.4，性质2，@tt{(14 . ())}是整数列表。因为@tt{14}是整数，@tt{()}
       是整数列表。写成@List-of-Int[]第二规则的实例，就是

       @$${\infer{@tt{(14 . ())} \in @List-of-Int{}} {@tt{14} \in Int & @tt{()}
          \in @List-of-Int{}}} }

 @item{由性质2，@tt{(3 . (14 . ()))}是整数列表。因为 @tt{3} 是整数，@tt{(14
        . ())}是整数列表。仍写成@List-of-Int[]的第二规则实例，是

       @$${\infer{@tt{(3 . (14 . ()))} \in @List-of-Int{}} {@tt{3} \in Int &
         @tt{(14 . ())} \in @List-of-Int{}}} }

 @item{由性质2，@tt{(-7 . (3 . (14 . ())))}是整数列表。因为@tt{-7}是整数，@tt{(3
       . (14 . ()))}是整数列表。再次写成@List-of-Int[]的第二规则实例，是

       @$${\infer{@tt{(-7 . (3 . (14 . ())))} \in @List-of-Int{}} {@tt{-7} \in
          Int & @tt{(3 . (14 . ()))}\in @List-of-Int{}}} }

 @item{不按照这种方式得到的都不是整数列表。}

]

改点示法为列表法，可知 @tt{()}、 @tt{(14)}、 @tt{(3 14)} 以及 @tt{(-7 3 14)} 都
是@List-of-Int[]的成员。

欲证明@${@tt{(-7 . (3 . (14 . ())))} \in @List-of-Int{}}，还可以结
合各条规则，得到链式推理的全貌。下面的树状图叫做@emph{推理}或者@emph{推理树}。

@$${\infer{@tt{(-7 . (3 . (14 . ())))} \in @List-of-Int{}}
          {@tt{-7} \in N &
           \infer{@tt{(3 . (14 . ()))} \in @List-of-Int{}}
                 {@tt{3} \in N & \infer{@tt{(14 . ())} \in @List-of-Int{}}
                                       {@tt{14} \in N & @tt{()} \in @List-of-Int{}}}
          }}

@; exercise: (exercise #:difficulty difficulty #:tag tag body ...)
@; @exercise[#:difficulty 1 #:tag "e1.1"]{

 写出下列集合的归纳式定义。以三种方式（自顶向下，自底向上，推理规则）写出每个定
 义。用你的规则证明，给每个集合的一些元素例子写出推理。

 @itemlist[#:style 'ordered

  @item{@${\{ 3n+2 \mid n \in N \}}}

  @item{@${\{ 2n + 3m + 1 \mid n, m \in N \}}}

  @item{@${\{ (n, 2n + 1) \mid n \in N \}}}

  @item{@${\{ (n, n^2) \mid n \in N \}}。不要在你的规则中使用平方。提示，想想方
        程@${ (n + 1) ^ 2 = n ^ 2 + 2n + 1}。}

 ]

@; }
@;

@; @exercise[#:difficulty 2 #:tag "e1.2"]{

 下面的几对规则分别定义了什么集合？解释原因。

 @itemlist[#:style 'ordered

  @item{@${(0, 1) \in S \infer{(n + 1, k + 7) \in S}{(n, k) \in S}}}

  @item{@${(0, 1) \in S \infer{(n + 1, 2k) \in S}{(n, k) \in S}}}

  @item{@${(0, 0, 1) \in S \infer{(n + 1, j, i + j) \in S}{(n, i, j) \in S}}}

  @; difficulty: (difficulty 3)
  @item{@elem{[}@${\star\star\star}@elem{]} @${(0, 1, 0) \in S \infer{(n + 1, i + 2, i + j) \in S}{(n, i, j) \in S}}}

 ]

@; }

@; @exercise[#:difficulty 1 #:tag "e1.3"]{

 找出自然数的集合 @${T}，使 @${0 \in T}，且对任何 @${n \in T}，都有 @${n + 3 \in
 T}，但 @${ T \neq S}， @${S} 是定义 1.1.2 中指定的集合。

@; }

@subsection[#:tag "dsug"]{用语法定义集合}

前述例子较为直观，但是不难想象，描述更为复杂的数据类型时，此过程将十分棘手。为便
利计，我们展示如何用@emph{语法}定义集合。语法通常用来指定字符串的集合，但是我
们也可以用它来定义值的集合。

例如，可以用语法定义集合 @List-of-Int[]：

@; @grammar : (grammar production ...)
@; @production : (production name expression #:code code-item)
@; @code-item : elem
@; @grammar{

@centered[

@tabular[

#:sep @hspace[1]
#:style 'centered
(list (list @List-of-Int[] @${::=} @tt{()} )
      (list @List-of-Int[] @${::=} @elem{@tt{(@${Int} . @List-of-Int[])}}))]

]

@; }

这里的两条规则对应上述定义 1.1.4 的两条属性。第一条规则是说空表属于
@List-of-Int[]，第二条是说若 @${n} 属于 @${Int} 且 @${l} 属于 @List-of-Int[]，则
@tt{(@${n} . @${l})} 属于 @List-of-Int[]。这些规则叫做@emph{语法}。

@elem[#:style question]{来看看这一定义的各个片段}。在这一定义中我们有：

@itemlist[

  @item{@bold{非终止符}。这些是所定义集合的名字。本例中只有一个这样的集合，但是
        通常，可能会定义数个集合。这些集合有时称为 @emph{句法类别}。

        依照惯例，我们将非终止符和集合名的首字母大写，但在文中提及它们的元素时，
        将使用小写名称。这要比听起来容易。例如， @${Expression} 是非终止符，但我
        们写作 @${e \in Expression} 或 “@${e} 是一个 expression。”

        另一常见惯例，名为@emph{巴科斯-诺尔范式}或 @emph{BNF}，是在词周围加尖括
        号，如 <expression>。 }

  @item{@bold{终止符}。这些是@elem[#:style question]{外部表示}中的字符，在本例中，
        是 @tt{.}、@tt{(} 和 @tt{)}。这些常用打字机字体写出，如 @tt{lambda}。}

  @item{@bold{生成式}。规则叫做@emph{生成式}。每个生成式的左边是一个非终止符，右
       边则包含终止符和非终止符。左右两边通常用符号 @${::=} 分隔，读作@emph{是}
       或@emph{可能是}。根据其他句法类别和@emph{终止符}（如左括号、右括号和句点），
       右边指定一种方法，用来构建句法类别的成员。}

]

若某些句法类别的含义在上下文中足够清晰，在生成式中提到它们时通常不做定义，如
@${Int}。

语法经常用一些简便形式书写。当一个生成式的左边与前一生成式相同时，一般会略去。根
据这一惯例，我们的语法可以写作：

@$${@List-of-Int[] ::= @tt{()}
                   ::= @tt{(Int . @List-of-Int[])}}

为单一句法类别编写一组规则时，也可以只写一次左边内容和 @${::=}，后续的所有右边内
容用特殊符号“|”（竖线，读作@emph{或}）分隔。 用“|”，@List-of-Int[]
的语法可以写作：


@$${@List-of-Int[] ::= @tt{()} | @tt{(Int . @List-of-Int[])}}

另一种简写是 @emph{Kleene 星} (@emph{Kleene Star})，写作 {...}*。当它出现
在右边时，@elem[#:style question]{表示花括号之间的内容出现任意次数的序列}。用
Kleene 星，@List-of-Int[] 的定义可以简写为：

@$${@List-of-Int[] ::= @tt{({Int}*)}}

@elem[#:style question]{这也包含不出现内容的情况。如果内容为0，得到的是空字符串。}

星号的变体是 @emph{Kleene 加} (@emph{Kleene Star}) {...}+，表示一个或多个
@elem[#:style question]{实例}的序列。在上例中，用 + 替代 *，就定义了句法类别非空
整数列表。

星号还有一种变体是@emph{分隔表} (@emph{separated list}) 法。例如，用
