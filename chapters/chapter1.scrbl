#lang scribble/book
@(require "../style/style.rkt")
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

@; {
@; definition
自然数@${n}属于@${S}，当且仅当：

@itemlist[#:style 'ordered

 @item{@${{n = 0}}，或}

 @item{@${n - 3 \in S}。}

]
@;
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

@; definition
定义集合@${S}为@${N}所包含的最小集合，满足如下两条性质：

@itemlist[#:style 'ordered

 @item{@${0 \in S}，且}

 @item{若@${n \in S}，则@${n + 3 \in S}。}

]
@;

“最小集合”是指该集合满足性质1和2，并且是其他任何满足性质1和2的集合的子集。易知
只能有一个这样的集合：如果@${S_1}和@${S_2}都满足性质1和2，并且都为最小，那么
@${S_1 \subseteq S_2}（因为@${S_1}最小）并且@${S_2 \subseteq S_1}（因为@${S_2}最
小），因此@${S_1 = S_2}。我们需要这一额外条件，因为否则的话将有许多集合满足其他
两个条件（见练习1.3）。

这里是另一种表示定义的方式：

@$${\infer{0 \in S}{}}

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

@; definition, with name
@; name
（整数列表，自顶向下）
@;
Scheme列表是整数列表，当且仅当

@itemlist[#:style 'ordered

 @item{列表为空，或}

 @item{列表为序对，首项为整数，余项为整数列表。}

]
@;

我们用@${Int}表示所有整数的集合，用@List-of-Int[]表示所有整数列表
的集合。

@; definition, with name
@; name
（整数列表，自底向上）
@;
集合@List-of-Int[]是满足如下两条性质的最小Scheme列表集合：

@itemlist[#:style 'ordered

 @item{@${() \in @List-of-Int{}}，或}

 @item{若@${n \in Int}且@${l \in @List-of-Int{}}，则 @${(n . l) \in
       @List-of-Int{}}。}

]
@;

这里，我们用中缀“.”代表Scheme中 @racket[cons] 操作的结果。式子@${(n . l)}代表
Scheme序对的首项为@${n}，余项为@${l}。

@; definition, with name
@; name
（整数列表，推理规则）
@;
@$${\infer{() \in @List-of-Int{}}{}}

@$${\infer{(n . l) \in @List-of-Int{}}{n \in Int & l \in @List-of-Int{}}}
@;

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
