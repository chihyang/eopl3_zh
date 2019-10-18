#lang scribble/book
@(require "style.rkt")
@(require latex-utils/scribble/theorem)
@(require scribble/manual)
@(require scribble-math)
@(require scribble/example)
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
        通常，可能会定义数个集合。这些集合有时称为@emph{句法类别}。

        依照惯例，我们将非终止符和集合名的首字母大写，但在文中提及它们的元素时，
        将使用小写名称。这要比听起来容易。例如， @${Expression} 是非终止符，但我
        们写作 @${e \in Expression} 或 “@${e} 是一个 expression。”

        另一常见惯例，名为@emph{巴科斯-诺尔范式}或 @emph{BNF}，是在词周围加尖括
        号，如 <expression>。 }

  @item{@bold{终止符}。这些是@elem[#:style question]{外部表示}中的字符，在本例中，
        是 @tt{.}、@tt{(} 和 @tt{)}。这些常用打字机字体写出，如 @tt{lambda}。}

  @item{@bold{生成式}。规则叫做@emph{生成式}。每个生成式的等号左边是一个非终止符，
       等号右边则包含终止符和非终止符。左右两边通常用符号 @${::=} 分隔，读作
       @emph{是}或@emph{可能是}。根据其他句法类别和@emph{终止符}（如左括号、右括
       号和句点），等号右边指定一种方法，用来构建句法类别的成员。}

]

若某些句法类别的含义在上下文中足够清晰，在生成式中提到它们时通常不作定义，如
@${Int}。

语法经常用一些简便形式书写。当一个生成式的等号左边与前一生成式相同时，一般会略去。
根据这一惯例，我们的语法可以写作

@$${@List-of-Int[] ::= @tt{()}
                   ::= @tt{(Int . @List-of-Int[])}}

为单一句法类别编写一组规则时，也可以只写一次左边内容和 @${::=}，后续的所有等号右
边内容用特殊符号“|”（竖线，读作@emph{或}）分隔。 用“|”，@List-of-Int[]的语法
可以写作：


@$${@List-of-Int[] ::= @tt{()} | @tt{(Int . @List-of-Int[])}}

另一种简写是 @emph{Kleene 星} (@emph{Kleene Star})，写作 {...}*。当它出现在等号
右边时，@elem[#:style question]{表示花括号之间的内容出现任意次数的序列}。用
Kleene 星，@List-of-Int[] 的定义可以简写为

@$${@List-of-Int[] ::= @tt{({Int}*)}}

@elem[#:style question]{这也包含不出现内容的情况。如果内容为0，得到的是空字符串。}

星号的变体是 @emph{Kleene 加} (@emph{Kleene Star}) {...}+，表示一个或多个
@elem[#:style question]{实例}的序列。在上例中，用 + 替代 *，就定义了句法类别非空
整数列表。

星号还有一种变体@emph{分隔表} (@emph{separated list}) 表示法。例如，用
@${Int^{*(c)}} 表示任意数量的非终止符 @${Int} 序列，以非空字符序列 @${c} 分隔。
这也包括没有任何@elem[#:style question]{实例}的情况。如果有 0 个实例，得到的是空
字符串。例如，@${Int^{*(,)}} 包含字符串

@tt{8}

@tt{14, 12}

@tt{7, 3, 14, 16}

@${Int^{*(;)}} 包含字符串

@tt{8}

@tt{14; 12}

@tt{7; 3; 14; 16}

这些简写不是必需的。总能不倚赖它们重写语法。

如果一个集合由语法定义，那么可以用@emph{句法推导} (@emph{syntactic derivation})
证明给定值是集合的成员。这样的推导起始于集合对应的非终止符，在由箭头
@${\Rightarrow} 指示的每一步骤中，或者用对应规则的等号右边代换非终止符，或者在句
法类别未作定义时，用该类别的已知成员代换非终止符。例如，前述证明 @tt{(14 . ())}
是整数列表，可以用句法推导形式化为

@$${@List-of-Int[] \Rightarrow @tt{(Int . @List-of-Int[])}
                   \Rightarrow @tt{(14 . @List-of-Int[])}
                   \Rightarrow @tt{(14 . ())} }

非终止符的替换顺序无关紧要。@elem[#:style question]{那么， @tt{(14 . ())} 的另一
推导是：}

@$${@List-of-Int[] \Rightarrow @tt{(Int . @List-of-Int[])}
                   \Rightarrow @tt{(Int . ())}
                   \Rightarrow @tt{(14 . ())} }

@; @exercise[#:difficulty 1 #:tag "e1.4"]{

 写出从 @List-of-Int[] 到 @tt{(-7 . (3 . (14 ())))} 的推导。

@; }

让我们思考其他一些有用集合的定义。

@itemlist[#:style 'ordered

 @item{许多操作符号的过程被设计为只处理包含符号和其他类似约束的列表。我们把这些
 叫做 @tt{s-list}，定义如下：

 @; @def[ #:title
 （s-list，s-exp）
 @; ]
 @; {
 @$${S\mbox{-}list ::= ({S-exp^*})}
 @$${S\mbox{-}list ::= Symbol | S\mbox{-}list}
 @; }

 s-list 是 s-exp 的列表，s-exp 或者是 s-list，或者是一个符号。这里是一些 s-list。

 @tt{(a b c)}
 @tt{(an (((s-list)) (with () lots) ((of) nesting)))}

 有时也使用推广的 s-list 定义，既允许整数，也允许符号。

 }

 @item{使用三个元素的列表表示内部节点，则叶子是数值，内部节点是符号的二叉树可用
 语法表示为：

 @; @def[ #:title
 （二叉树）
 @; ]
 @; {
 @$${Bintree ::= Int | (Symbol Bintree Bintree)}
 @; }

 这里是此类树的一些例子：

 @tt{1}
 @tt{2}
 @tt{(foo 1 2)}
 @tt{(bar 1 (foo 1 2))}
 @tt{(baz
       (bar 1 (foo 1 2))
       (biz 4 5))}

 }

 @item{@emph{lambda 演算} (@emph{lambda calculus}) 是一种简单语言，常用于研究编
 程语言理论。这一语言只包含变量引用，单参数过程，以及过程调用，可用语法定义为：

 @; @def[ #:title
 （lambda 演算）
 @; ]
 @; {
 @$${LcExp ::= Identifier ::= @tt{(lambda (@${Identifier}) @${LcExp})} ::= @tt{(@${LcExp} @${LcExp})}}

 @emph{其中，标识符 (@${Identifier}) 是除 @tt{lambda} 之外的任何符号。}
 @; }

 第二个生成式中的标识符是 @tt{lambda} 表达式主体内的变量名。这一变量叫做表达式的
 @emph{绑定变量} (@emph{bound variable})，因为@elem[#:style question]{同名变量}
 一旦出现在主体内就由它绑定或捕获。主体内出现的任何@elem[#:style question]{同名
 变量}都指代这一个。

 要明白这怎么用，考虑推广到算术操作符的 lambda 演算。在那种语言里，

 @codeblock{(lambda (x) (+ x 5))}

 是一表达式，@tt{x} 是其绑定变量。这一表达式描述的过程把参数加5。因此，在

 @codeblock{((lambda (x) (+ x 5)) (- x 7))}

 中，最后一个出现的 @tt{x} 不是指 @tt{lambda} 表达式中绑定的 @tt{x}。1.2.4 节中
 介绍了 @tt{occurs-free?}，@elem[#:style question]{到时我们再讨论这问题。}

 这一语法把 @${LcExp} 的元素定义为 Scheme 值，因此很容易写出程序来操作它们。

 }

]

这些语法叫做 @emph{上下文无关} (@emph{context-free}) 语法，因为由给定句法类别定
义的规则可以在引用这一语法类别的任何上下文中使用。有时这不够严格。考虑二叉搜索树。
二叉搜索树中的一个节点或者为空，或者包含一个整数、两棵子树

@$${Binary\mbox{-}search\mbox{-}tree ::= @tt{()} | @tt{(@${Int}
@${Binary\mbox{-}search\mbox{-}tree} @${Binary\mbox{-}search\mbox{-}tree})}}

该语法正确描述了每个节点的结构，但是忽略了关于二叉搜索树的一个重要事实：所有左子
树的键值都小于（或等于）当前节点，所有右子树的键值都大于当前节点。

由于这一额外约束，不是每个由 @${Binary\mbox{-}search\mbox{-}tree} 得出的句法推导
都是正确的二叉搜索树。要判断特定的生成式能否用于特定的句法推导，必须察看使用生成
式的上下文。这样的约束叫做@emph{上下文敏感约束} (context-sensitive constraints)，
或@emph{不变式} (invariants)。

定义编程语言的语法也会带来上下文敏感约束。例如，在许多编程语言中变量必须在使用之
前声明。@elem[#:style question]{对变量使用的这一约束就对使用它们的上下文敏
感。}@nonbreaking{}虽然可以用形式化方法定义上下文敏感约束，但这些方法远比本章考
虑的复杂。实际中，通常的方法是先定义上下文无关语法，随后再用其他方法添加约束。第
七章展示了这种技术的一个例子。

@subsection[#:tag "induct"]{归纳}

@elem[#:style question]{归纳式的集合定义有两种用法}：证明关于集合成员的定理，写
出操作集合成员的程序。这里给出一个此类证明的例子，写程序是下一节的主题。

@; @theorem
@; {
令 t 为二叉树，形如定义 1.1.7，则 t 包含奇数个节点。
@; }

@; @proof
@; @{
用归纳法证明 t 的尺寸。令 t 的尺寸等于 t 中节点的个数。归纳假设 IH(k) 为，尺寸
@${\leq k} 的任何树有奇数个节点。依照归纳证明的常规方法：先证明 @${IH(0)} 为真，
然后证明对任何 @${k} 整数，@${IH} 均为真，则对 @${k + 1}，@${IH} 也为真。

@itemlist[#:style 'ordered

 @item{没有树包含 0 个节点，所以 @${IH(0)} 显然成立。}

 @item{设 @${k} 为整数，@${IH(k)} 成立，即，任何树的节点数 @${\leq k} 时，
 @elem[#:style question]{准确}节点数为奇数。需证明 @${IH(k + 1)} 也成立：任何树
 的节点数 @${\leq k + 1} 时，节点数为奇数。若 @${t} 有 @${\leq k + 1} 个节点，根
 据二叉树的定义，只有两种可能：

 @itemlist[#:style 'ordered

  @item{@${t} 形如 @${n}，@${n} 为整数。此时 @${t} 只有一个节点，一为奇数。}

  @item{@${t} 形如 @${@tt{(@${sym} @${t_1} @${t_2})}}，其中，@${sym} 是一符号，
  @${t_1} 和 @${t_2} 是树。此时 @${t_1} 和 @${t_2} 节点数少于 @${t}。因为 @${t}
  有 @${\leq k + 1}个节点，@${t_1} 和 @${t_2} 一定有 @${\leq k} 个节点。因此它们
  符合 @${IH(k)}，一定各有奇数个节点，不妨分别设为 @${2n_1 + 1} 和 @${2n_2 + 1}。
  则算上两棵子树和根，原树中的节点总数为

  @$${(2n_1 + 1) + (2n_2 + 1) + 1 = 2(n_1 + n_2 + 1) + 1}

  也是一个奇数。}

 ]
 }
]

声明 @${IH(k + 1)} 成立证毕，归纳完成。
@; @}

证明的关键是树 @${t} 的子结构总是比 @${t} 本身小。这种证明模式叫做@emph{结构化归
纳}。

@; @tip[#:title
结构化归纳证明
@; ]{
欲证假设 @${IH(s)} 对所有结构 @${s} 为真，证明如下：

@itemlist[#:style 'ordered

 @item{@${IH} 对简单结构（没有子结构）为真。}

 @item{若 @${IH} 对 @${s} 的子结构为真，则对 @${s} 本身也为真。}
]
@;}

@; @exercise[#:difficulty 2 #:tag "e1.5"]{

 证明若 @${e \in LcExp}，则 @${e} 中的左右括号数量相等。

@; }

@section[#:tag "drp"]{{推导}递归程序}

我们已经用归纳式定义法描述了复杂集合。我们已明白可通过分析归纳式定义集合的元素来
观察集合是如何从较小元素构建的。我们已经用这一思想写出了过程 @tt{in-S?} 来判断一
个自然数是否属于集合 @${S}。现在，我们用同样的思想定义更通用的过程，以便对归纳式
定义集合做运算。

递归程序依赖于一条重要原则：

@; @tip[#:title
较小子问题原则
@; ]
@; {
若能化问题为较小的子问题，则能调用解决原问题的过程解决子问题。
@; }

已返回的子问题解随后可用来求解原问题。这行得通，因为每次过程调用，都是针对较小的
子问题，直到最终调用，针对的是一个可直接解决的问题，而不必再次调用它本身。

我们用一些例子解释这一思想。

@subsection[#:tag "l-l"]{@tt{list-length}}

标准的 Scheme 程序 @tt{length} 求出列表中的元素个数。

@examples[#:label #f (length '(a b c))
                     (length '((x) ()))]

我们来写出自己的过程，名叫 @tt{list-length}，做同样的事。

先来写出过程的@emph{合约}。合约指定了过程可取参数和可能返回值的集合。合约也可以
包含过程的期望用法或行为。这有助于我们在编写时及以后追溯我们的意图。在代码中，这
是一条注释，我们用打字机字体示之，以便阅读。

@; racketblock with contracts and usage
@codeblock{
@; contracts
; list-length : #,($ List) -> #,($ Int)
@; usage
; 用法 : (list-length l) = l 的长度
(define list-length
  (lambda (lst)
    ...))
}
@;

列表的集合可定义为

@$${List ::= () | (Scheme value . List)}

因此，考虑列表的每种情况。若列表为空，则长度为0。

@; racketblock with contracts and usage and diff
@codeblock{
@; contracts
; list-length : List -> Int
@; usage
; 用法 : (list-length l) = l 的长度
(define list-length
  (lambda (lst)
@; diff{
    (if (null? lst)
        0
@; }
        ...)))
}
@;

若列表非空，则其长度比其余项长度多1。这就给除了完整定义。

@; racketblock with contracts and usage and diff
@codeblock{
@; contracts
; list-length : List -> Int
@; usage
; 用法 : (list-length l) = l 的长度
(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        @; diff{
        (+ 1 (list-length (cdr lst))))))
        @; }
}
@;

通过 @tt{list-length} 的定义，我们可以看到它的运算过程。

@tt{(list-length '(a (b c) d))
 = (+ 1 (list-length '((b c) d)))
 = (+ 1 (+ 1 (list-length '(d))))
 = (+ 1 (+ 1 (+ 1 (list-length '()))))
 = (+ 1 (+ 1 (+ 1 0)))
 = 3}

@subsection[#:tag "n-e"]{@tt{nth-element}}

标准的 Scheme 过程 @tt{list-ref} 取一列表 @tt{lst} 和从 0 开始计数的索引 @tt{n}，
返回 @tt{lst} 的第 @tt{n} 个元素。

@examples[#:label #f (list-ref '(a b c) 1)]

我们来写出自己的过程，名叫 @tt{nth-element}，做同样的事。

我们仍用上述 @${List} 的定义。

当 @${lst} 为空时，@tt{(nth-element @${lst} @${n})} 应当返回什么？这种情况下，
@tt{(nth-element @${lst} @${n})} 要取出空列表的元素，所以我们报告错误。

当 @${lst} 非空时，@tt{(nth-element @${lst} @${n})} 应当返回什么？答案取决于
@${n}。若 @${n = 0}，答案就是 @${lst} 的首项。

当 @${lst} 非空，且 @${n \neq 0} 时，@tt{(nth-element @${lst} @${n})} 应当返回
什么？这种情况下，答案是 @${lst} 余项的第 @${(n - 1)} 个元素。由 @${n \in N} 且
@${n \neq 0}，可知 @${n - 1} 一定属于 @${N}，所以可通过递归调用 @tt{nth-element}
找出第 @${(n - 1)} 个元素。

这使我们得出定义

@; codeblock with contracts and usage
@codeblock{
@; contracts
; nth-element : List x Int -> SchemeVal
@; usage
; 用法 : (nth-element lst n) = lst 的第 n 个元素
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
}
@;

这里的注释 @code{nth-element : List x Int -> SchemeVal} 表示 @bold{nth-element}
是一个过程，取两个参数，一个为列表，一个为整数，返回一个 Scheme 值。这与数学中的
表示 @${f : A \times B \to C} 相同。

过程 @tt{report-list-too-short} 调用 @tt{eopl:} @tt{error} 来报告错误。过程
@tt{eopl:error} 会终止计算。它的首个参数是一符号，用于在错误信息中指示调用
@tt{eopl:error} 的过程。第二个参数是一个字符串，会打印为错误信息。对应于字符串中
的每个字符序列 @tt{~s} ，都必须有一个额外参数。打印字符串时，这些参数的值会替换
对应的 @tt{~s} 。@tt{~%} 视作换行。错误信息打印出来之后，计算终止。过程
@tt{eopl:error} 并非标准 Scheme 的一部分，但大多数 Scheme 实现提供这样的组件。在
本书中，我们以类似方式，用名字含 @tt{report-} 的过程报告错误。

来看看 @tt{nth-element} 如何算出它的答案：

@tt{(nth-element '(a b c d e) 3)
  = (nth-element   '(b c d e) 2)
  = (nth-element     '(c d e) 1)
  = (nth-element       '(d e) 0)
  = d}

@tt{nth-element} 递归处理越来越短的列表和越来越小的数字。

如果排除错误检查，我们得靠 @tt{car} 和 @tt{cdr} 的抱怨来获知传递了空列表，但它们
的错误信息没什么帮助。例如，当我们收到 @tt{car} 的错误信息，可能得找遍整个程序中
使用 @tt{car} 的地方。

@; @exercise[#:difficulty 1 #:tag "e1.6"]{

 如果翻转 @tt{nth-element} 中两个测试的顺序，会有什么问题？

@; }

@; @exercise[#:difficulty 2 #:tag "e1.7"]{

 @tt{nth-element} 的错误信息不够详尽。重写 @tt{nth-element}，给出更详细的错误信
 息，像是 “@tt{(a b c)} 不足 8 个元素”。

@; }

@subsection[#:tag "r-f"]{@tt{remove-first}}

过程 @tt{remove-first} 取两个参数：符号 @${s} 和符号列表 @${los}。它返回一个列表，
除了不含第一个出现在 @${los} 中的符号 @${s} 外，元素及其排列顺序与 @${los} 相同。
如果 @${s} 没有出现在 @${los} 中，则返回 @${los}。
