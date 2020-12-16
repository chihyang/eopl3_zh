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

@title[#:style part-title-style-numbered #:tag "da"]{数据抽象}

@eopl-index["Data abstraction"]
@section[#:style section-title-style-numbered #:tag "s2.1"]{用接口定义数据}

每当我们想以某种方式表示一些量时，我们就新定义了一种数据类型：它的取值是其表示，
它的操作是处理其实体的过程。

这些实体的表示通常很复杂，所以如能避免，我们不愿关心其细节。我们可能想改变数据的
表示。最高效的表示往往难以实现，所以我们可能希望先简单实现，只在确知系统的整体性
能与之攸关时，才改用更高效的表示。不管出于什么原因，如果我们决定改变某些数据的表
示方式，我们得能定位程序中所有依赖表示方式的部分。这就需要
借助@term["data abstraction"]{数据抽象} 技术。

@eopl-index["Implementation" "of ADT"]
@eopl-index["Interface" "of ADT"]
数据抽象将数据类型分为两部分：@term["interface"]{接口} 和@term["implementation"]{实现}。
@term[#f]{接口}告诉我们某类型表示什么数据，能对数据做什么操作，以及可由这些操作得出
的性质。@term[#f]{实现}给出数据的具体表示，以及处理数据表示的代码。

@eopl-index{Abstract data types (ADTs)}
这样抽象出的数据类型称为@term["abstract data type"]{抽象数据类型}。程序的其余部
@eopl-index["Client of ADT"]
分——数据类型的@term["client"]{客户} ——只能通过接口中指定的操作处理新数据。这样一
来，如果我们希望改变数据的表示，只需改变数据处理接口的实现。

这一想法并不陌生：我们写程序处理文件时，多数时候只关心能否调用过程来打开，关闭，
读取文件或对文件做其他操作。同样地，大多数时候，我们不关心整数在机器中究竟怎样表
示，只关心能否可靠地执行算术操作。

@eopl-index["Client of ADT"]
当客户只能通过接口提供的过程处理某类型的数据时，我们说客户代码
与@term["representation-independent"]{表示无关}，因为这些代码不依赖数据类型值的
表示。

那么所有关于数据表示的信息必然在实现代码之中。实现最重要的部分就是表示数据的规范。
我们用符号 @${\lceil v \rceil} 指代@exact-elem{“}数据 @${v} 的表示@exact-elem{”}。

@eopl-index[#:range-mark 'start "Natural numbers ADT"]
要说得更明白些，来看一个简单例子：自然数类型。待表示的数据是自然数。接口由四个过
程组成：@tt{zero}、@tt{is-zero?}、@tt{successor} 和 @tt{predecessor}。当然，不是
随便几个过程都可以作为这一接口的实现。当且仅当一组过程满足如下四个方程时，可以作
为 @tt{zero}、@tt{is-zero?}、@tt{successor} 和 @tt{predecessor} 的实现：

 @nested[#:style 'inset]{
  @elemtag["nat"]{@tt{(zero)} = @${\lceil 0 \rceil}}

  @tt{(is-zero? @${\lceil n \rceil})} = @m{@env["cases"]{@tt{#t} & n = 0 \\
                                                         @tt{#f} & n \neq 0}}

  @tt{(successor @${\lceil n \rceil})} = @${\lceil n + 1 \rceil \quad (n \geq 0)}

  @tt{(predecessor @${\lceil n + 1 \rceil})} = @${\lceil n \rceil \quad (n \geq 0)}}

这一定义没有指明自然数应当如何表示，它只要求这些过程都产生指定的行为。即，过程
@tt{zero} 必须返回 @${0} 的表示；给定数字 @${n} 的表示，过程 @tt{successor} 必须
返回数字 @${n + 1} 的表示，等等。这个定义没对 @tt{(predecessor (zero))} 做出说明，
所以按这个定义，任何行为都是可以接受的。

现在可以写出处理自然数的客户程序，而且不论用哪种表示方式，都保证能得出正确的结果。
例如，不论怎样实现自然数，

@nested{
@eopl-code{
@racketblock[
(@#,elem{@elemtag["plus"]{}}define plus
  (lambda (x y)
    (if (is-zero? x)
      y
      (successor (plus (predecessor x) y)))))
]
}

都满足 @tt{(plus @${\lceil x \rceil} @${\lceil y \rceil}) @${=} @${\lceil x +
y\rceil}}。}

@eopl-index["Constructor"]
@eopl-index["Observers"]
大多数接口都包含：若干@term["constructor"]{构造器}，用来产生数据类型的元素；
若干@term["observer"]{观测器}，用来从数据类型的值中提取信息。这里有三个构造器，
@tt{zero}、@tt{successor} 和 @tt{predecessor}；一个观测器，@tt{is-zero?}。
@eopl-index[#:range-mark 'end "Natural numbers ADT"]

可以用多种方式表示这套接口，我们考虑其中三种。

@itemlist[#:style 'ordered

 @item{@eopl-index[#:range-mark 'start "Natural numbers ADT"
 @eopl-index-entry["with unary representation" "unaryrepresentation"]]
 @eopl-index["Unary representation of natural numbers"]
 @term["Unary representation"]{一元表示法}：在一元表示法中，自然数@${n} 由
 @${n} 个 @tt{#t} 组成的列表表示。所以，@${0} 表示为 @tt{()}，@${1} 表示为
 @tt{(#t)}，@${2} 表示为 @tt{(#t #t)}，等等。可以用归纳法定义这种表示方式：

 @nested[#:style 'inset]{@${\lceil 0 \rceil = @tt{()}}

 @${\lceil n + 1 \rceil = @tt{(#t . @${\lceil n \rceil})}}}

 要满足该表示的定义，数据处理过程可以写成：

@eopl-code{
 @racketblock[(define zero (lambda () '()))
 (define is-zero? (lambda (n) (null? n)))
 (define successor (lambda (n) (cons #t n)))
 (define predecessor (lambda (n) (cdr n)))]}
 @eopl-index[#:range-mark 'start "Natural numbers ADT"
 @eopl-index-entry["with unary representation" "unaryrepresentation"]]}

 @item{@eopl-index[#:range-mark 'start "Natural numbers ADT"
 @eopl-index-entry["with Scheme numbers" "Schemenumbers"]]
 @term["Scheme number representation"]{Scheme 数字表示法}：在这种表示中，
 只需用 Scheme 内置的数字表示法（本身可能十分复杂！）。令 @${\lceil n \rceil} 为
 Scheme 整数 @tt{n}，则所需的四个过程可以定义为：

@eopl-code{
 @racketblock[(define zero (lambda () 0))
 (define is-zero? (lambda (n) (zero? n)))
 (define successor (lambda (n) (+ n 1)))
 (define predecessor (lambda (n) (- n 1)))]
@eopl-index[#:range-mark 'end "Natural numbers ADT"
@eopl-index-entry["with Scheme numbers" "Schemenumbers"]]}
 }

 @item{@term["Bignum representation"]{大数表示法}：
 @eopl-index["Bignum representation of natural numbers"]
 @eopl-index[#:range-mark 'start "Natural numbers ADT" @eopl-index-entry["with bignum representation" "bignumrepresentation"]]
 在大数表示法中，数值以 @${N} 进制表示，@${N} 是某个大整数。该方法以 @${0} 到
 @${N-1} 之间的数字（有时不称数位，而称@term["bigits"]{大位}）组成的列表表示数值，
 这就很容易表示远超机器字长的整数。这里，为了便于使用，我们把最低位放在列表最前
 端。这种表示法可用归纳法定义为：

 @m{\lceil n \rceil = @env["cases"]{@tt{()} & n = 0 \\ @tt{(@m{r} . @m{\lceil q
                                    \rceil})} & n = qN + r, 0 \leqslant r < N}}

 所以，如果 @${N = 16}，那么 @${\lceil 33 \rceil = @tt{(1 2)}}，@${\lceil 258
 \rceil = @tt{(2 0 1)}}，因为：

 @$${258 = 2 \times 16^0 + 0 \times 16^1 + 1 \times 16^2}
 @eopl-index[#:range-mark 'end "Natural numbers ADT" @eopl-index-entry["with bignum representation" "bignumrepresentation"]]
 }

]

@eopl-index{Abstract type}这些实现都没有强制数据抽象：无法防止客户程序查验表示用
的是列表还是 Scheme 整数。与之相对，有些语言直接支持数据抽象：它们允许程序员创建
新的接口，确保只能通过接口提供的过程处理新数据。如果类型的表示隐藏起来，不会因任
何操作而暴露（包括打印），那就说该类型是@term["opaque"]{模糊} 的，否则称之为
@term["transparent"]{透明} 的。
@eopl-index["Concrete types"]
@eopl-index["Opaque type"]

Scheme 没有提供标准机制来创建新的模糊类型，所以我们退而求其次：定义接口，靠客户
程序的作者小心行事，只使用接口中定义的过程。

在@secref{modules}中，我们讨论一些方法，以便强化语言的这种协议。

@exercise[#:level 1 #:tag "ex2.1"]{

 @eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.1"] "Factorial function"]
 实现大数表示法的四种操作。然后用你的实现计算10的阶乘。随着参数改变，执行时间如
 何变化？随着进制改变，执行时间如何变化？解释原因。
 @eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.1"] "Factorial function"]

}

@exercise[#:level 2 #:tag "ex2.2"]{

详加分析上述表示。从满足数据类型定义的角度来说，它们在何种程度上是成功或失败的？

}

@exercise[#:level 2 #:tag "ex2.3"]{

 @eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.3"]
 "Natural numbers ADT" @eopl-index-entry["with diff-tree representation" "difftreerepresentation"]]
 用差分树表示所有整数（负数和非负数）。差分树是一列表，可用语法定义如下：

 @nested{@$${\mathit{Diff\mbox{-}tree} ::= @tt{(one)} \mid @tt{(diff
 @${\mathit{Diff\mbox{-}tree}} @${\mathit{Diff\mbox{-}tree}})}}
 @eopl-index[(eopl-index-entry @elem{Diff-trees (@${\mathit{Diff\mbox{-}tree}})} "Difftrees")]

 列表 @tt{(one)} 表示 1。如果 @${t_1} 表示 @${n_1}，@${t_2} 表示 @${n_2}，那么
 @tt{(diff @${n_1} @${n_2})} 表示 @${n_1 - n_2}。

 所以，@tt{(one)} 和 @tt["(diff"] @tt{(one)} @tt["(diff"] @tt{(one)}
 @tt["(one)))"] 都表示1；@tt["(diff"] @tt["(diff"] @tt{(one)} @tt["(one))"]
 @tt["(one))"] 表示 @${-1}。

 @itemlist[#:style 'ordered

   @item{证明此系统中，每个数都有无限种表示方式。}

   @item{实现这种整数表示法：写出@pageref{nat}定义的 @tt{zero}、@tt{is-zero?}、
   @tt{successor} 和 @tt{predecessor}，此外还要能表示负数。这种方式下，整数的任
   何合法表示都应该能作为你过程的参数。例如，你的过程 @tt{successor} 可以接受无
   限多种 @${1} 的合法表示，且都应给出一个 @${2}的合法表示。对 @${1} 的不同合法
   表示，可以给出不同的 @${2} 的合法表示。}

   @item{写出过程 @tt{diff-tree-plus}，用这种表示做加法。你的过程应针对不同的差
   分树进行优化，并在常数时间内得出结果（即与输入大小无关）。注意，不可使用递归。
   @eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.3"]
   "Natural numbers ADT" @eopl-index-entry["with diff-tree representation" "difftreerepresentation"]]}
   ]}
}

@section[#:style section-title-style-numbered #:tag "s2.2"]{数据类型的表示策略}

@eopl-index[#:range-mark 'start @eopl-index-entry[@elem{Environment ADT (@${\mathit{Env}})} "EnvironmentADT"]]
使用数据抽象的程序具有表示无关性：与用来实现抽象数据类型的具体表示方式无关，甚至
可以通过重新定义接口中的一小部分过程来改变表示。在后面的章节中我们常会用到这条性
质。

@eopl-index["Environments"]
本节介绍几种数据类型的表示策略。我们用数据类型@term["environment"]{环境} 解释这
些选择。对有限个变量组成的集合，环境将值与其中的每个元素关联起来。在编程语言的实
现之中，环境可用来维系变量与值的关系。编译器也能用环境将变量名与变量信息关联起来。

只要能够检查两个变量是否相等，变量能够用我们喜欢的任何方式表示。我们选用 Scheme
符号表示变量，但在没有符号数据类型的语言中，变量也可以用字符串，哈希表引用，甚至
数字表示（见 @secref{s3.6}）。

@subsection[#:style section-title-style-numbered #:tag "s2.2.1"]{环境的接口}

环境是一函数，定义域为有限个变量的集合，值域为所有 Scheme 值的集合。数学上常说的
有限函数是指有序数对组成的有限集合，我们采用这一含义，就得表示形如
@m{\{(var_1,\allowbreak val_1),\allowbreak ...,\allowbreak (var_n, val_n)\}}的所
有集合。其中，@${var_i} 是某一变量，@${val_i} 是任意 Scheme 值。有时称环境
@${env} 中变量 @${var} 的值 @${val} 为其在 @${env} 中的@term["binding"]{绑定}。
@eopl-index["Binding" (eopl-index-entry "in environment" "environment")]


这一数据类型的接口有三个过程，定义如下：

 @nested{
 @eopl-index[@eopl-index-entry[@bold{@tt{empty-env}} "empty-env"]]
 @eopl-index[@eopl-index-entry[@bold{@tt{extend-env}} "extendenv"]]
 @eopl-index[@eopl-index-entry[@bold{@tt{extend-env*}} "extendenvasterisk"]]
 @envalign*{&@tt{(empty-env)} &= &\ \lceil \emptyset \rceil \\
            &@tt{(apply-env @m{\lceil f \rceil} @m{var})} &= &\ f(var) \\
            &@tt{(extend-env @m{var} @m{v} @m{\lceil f \rceil})} &= &\ \lceil g \rceil \\
            &\phantom{x} &其中，&\ g(var_1) = @env["cases"]{v & 若\ var_1 = var \\
                                                          f(var_1) & 否则}
 }
 @eopl-index[@eopl-index-entry[@bold{@tt{apply-env}} "applyenv"]]
 }

过程 @tt{empty-env} 不带参数，必须返回空环境的表示；@tt{apply-env} 用环境对变量
求值；@tt{(extend-env @${var} @${val} @${env})} 产生一个新环境，将变量 @${var}
的值设为 @${val}，此外与 @${env} 相同。例如，表达式

 @nested{
 @eopl-code{
 @racketinput[
   (define e
     (extend-env 'd 6
       (extend-env 'y 8
         (extend-env 'x 7
           (extend-env 'y 14
             (empty-env))))))]
 }

 定义了一个环境 @${e}，使 @${e(@tt{d}) = 6}，@${e(@tt{x}) = 7}，@${e(@tt{y}) =
 8}，且对任何其他变量，@${e}未定义。本例中，@tt{y}先绑定到 @${14}，随后绑定到
 @${8}。这当然只是生成该环境的众多方法之一。}

如同前一个例子，可以将接口中的过程分为构造器和观测器。本例中，@tt{empty-env}和
@tt{extend-env}是构造器，@tt{apply-env}是唯一的观测器。
@eopl-index[#:range-mark 'end @eopl-index-entry[@elem{Environment ADT (@${\mathit{Env}})} "EnvironmentADT"]]

@exercise[#:level 2 #:tag "ex2.4"]{

 考虑数据类型@term["stack"]{堆栈}，其接口包含过程 @tt{empty-stack}、@tt{push}、
 @tt{pop}、@tt{top} 和 @tt{empty-stack?}。按照示例中的方式写出这些操作的定义。哪
 些操作是构造器？哪些是观测器？

}

@subsection[#:style section-title-style-numbered #:tag "s2.2.2"]{数据结构表示法}

@eopl-index[#:range-mark 'start "Data structure representation" @eopl-index-entry["of environments" "environments"]]
@eopl-index[#:range-mark 'start "Environments" "data structure representation of"]
观察可知，每个环境都能从空环境开始，@${n} 次调用 @tt{extend-env} 得到，其中 @${n
\geqslant 0}。例如，

@nested{
@eopl-code{
@racketblock[
(extend-env @#,elem{@${var_n}} @#,elem{@${val_n}}
   ...
   (extend-env @#,elem{@${var_1}} @#,elem{@${val_1}}
     (empty-env)))]
}

由此可得一种环境的表示方法。
}

每个环境都能用下列语法定义的表达式生成：

@envalign*{\mathit{Env\mbox{-}exp} &::= @tt{(empty-env)} \\ &::= @tt{(extend-env
                            @m{\mathit{Identifier}}
                            @m{\mathit{Scheme\mbox{-}value}}
                            @m{\mathit{Env\mbox{-}exp}})}}

可以用描述列表集合的语法表示环境，由此得出@figure-ref{fig-2.1} 中的实现。过程 @tt{apply-env} 查
看表示环境的数据结构 @tt{env}，判断它表示哪种环境，并做适当操作。如果它表示空环
境，那就报错；如果它表示 @tt{extend-env} 生成的环境，那就判断要查找的变量是否与
环境中绑定的某一变量相同，如果是，则返回保存的值，否则在保存的环境中查找变量。

这是一种常见的代码模式。我们叫它@term["interpreter recipe"]{解释器秘方}：

@nested[#:style tip]{
 @centered{@bold{解释器秘方}
 @eopl-index["Interpreter Recipe"]}

 @nested[#:style tip-content]{
 @itemlist[#:style 'ordered

  @item{查看一段数据。}

  @item{判断它表示什么样的数据。}

  @item{提取数据的各个部分，对它们做适当操作。}

   ]}}

@eopl-figure{
@racketblock[
@#,elem{@${\mathit{Env} = @tt{(empty-env)} \mid @tt{(extend-env @${\mathit{Var}} @${\mathit{SchemeVal}} @${\mathit{Env}})}}}
@#,elem{@${\mathit{Var} = \mathit{Sym}}}

@#,eopl-index[@eopl-index-entry[@bold{@tt{empty-env}} "empty-env"]]
@#,elem{@bold{@tt{empty-env}} : @${() \to \mathit{Env}}}
(define empty-env
  (lambda () (list 'empty-env)))

@#,eopl-index[@eopl-index-entry[@bold{@tt{extend-env}} "extendenv"]]
@#,eopl-index[@eopl-index-entry[@bold{@tt{extend-env*}} "extendenvasterisk"]]
@#,elem{@bold{@tt{extend-env}} : @${\mathit{Var} \times \mathit{SchemeVal} \times \mathit{Env} \to \mathit{Env}}}
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

@#,eopl-index[@eopl-index-entry[@bold{@tt{apply-env}} "applyenv"]]
@#,elem{@bold{@tt{apply-env}} : @${\mathit{Env} \times \mathit{Var} \to \mathit{SchemeVal}}}
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
        (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
        (let ((saved-var (cadr env))
              (saved-val (caddr env))
              (saved-env (cadddr env)))
          (if (eqv? search-var saved-var)
            saved-val
            (apply-env saved-env search-var))))
      (else
        (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env "~s未绑定" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "非法环境: ~s" env)))
]

@eopl-caption["fig-2.1"]{环境的数据结构表示}
}
@eopl-index[#:range-mark 'end "Data structure representation" @eopl-index-entry["of environments" "environments"]]
@eopl-index[#:range-mark 'end "Environments" "data structure representation of"]

@exercise[#:level 1 #:tag "ex2.5"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.5"] "Association list (a-list)"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.5" "ex2.11"] "Data structure representation" @eopl-index-entry["of environments" "environments"]]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.5"] "Environments" "association-list representation of"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.5" "ex2.11"] "Environments" "data structure representation of"]
只要能区分空环境和非空环境，并能从后者中提取出数据片段，就能用任何数据结构表示环
境。按这种方式实现环境：空环境由空列表表示，@tt{extend-env}生成如下环境：

@nested{
@centered{
@(image "../images/alist-env"
  #:suffixes (list ".pdf" ".svg")
  "关联列表表示法")
}

@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.5"] "Association list (a-list)"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.5"] "Environments" "association-list representation of"]
这叫 @term[#f]{a-list} 或@term["association-list"]{关联列表} 表示法。
}

}

@exercise[#:level 1 #:tag "ex2.6"]{

发明三种以上的环境接口表示，给出实现。

}

@exercise[#:level 1 #:tag "ex2.7"]{

重写@figure-ref{fig-2.1} 中的 @tt{apply-env}，给出更详细的错误信息。

}

@exercise[#:level 1 #:tag "ex2.8"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.8" "ex2.9" "ex2.10"] "Association list (a-list)"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.8" "ex2.9" "ex2.10"] "Environments" "association-list representation of"]
给环境接口添加观测器 @tt{empty-env?}，用 a-list 表示法实现它。

}

@exercise[#:level 1 #:tag "ex2.9"]{

给环境接口添加观测器 @tt{has-binding?}，它取一环境 @${env}，一个变量 @${s}，判断
@${s} 在 @${env} 中是否有绑定值。用 a-list 表示法实现它。

}

@exercise[#:level 1 #:tag "ex2.10"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.10" "ex2.11"] @eopl-index-entry[@bold{@tt{extend-env*}} "extendenvasterisk"]]
给环境接口添加构造器 @tt{extend-env*}，用 a-list 表示法实现它。这个构造器取一变
量列表和一长度相等的值列表，以及一环境，其定义为：

 @envalign*{
   & @tt{(extend-env* (@m{var_1} @m{\dots} @m{var_k}) (@m{val_1} @m{\dots} @m{var_k}) @m{\lceil f \rceil})} = \lceil g \rceil, \\
   & \quad 其中，g(var) =
    @env["cases"]{
    val_i & 若 \ var = var_i \  对某个 \ i \ 成立，1 \leqslant i \leqslant k \\
    f(var) & 否则
    }
 }
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.8" "ex2.9" "ex2.10"] "Association list (a-list)"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.8" "ex2.9" "ex2.10"] "Environments" "association-list representation of"]

}

@exercise[#:level 2 #:tag "ex2.11"]{

前一题中的 @tt{extend-env*} 实现比较拙劣的话，运行时间与 @${k} 成正比。有一种表
示可使 @tt{extend-env*} 的运行时间为常数：用空列表表示空环境，用下面的数据结构表
示非空环境：

@centered{
@(image "../images/rib-cage-one"
  #:suffixes (list ".pdf" ".svg")
  "肋排环境表示法片段")
}

@nested[#:style 'noindent]{

那么一个环境看起来像是这样：

@centered{
@(image "../images/rib-cage"
  #:suffixes (list ".pdf" ".svg")
  #:width 'textwidth
  "肋排环境表示法")
}

@eopl-index["Environments" "ribcage representation of"]
这叫做@term["ribcage"]{肋排} 表示法。环境由名为@term["rib"]{肋骨} 的序对列表表示；
每根左肋是变量列表，右肋是对应的值列表。

用这种表示实现 @tt{extend-env*} 和其他环境接口。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.5" "ex2.11"] "Data structure representation" @eopl-index-entry["of environments" "environments"]]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.5" "ex2.11"] "Environments" "data structure representation of"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.10" "ex2.11"] @eopl-index-entry[@bold{@tt{extend-env*}} "extendenvasterisk"]]

}

}

@subsection[#:style section-title-style-numbered #:tag "s2.2.3"]{过程表示法}

@eopl-index[#:range-mark 'start "Environments" "procedural representation of"]
环境接口有一条重要性质：它只有 @tt{apply-env} 一个观测器。这样就能用取一变量，返
回绑定值的 Scheme 过程表示环境。

要这样表示，定义 @tt{empty-env} 和 @tt{extend-env} 的返回值为过程，调用二者的返
回值就如同调用上一节的 @tt{apply-env} 一样。由此得出下面的实现。

@eopl-code{
@racketblock[
@#,elem{@${\mathit{Env} = \mathit{Var} \to \mathit{SchemeVal}}}
@#,elem{@${\mathit{Var} = \mathit{Sym}}}

@#,eopl-index[@eopl-index-entry[@bold{@tt{empty-env}} "empty-env"]]
@#,elem{@bold{@tt{empty-env}} : @${() \to \mathit{Env}}}
(define empty-env
  (lambda ()
    (lambda (search-var)
      (report-no-binding-found search-var))))

@#,eopl-index[@eopl-index-entry[@bold{@tt{extend-env}} "extendenv"]]
@#,elem{@bold{@tt{extend-env}} : @${\mathit{Var} \times \mathit{SchemeVal} \times \mathit{Env} \to \mathit{Env}}}
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (lambda (search-var)
      (if (eqv? search-var saved-var)
        saved-val
        (apply-env saved-env search-var)))))

@#,eopl-index[@eopl-index-entry[@bold{@tt{apply-env}} "applyenv"]]
@#,elem{@bold{@tt{apply-env}} : @${\mathit{Env} \times \mathit{Var} \to \mathit{SchemeVal}}}
(define apply-env
  (lambda (env search-var)
    (env search-var)))
]
}

@tt{empty-env} 创建的空环境收到任何变量都会报错，表明给定的变量不在其中。过程
@tt{extend-env} 返回的过程代表扩展而得的环境。这个过程收到变量 @tt{search-var}
后，判断该变量是否与环境中绑定的相同。如果相同，就返回保存的值；否则，就在保存的
环境中查找它。

这种表示法中，数据由 @tt{apply-env} @term[#f]{执行的动作}表示，我们称之
为@term["procedural representation"]{过程表示法}。

数据类型只有一个观测器的情形并非想象中那般少见。比如，当数据是一组函数，就能用调
用时执行的动作表示。这种情况下，可以按照下列步骤提炼出接口和过程表示法：
@eopl-index{Action under application}

@itemlist[#:style 'ordered

 @item{找出客户代码中求取指定类型值的 lambda 表达式。为每个这样的 lambda 表达式
 写一个构造器过程。构造器过程的参数用作 lambda 表达式中的自由变量。在客户代码中，
 用构造器调用替换对应的 lambda 表达式。}

 @item{像定义 @tt{apply-env} 那样定义一个 @tt{apply-} 过程。找出客户代码中所有使
 用指定类型值的地方，包括构造器过程的主体。所有使用指定类型值的地方都改用
 @tt{apply-} 过程。}

]

@eopl-index[@eopl-index-entry[@elem{@tt{apply-} procedures} "applyprocedures"]]
一旦完成这些步骤，接口就包含所有的构造器过程和 @tt{apply-} 过程，客户代码则与表
示无关：它不再依赖表示，我们将能随意换用另一套接口实现，正如 @secref{s2.2.2}所述。

如果用于实现的语言不支持高阶过程，那就得再做一些步骤，用数据结构表示法和解释器秘
方实现所需接口，就像上一节那样。这一操作叫做@term["defunctionalization"]{消函}。
@eopl-index["Defunctionalization"]
环境的数据结构表示中，各种变体都是消函的简单例子。过程表示法和消函表示法的关系将
是本书反复出现的主题。
@eopl-index[#:range-mark 'end "Environments" "procedural representation of"]

@exercise[#:level 1 #:tag "ex2.12"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.12" "ex2.13" "ex2.14"] "Environments" "procedural representation of"]
用过程表示法实现@exercise-ref{ex2.4} 中的堆栈数据类型。

}

@exercise[#:level 2 #:tag "ex2.13"]{

扩展过程表示法，用两个过程组成的列表表示环境，实现 @tt{empty-env?}。一个过程像前
面那样，返回变量的绑定值；一个返回环境是否为空。

}

@exercise[#:level 2 #:tag "ex2.14"]{

扩展前一题中的表示法，加入第三个过程，用它来 @tt{has-binding?} （见
@exercise-ref{ex2.9}）。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.12" "ex2.13" "ex2.14"] "Environments" "procedural representation of"]

}

@section[#:style section-title-style-numbered #:tag "s2.3"]{递推数据类型的接口}

@eopl-index[#:range-mark 'start "Lambda expression (LcExp)"]
@secref{isd}大部分都在处理递推数据类型。例如，@definition-ref{d1.1.8} 给出了
lambda 演算表达式的语法：

@envalign*{\mathit{Lc\mbox{-}Exp}
           &::= \mathit{Identifier} \\
           &::= \normalfont{@tt{(lambda (@m{\mathit{Identifier}}) @m{\mathit{Lc\mbox{-}Exp}})}} \\
           &::= \normalfont{@tt{(@m{\mathit{Lc\mbox{-}Exp}} @m{\mathit{Lc\mbox{-}Exp}})}}}

我们还写出了过程 @tt{occurs-free?}。像当时提到的，@secref{s1.2.4}中
@tt{occurs-free?} 的定义不大容易读懂。比如，很难搞明白 @tt{(car (cadr exp))} 指
代 @tt{lambda} 表达式中的变量声明，或者 @tt{(caddr exp)} 指代表达式的主体。

@eopl-index[#:range-mark 'start "Predicates"]
要改善这种情况，可以给 lambda 演算表达式添加一套接口。我们的接口有几个构造器，以
及两种观测器：谓词和提取器。

构造器有：

@nested[#:style 'inset]{
@verbatim|{
|@bold{@tt{var-exp}}: |@${\mathit{Var} \to \mathit{Lc\mbox{-}Exp}}
|@bold{@tt{lambda-exp}}: |@${\mathit{Var} \times \mathit{Lc\mbox{-}Exp} \to \mathit{Lc\mbox{-}Exp}}
|@bold{@tt{app-exp}}: |@${\mathit{Lc\mbox{-}Exp} \times \mathit{Lc\mbox{-}Exp} \to \mathit{Lc\mbox{-}Exp}}
}|
}

谓词有：

@nested[#:style 'inset]{
@verbatim|{
|@bold{@tt{var-exp?}}: |@${\mathit{Lc\mbox{-}Exp} \to \mathit{Bool}}
|@bold{@tt{lambda-exp?}}: |@${\mathit{Lc\mbox{-}Exp} \to \mathit{Bool}}
|@bold{@tt{app-exp?}}: |@${\mathit{Lc\mbox{-}Exp} \to \mathit{Bool}}
}|
}

@eopl-index[#:range-mark 'start "Extractors"]
提取器有：

@nested[#:style 'inset]{
@verbatim|{
|@bold{@tt{var-exp->exp}}: |@${\mathit{Lc\mbox{-}Exp} \to \mathit{Var}}
|@bold{@tt{lambda-exp->bound-var}}: |@${\mathit{Lc\mbox{-}Exp} \to \mathit{Var}}
|@bold{@tt{lambda-exp->body}}: |@${\mathit{Lc\mbox{-}Exp} \to \mathit{Lc\mbox{-}Exp}}
|@bold{@tt{app-exp->rator}}: |@${\mathit{Lc\mbox{-}Exp} \to \mathit{Lc\mbox{-}Exp}}
|@bold{@tt{app-exp->rand}}: |@${\mathit{Lc\mbox{-}Exp} \to \mathit{Lc\mbox{-}Exp}}
}|
}

每个提取器对应 lambda 演算表达式中的一部分。现在可以写出一版只依赖接口的
@tt{occurs-free?}。

@eopl-code{
@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{occurs-free?}} "occursfree"]]
@racketblock[
@#,elem{@elemtag["occurs-free?"]{@bold{@tt{occurs-free?}}} : @${\mathit{Sym} \times \mathit{LcExp} \to \mathit{Bool}}}
(lambda (search-var exp)
  (cond
    ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
    ((lambda-exp? exp)
      (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
    (else
      (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp))))))
]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{occurs-free?}} "occursfree"]]
}

只要使用上述构造器，怎样表示 lambda 演算表达式都可以。
@eopl-index[#:range-mark 'end "Lambda expression (LcExp)"]

@eopl-index["Constructor"]
我们可以写出设计递推数据类型接口的一般步骤：

@nested[#:style tip]{
 @centered{@bold{设计递推数据类型的接口}}

 @nested[#:style tip-content]{
 @itemlist[#:style 'ordered

  @item{为数据类型的每种变体加入一个构造器。}

  @item{为数据类型的每种变体加入一个谓词。}

  @item{为传给数据类型构造器的每段数据加入一个提取器。
  @eopl-index[#:range-mark 'end "Extractors"]
  @eopl-index[#:range-mark 'end "Predicates"]}]}}

@exercise[#:level 1 #:tag "ex2.15"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.15" "ex2.16" "ex2.17"] "Lambda expression (LcExp)"]
上述语法指定了 lambda 演算表达式的表示方式，实现其接口。

}

@exercise[#:level 1 #:tag "ex2.16"]{

修改实现，换用另一种表示，去掉 @tt{lambda} 表达式绑定变量周围的括号。

}

@exercise[#:level 1 #:tag "ex2.17"]{

再发明至少两种方式来表示数据类型 lambda 演算表达式，实现它们。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.15" "ex2.16" "ex2.17"] "Lambda expression (LcExp)"]

}

@(define bidirection-eval
(parameterize ([sandbox-output 'string]
               [sandbox-error-output 'string]
               [sandbox-memory-limit 50])
  (make-evaluator
   'eopl

'(define number->sequence
  (lambda (number)
    (list number '() '())))

'(define current-element
  (lambda (node)
    (car node)))

'(define move-to-left
  (lambda (node)
    (if (at-left-end? node)
        (report-left-end (car node))
        (list (caadr node) (cdadr node) (cons (car node) (caddr node))))))

'(define move-to-right
  (lambda (node)
    (if (at-right-end? node)
        (report-right-end (car node))
        (list (caaddr node)
              (cons (car node) (cadr node))
              (cdr (caddr node))))))
'(define report-left-end
  (lambda (n)
    (eopl:error 'move-to-left "~s is at the left end of sequence." n)))

'(define report-right-end
  (lambda (n)
    (eopl:error 'move-to-right "~s is at the right end of sequence." n)))

'(define insert-to-left
  (lambda (n node)
    (list (car node)
          (cons n (cadr node))
          (caddr node))))

'(define insert-to-right
  (lambda (n node)
    (list (car node)
          (cadr node)
          (cons n (caddr node)))))

'(define at-left-end?
  (lambda (node)
    (null? (cadr node))))

'(define at-right-end?
  (lambda (node)
    (null? (caddr node)))))))

@exercise[#:level 1 #:tag "ex2.18"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.18"] "Bidirectional sequences"]
我们常用列表表示值的序列。在这种表示法中，很容易从序列中的一个元素移动到下一个，
但是不借助上下文参数，很难从一个元素移动到上一个。实现非空双向整数序列，语法为：

@mp{\mathit{NodeInSequence} ::= @tt{(@m{\mathit{Int}}
@m{\mathit{Listof@tt{(@m{\mathit{Int}})}}} @m{\mathit{Listof@tt{(@m{\mathit{Int}})}}})}}

第一个整数列表是当前元素之前的序列，反向排列。第二个列表是当前元素之后的序列。例
如，@tt{(6 (5 4 3 2 1) (7 8 9))} 表示列表 @tt{(1 2 3 4 5 6 7 8 9)}，当前元素为6。

用这种表示实现过程 @tt{number->sequence}，它取一数字，生成只包含该数字的序列。接
着实现 @tt{current-element}、@tt{move-to-left}、@tt{move-to-right}、
@tt{insert-to-left}、@tt{insert-to-right}、@tt{at-left-end?} 和
@tt{at-right-end?}。

例如：

@eopl-code{
@examples[#:eval bidirection-eval
          #:label #f
          (number->sequence 7)
          (current-element '(6 (5 4 3 2 1) (7 8 9)))
          (move-to-left '(6 (5 4 3 2 1) (7 8 9)))
          (move-to-right '(6 (5 4 3 2 1) (7 8 9)))
          (insert-to-left 13 '(6 (5 4 3 2 1) (7 8 9)))
          (insert-to-right 13 '(6 (5 4 3 2 1) (7 8 9)))]
}

如果参数在序列最右端，过程@tt{move-to-right}应失败。如果参数在序列最左端，过程
@tt{move-to-left}应失败。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.18"] "Bidirectional sequences"]

}

@(define bintree-eval
(parameterize ([sandbox-output 'string]
               [sandbox-error-output 'string]
               [sandbox-memory-limit 50])
  (make-evaluator
   'eopl

'(define number->bintree
  (lambda (n)
    (list n '() '())))

'(define current-element
  (lambda (tree)
    (car tree)))

'(define move-to-left-son
  (lambda (tree)
    (cadr tree)))

'(define move-to-right-son
  (lambda (tree)
    (caddr tree)))

'(define at-leaf?
  (lambda (tree)
    (null? tree)))

'(define insert-to-left
  (lambda (n tree)
    (list (car tree)
          (list n
                (cadr tree)
                '())
          (caddr tree))))

'(define insert-to-right
  (lambda (n tree)
    (list (car tree)
          (cadr tree)
          (list n
                (caddr tree)
                '())))))))

@exercise[#:level 1 #:tag "ex2.19"]{

@eopl-index[#:suffix @exer-ref-range["ex2.19" "ex2.20"] (eopl-index-entry @elem{Binary tree (@${\mathit{Bintree}})} "Binarytree")]
空二叉树和用整数标记内部节点的二叉树可以用语法表示为：

@mp{\mathit{BinTree} ::= @tt{()} \mid @tt{(@m{\mathit{Int}} @m{\mathit{BinTree}}
@m{\mathit{BinTree}})}}

用这种表示实现过程 @tt{number->bintree}，它取一个整数，产生一棵新的二叉树，树的
唯一节点包含该数字。接着实现 @tt{current-element}、@tt{move-to-left-son}、
@tt{move-to-right-son}、@tt{at-leaf?}、@tt{insert-to-left} 和
@tt{insert-to-right}。例如：

@eopl-code{
@examples[#:eval bintree-eval
          #:label #f
          (number->bintree 13)
          (define t1 (insert-to-right 14
                       (insert-to-left 12
                         (number->bintree 13))))
          t1
          (move-to-left-son t1)
          (current-element (move-to-left-son t1))
          (at-leaf? (move-to-right-son (move-to-left-son t1)))
          (insert-to-left 15 t1)]
}

}

@exercise[#:level 3 #:tag "ex2.20"]{

按照@exercise-ref{ex2.19} 中的二叉树表示，很容易从父节点移到某个子节点，但是不借
助上下文参数，无法从子节点移动到父节点。扩展@exercise-ref{ex2.18} 中的列表表示法，
用以表示二叉树中的节点。提示：想想怎样用逆序列表表示二叉树在当前节点以上的部分，
就像@exercise-ref{ex2.18} 那样。

用这种表示实现@exercise-ref{ex2.19} 中的过程。接着实现 @tt{move-up} 和
@tt{at-root?}。

}

@section[#:style section-title-style-numbered #:tag "s2.4"]{定义递推数据类型的工具}

对复杂的数据类型，按照上述步骤设计接口很快就会使人厌倦。本节介绍用 Scheme 自动设
计和实现接口的工具。这个工具产生的接口与前一节的虽不完全相同，却很类似。

@eopl-index[#:range-mark 'start (eopl-index-entry @elem{@tt{define-datatype} form} "definedatatypeform")]
@eopl-index[#:range-mark 'start "Lambda expression (LcExp)" "Scheme implementation"]
仍考虑前一节讨论的数据类型 lambda 演算表达式。lambda 演算表达式的接口可以这样写：

@eopl-code{
@racketblock[
(@#,elem{@elemtag["lc-exp"]{}}define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
]
}

这里的名字 @tt{var-exp}、@tt{var}、@tt{bound-var}、@tt{app-exp}、@tt{rator} 和
@tt{rand} 分别是 @term["variable expression"]{变量表达式}、@term["variable"]{变
量}、@term["bound variable"]{绑定变量}、@term["application expression"]{调用表达
式}、@term["operator"]{操作符} 和@term["operand"]{操作数} 的缩写。

这些表达式声明了三种构造器：@tt{var-exp}、@tt{lambda-exp} 和 @tt{app-exp}，以及
一个谓词 @tt{lc-exp?}。三个构造器用谓词 @tt{identifier?} 和 @tt{lc-exp?} 检查它
们的参数，确保参数合法。所以，如果生成 lc-exp 时只用这些构造器，可以确保表达式及
其所有子表达式合法。如此一来，处理 lambda 表达式时就能跳过许多检查。

@eopl-index[(eopl-index-entry @elem{@tt{cases} form} "Casesform")]
@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{occurs-free?}} "occursfree"]]
我们用形式 @tt{cases} 代替谓词和提取器，判断数据类型的实例属于哪种变体，并提取出
它的组件。为解释这一形式，我们用数据类型 @tt{lc-exp} 重写
@tt{occurs-free?}（@pageref{occurs-free?}）：

@eopl-code{
@racketblock[
@#,elem{@${@bold{@tt{occurs-free?}} : \mathit{Sym} \times \mathit{LcExp} \to \mathit{Bool}}}
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp
        (var) (eqv? var search-var))
      (lambda-exp
        (bound-var body)
        (and
          (not (eqv? search-var bound-var))
          (occurs-free? search-var body)))
      (app-exp
        (rator rand)
        (or
          (occurs-free? search-var rator)
          (occurs-free? search-var rand))))))
]
}

要理解它，假设 @tt{exp} 是由 @tt{app-exp} 生成的 lambda 演算表达式。根据
@tt{exp} 的取值，分支 @tt{app-exp} 将被选中，@tt{rator} 和 @tt{rand} 则绑定到两
个子表达式，接着，表达式

@nested{
@eopl-code{
@racketblock[
(or
  (occurs-free? search-var rator)
  (occurs-free? search-var rand))
]
}

将会求值，就像我们写：

@eopl-code{
@racketblock[
(if (app-exp? exp)
  (let ((rator (app-exp->rator exp))
        (rand (app-exp->rand exp)))
    (or
      (occurs-free? search-var rator)
      (occurs-free? search-var rand)))
...)
]
}

递归调用 @tt{occurs-free?} 像这样完成运算。
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{occurs-free?}} "occursfree"]]
}

一般的 @tt{define-datatype} 声明形如：

@nested[#:style 'noindent]{
@racketblock[
(@#,elem{@elemtag["define-datatype"]{}}define-datatype @#,elem{@${type\mbox{-}name}} @#,elem{@${type\mbox{-}predicate\mbox{-}name}}
  @#,elem{@${\{@tt["("]variant\mbox{-}name \quad \{@tt["("]filed\mbox{-}name \quad predicate@tt[")"]\}^{*} @tt[")"]\}^{+}}})
]
}

这新定义了一种数据类型，名为 @${type\mbox{-}name}，它有一些@term["variants"]{变
体}。每个变体有一变体名，以及 0 或多个字段，每个字段各有其字段名和相应的谓词。不
论是否属于不同的类型，变体都不能重名。类型也不能重名，且类型名不能用作变体名。每
个字段的谓词必须是一个 Scheme 谓词。

每个变体都有一个构造器过程，用于创建该变体的值。这些过程的名字与对应的变体相同。
如果一个变体有 @${n} 个字段，那么它的构造器取 @${n} 个参数，用对应的谓词检查每个
参数值，并返回变体值，值的第 @${i} 个字段为第 @${i} 个参数值。

@${type\mbox{-}predicate\mbox{-}name} 绑定到一个谓词。这个谓词判断其参数值是否是
相应的类型。

记录可以用只有一种变体的数据类型定义。为了区分只有一种变体的数据类型，我们遵循一
种命名惯例：当只有一个变体时，我们以 @tt{a-@${type\mbox{-}name}}
@eopl-index[@eopl-index-entry[@elem{@tt{a(n)-@${\mathit{type\mbox{-}name}}} constructor} "antypename"]] 或
@tt{an-@${type\mbox{-}name}} 命名构造器；否则，以
@${variant\mbox{-}name\mbox{-}type\mbox{-}name} 命名构造器。

@eopl-index["Mutual recursion"]
由 @tt{define-datatype} 生成的数据结构可以互递归。例如，@secref{s1.1}中的 s-list
语法为：

@envalign*{\mathit{S\mbox{-}list} &::= {\normalfont{@tt{(@m{\{\mathit{S\mbox{-}exp}\}^{*}})}}} \\
           \mathit{S\mbox{-}exp} &::= \mathit{Symbol} \mid \mathit{S\mbox{-}list}}

s-list中的数据可以用数据类型 @tt{s-list}表示为：

@eopl-code{
@racketblock[
(define-datatype s-list s-list?
  (empty-s-list)
  (non-empty-s-list
    (first s-exp?)
    (rest s-list?)))

(define-datatype s-exp s-exp?
  (symbol-s-exp
    (sym symbol?))
  (s-list-s-exp
    (slst s-list?)))
]
}

数据类型 @tt{s-list} 用 @tt{(empty-s-list)} 和 @tt{non-empty-s-list} 代替
@tt{()} 和 @tt{cons} 来表示列表。如果我们还想用 Scheme 列表，可以写成：

@nested{
@eopl-code{
@racketblock[
(define-datatype s-list s-list?
  (an-s-list
    (sexps (list-of s-exp?))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
        (and (pair? val)
          (pred (car val))
          ((list-of pred) (cdr val)))))))
]
}

这里 @tt{(list-of @${pred})} 生成一个谓词，检查其参数值是否是一个列表，且列表的
每个元素都满足 @${pred}。

}

@eopl-index[(eopl-index-entry @elem{@tt{cases} form} "Casesform")]
一般的 @tt{cases} 语法为：

@nested[#:style 'noindent]{
@racketblock[
(cases @#,elem{@${type\mbox{-}name}} @#,elem{@${expression}}
  @#,elem{@${\{@tt["("]variant\mbox{-}name \phantom{x} @tt["("]\{filed\mbox{-}name\}^{*}@tt[")"] \phantom{x} consequent@tt[")"]\}^{*}}}
  (else @#,elem{@${default}}))
]

该形式指定类型，一个待求值和检查的表达式，以及一些从句。每个从句以指定类型的某一
变体名及相应字段名为标识。@tt{else} 从句可有可无。首先求 @${expression} 的值，得
到 @${type\mbox{-}name} 的某个值 @${v}。如果 @${v} 是某个
@${variant\mbox{-}name} 的变体，那就选中对应的从句。各 @${type\mbox{-}name} 绑定
到 @${v} 中对应的字段值。然后在这些绑定的作用域内求取并返回 @${consequent} 的值。
如果 @${v} 不属于任何变体，且有 @tt{else} 从句，则求取并返回 @${default} 的值。
如果没有 @tt{else} 从句，必须为指定数据类型的@emph{每个}变体指定从句。

}

形式 @tt{cases} 根据位置绑定变量： 第 @${i} 个变量绑定到第 @${i} 个字段。所以，
我们可以用：

@nested{

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(app-exp (exp1 exp2)
  (or
    (occurs-free? search-var exp1)
    (occurs-free? search-var exp2)))
}
}

代替

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(app-exp (rator rand)
  (or
    (occurs-free? search-var rator)
    (occurs-free? search-var rand)))
}
}

}

@tt{define-datatype} 和 @tt{cases} 形式提供了一种简洁的方式来定义递推数据类型，
但这种方式并不是唯一的。根据使用场景，可能得用专门的表示方式，它们利用数据的特殊
性质，更紧凑或者更高效。要获得这些优势，代价是不得不动手实现接口中的过程。

@eopl-index[#:range-mark 'start "Domain-specific languages"]
@tt{define-datatype} 形式是@term["domain-specific language"]{特定领域语言} 的例
子。特定领域语言是一种小巧的语言，用来描述小而明确的任务中的单一任务。本例中的任
务是定义一种递推数据类型。这种语言可能像 @tt{define-datatype} 一样，存在于通用语
言中；也可能是一门单独的语言，别有一套工具。一般来说，创造这类语言首先要找出任务
的不同变体，然后设计语言，描述这些变体。这种策略通常非常有效。
@eopl-index[#:range-mark 'end (eopl-index-entry @elem{@tt{define-datatype} form} "definedatatypeform")]
@eopl-index[#:range-mark 'end "Domain-specific languages"]
@eopl-index[#:range-mark 'end "Lambda expression (LcExp)" "Scheme implementation"]

@exercise[#:level 1 #:tag "ex2.21"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.21"] @eopl-index-entry[@elem{Environment ADT (@${\mathit{Env}})} "EnvironmentADT"]]
用 @tt{define-datatype} 实现@secref{s2.2.2}中的环境数据类型。然后
实现@exercise-ref{ex2.9} 中的 @tt{has-binding?}。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.21"] @eopl-index-entry[@elem{Environment ADT (@${\mathit{Env}})} "EnvironmentADT"]]

}

@exercise[#:level 1 #:tag "ex2.22"]{

用 @tt{define-datatype} 实现@exercise-ref{ex2.4} 中的堆栈数据类型。

}

@exercise[#:level 1 #:tag "ex2.23"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.23"] "Lambda expression (LcExp)"]
@tt{lc-exp} 的定义忽略了@definition-ref{d1.1.8} 中的条件：
@exact-elem{“}@${\mathit{Identifier}} 是除 @tt{lambda} 之外的任何符号。
@exact-elem{”}修改 @tt{identifier?} 的定义，补充这一条件。提示：任何谓词都能在
@tt{define-datatype} 中使用，你定义的也能。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.23"] "Lambda expression (LcExp)"]

}

@exercise[#:level 1 #:tag "ex2.24"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.24" "ex2.25"] (eopl-index-entry @elem{Binary tree (@${\mathit{Bintree}})} "Binarytree")]
这是用 @tt{define-datatype} 表示的二叉树：

@eopl-code{
@racketblock[
(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))
]
}

实现操作二叉树的过程 @tt{bintree-to-list}，则 @tt{(bintree-to-list
(interior-node 'a (leaf-node 3) (leaf-node 4)))} 应返回列表：

@eopl-code{
@racketblock[
(interior-node
  a
  (leaf-node 3)
  (leaf-node 4))
]
}

}

@(define max-interior-eval
(parameterize ([sandbox-output 'string]
               [sandbox-error-output 'string]
               [sandbox-memory-limit 50])
  (make-evaluator
   'eopl

'(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))
'(define bintree-sum
  (lambda (tree)
    (cases bintree tree
           (leaf-node
            (num)
            num)
           (interior-node
            (key left right)
            (+ (bintree-sum left)
               (bintree-sum right))))))
'(define max-interior
  (lambda (tree)
    (cases bintree tree
           (leaf-node
            (num)
            (eopl:error 'bintree
                        "Cannot get max interior from a leaf ~s!" tree))
           (interior-node
            (key left right)
            (let* ((left-sum (bintree-sum left))
                   (right-sum (bintree-sum right))
                   (total-sum (+ left-sum right-sum)))
              (cond ((and (interior-node? left)
                          (interior-node? right))
                     (if (> left-sum right-sum)
                         (if (> left-sum total-sum)
                             (max-interior left)
                             key)
                         (if (> right-sum total-sum)
                             (max-interior right)
                             key)))
                    ((interior-node? left)
                     (if (> left-sum total-sum)
                         (max-interior left)
                         key))
                    ((interior-node? right)
                     (if (> right-sum total-sum)
                         (max-interior right)
                         key))
                    (else key)))))))
'(define interior-node?
  (lambda (tree)
    (cases bintree tree
           (interior-node
            (key left right)
            #t)
           (else #f)))))))

@exercise[#:level 2 #:tag "ex2.25"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.25"] (eopl-index-entry @elem{@tt{cases} form} "Casesform")]
用 @tt{cases} 写出 @tt{max-interior}，它取至少有一个内部节点的整数二叉树（像前一
道练习那样），返回叶子之和最大的内部节点对应的标签。

@eopl-code{
@examples[#:eval max-interior-eval
          #:label #f
          (define tree-1
            (interior-node 'foo (leaf-node 2) (leaf-node 3)))
          (define tree-2
            (interior-node 'bar (leaf-node -1) tree-1))
          (define tree-3
            (interior-node 'baz tree-2 (leaf-node 1)))
          (max-interior tree-2)
          (max-interior tree-3)
]
}

最后一次调用 @tt{max-interior} 也可能返回 @tt{foo}，因为节点 @tt{foo} 和
@tt{baz} 的叶子之和都为 5。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.24" "ex2.25"] (eopl-index-entry @elem{Binary tree (@${\mathit{Bintree}})} "Binarytree")]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.25"] (eopl-index-entry @elem{@tt{cases} form} "Casesform")]

}

@exercise[#:level 2 #:tag "ex2.26"]{

@exercise-ref{ex1.33} 还有一种写法。树的集合可以用下列语法定义：

@envalign*{\mathit{Red\mbox{-}blue\mbox{-}tree} &::= \mathit{Red\mbox{-}blue\mbox{-}subtree} \\
           \mathit{Red\mbox{-}blue\mbox{-}subtree} &::= @tt{(red-node @m{\mathit{Red\mbox{-}blue\mbox{-}subtree}} @m{\mathit{Red\mbox{-}blue\mbox{-}subtree}})} \\
                                          &::= @tt{(blue-node @m{\{\mathit{Red\mbox{-}blue\mbox{-}subtree}\}^{*}})} \\
                                          &::= @tt{(leaf-node @m{\mathit{Int}})}
}

用 @tt{define-datatype} 写出等价定义，用得到的接口写出一个过程，它取一棵树，生成
形状相同的另一棵树，但把每片叶子的值改为从当前叶子节点到树根之间红色节点的数目。

}

@section[#:style section-title-style-numbered #:tag "s2.5"]{抽象语法及其表示}

@eopl-index[#:range-mark 'start "Lambda expression (LcExp)" "abstract vs. concrete syntax"]
@eopl-figure[#:position "!t"]{
@centered{
@(image "../images/ast"
  #:suffixes (list ".pdf" ".svg")
  (tt "(lambda (x) (f (f x)))") "的抽象语法树")
}

@eopl-caption["fig-2.2"]{@tt{(lambda (x) (f (f x)))} 的抽象语法树}
}

@eopl-index[#:range-mark 'start "Abstract syntax"]
@eopl-index[#:range-mark 'start "Concrete syntax"]
语法通常指定归纳式数据类型的某一具体表示，后者使用前者生成的字符串或值。这种表示
叫做@term["concrete syntax"]{具体语法}，或@term["external"]{外在} 表示。

例如，@definition-ref{d1.1.8} 指定集合 lambda 演算表达式，用的就是 lambda 演算表
达式的具体语法。我们可以用其他具体语法表示 lambda 演算表达式。例如，可以用

@nested{

@elemtag["lambda-2"]{}@envalign*{\mathit{Lc\mbox{-}exp} &::= \mathit{Identifier} \\
                         &::= @tt{proc @m{\mathit{Identifier}} @tt{=>} @m{\mathit{Lc\mbox{-}exp}}} \\
                         &::= @tt{@m{\mathit{Lc\mbox{-}exp}} (@m{\mathit{Lc\mbox{-}exp}})}}

把 lambda 演算表达式定义为另一个字符串集合。

}

为了处理这样的数据，需要将其转换为@term["internal"]{内在} 表示。
@tt{define-datatype} 形式提供了一种简洁的方式来定义这样的内在表示。我们称之
为@term["abstract syntax"]{抽象语法}。在抽象语法中，不需要存储括号之类的终止符，
因为它们不传达信息。另一方面，我们要确保数据结构足以区分它所表示的 lambda 演算表
达式，并提取出各部分。@pageref{lc-exp}的数据类型 @tt{lc-exp} 助我们轻松实现这些。

@eopl-index[#:range-mark 'start "Abstract syntax tree"]
@eopl-index[#:range-mark 'start "Nonterminal symbols"]
将内在表示形象化为@term["abstract syntax tree"]{抽象语法树} 也很不错。
@figure-ref{fig-2.2} 展示了一棵抽象语法树，它代表数据类型 @tt{lc-exp} 表示的
lambda 演算表达式 @tt{(lambda (x) (f (f x)))}。树的每个内部节点以相应的生成式名
字为标识。树枝以所出现的非终结符名字为标识。叶子对应终止符字符串。

要为某种具体语法设计抽象语法，需要给其中的每个生成式，以及生成式中出现的每个非终
止符命名。很容易将抽象语法写成 @tt{define-datatype} 声明。我们为每个非终结符添加
一个 @tt{define-datatype}，为每个生成式添加一个变体。
@eopl-index[#:range-mark 'end "Nonterminal symbols"]

@figure-ref{fig-2.2} 中挑出的内容可以精确表示如下：

@elemtag["lc-grammar2"]{}@envalign*{\mathit{Lc\mbox{-}Exp} &::= \mathit{Identifier} \\[-3pt]
                                  &\mathrel{\phantom{::=}} \fbox{@tt{var-exp (var)}} \\[5pt]
                                  &::= \normalfont{@tt{(lambda (@m{\mathit{Identifier}}) @m{\mathit{Lc\mbox{-}Exp}})}} \\[-3pt]
                                  &\mathrel{\phantom{::=}} \fbox{@tt{lambda-exp (bound-var body)}} \\[5pt]
                                  &::= \normalfont{@tt{(@m{\mathit{Lc\mbox{-}Exp}} @m{\mathit{Lc\mbox{-}Exp}})}} \\[-3pt]
                                  &\mathrel{\phantom{::=}} \fbox{@tt{app-exp (rator rand)}}}

本书采用这种表示，同时指明具体语法和抽象语法。

具体语法主要供人使用，抽象语法主要供计算机使用，既已区分二者，现在来看看如何将一
种语法转换为另一种。

当具体语法是个字符串集合，推导出对应的抽象语法树可能相当棘手。这一任务
叫做@term["parsing"]{解析}，由@term["parser"]{解析器} 完成。因为写解析器通常比较
麻烦，所以最好借由工具@term["parser generator"]{解析器生成器} 完成。
@eopl-index["Parser generator"]
@eopl-index["Parsing"]
解析器生成器以一套语法作为输入，产生一个解析器。由于语法是由工具处理的，它们必需
以某种机器能够理解的语言写成，即写语法用的特定领域语言。有很多现成的解析器生成器。
@eopl-index["Domain-specific languages"]

@eopl-index[#:range-mark 'end "Concrete syntax"]
如果具体语法以列表集合的形式给出，解析过程就会大大简化。比如，
和@pageref{define-datatype} @tt{define-datatype} 的语法类似，
@elemref["lambda-2"]{本节开头}的 lambda 演算表达式指定了一个列表集合。这样，Scheme
过程 @tt{read} 会自动把字符串解析为列表和符号。然后，把这些列表结构解析为抽象语
法树就容易多了，就像 @tt{parse-expression} 这样。

@eopl-code{
@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{parse-expression}} "parseexpression"]]
@racketblock[
@#,elem{@bold{@tt{parse-expression}} : @${\mathit{SchemeVal} \to \mathit{LcExp}}}
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
        (if (eqv? (car datum) 'lambda)
          (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
          (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (report-invalid-concrete-syntax datum)))))
 ]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{parse-expression}} "parseexpression"]]
}

@eopl-index[#:range-mark 'start @eopl-index-entry[@bold{@tt{unparse-lc-exp}} "unparselcexp"]]
通常，很容易把抽象语法树重新转换为列表-符号表示。我们这样做了，Scheme 的打印过程
就会将其显示为列表形式的具体语法。这由 @tt{unparse-lc-exp} 完成：

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{unparse-lc-exp}} : @${\mathit{LcExp} \to \mathit{SchemeVal}}}
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
      (var-exp (var) var)
      (lambda-exp
        (bound-var body)
        (list 'lambda
          (list bound-var)
          (unparse-lc-exp body)))
      (app-exp
        (rator rand)
        (list (unparse-lc-exp rator)
          (unparse-lc-exp rand))))))
]
@eopl-index[#:range-mark 'end "Abstract syntax"]
@eopl-index[#:range-mark 'end "Abstract syntax tree"]
@eopl-index[#:range-mark 'end "Lambda expression (LcExp)" "abstract vs. concrete syntax"]
@eopl-index[#:range-mark 'end @eopl-index-entry[@bold{@tt{unparse-lc-exp}} "unparselcexp"]]
}

@exercise[#:level 1 #:tag "ex2.27"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.27" "ex2.28" "ex2.29" "ex2.30"] "Lambda expression (LcExp)"]
画出下面 lambda 演算表达式的抽象语法树：

@eopl-code{
@racketblock[
((lambda (a) (a b)) c)

(lambda (x)
  (lambda (y)
    ((lambda (x)
       (x y))
     x)))
]
}
}

@exercise[#:level 1 #:tag "ex2.28"]{

@eopl-index[#:suffix @exer-ref-range["ex2.28"] @eopl-index-entry[@bold{@tt{unparse-lc-exp}} "unparselcexp"]]
写出反向解析器，将lc-exp的抽象语法转换为符合本节第二个语法
（@pageref{lc-grammar2}）的字符串。

}

@exercise[#:level 1 #:tag "ex2.29"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.29"] "Kleene plus"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.29"] "Kleene star (closure)"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.29" "ex2.30"] @eopl-index-entry[@bold{@tt{parse-expression}} "parseexpression"]]
当具体语法使用克莱尼星号或加号（@pageref{kleene-star}）时，生成抽象语法树时最好
使用相应子树的@emph{列表}。例如，如果 lambda 演算表达式的语法为：

@nested[#:style 'noindent]{

@envalign*{\mathit{Lc\mbox{-}Exp} &::= \mathit{Identifier} \\[-3pt]
                                  &\mathrel{\phantom{::=}} \fbox{@tt{var-exp (var)}} \\[5pt]
                                  &::= \normalfont{@tt{(lambda (@m{\{\mathit{Identifier}\}^{*}}) @m{\mathit{Lc\mbox{-}Exp}})}} \\[-3pt]
                                  &\mathrel{\phantom{::=}} \fbox{@tt{lambda-exp (bound-vars body)}} \\[5pt]
                                  &::= \normalfont{@tt{(@m{\mathit{Lc\mbox{-}Exp}} @m{\{\mathit{Lc\mbox{-}Exp}\}^{*}})}} \\[-3pt]
                                  &\mathrel{\phantom{::=}} \fbox{@tt{app-exp (rator rands)}}}

那么字段 @tt{bound-vars} 的谓词可写作 @tt{(list-of identifier?)}，@tt{rands} 的
谓词可写作 @tt{(list-of lc-exp?)}。以这种方式写出该语法的 @tt{define-datatype}
和解析器。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.29"] "Kleene plus"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.29"] "Kleene star (closure)"]
}
}

@exercise[#:level 2 #:tag "ex2.30"]{

上面定义的过程 @tt{parse-expression} 很不可靠：它检查不到某些可能的语法错误，例
如 @tt{(a b c)}，并且因其他表达式终止时给不出恰当的错误信息，如 @tt{(lambda)}。
修改一下，使之更健壮，可接受任何s-exp，并且对不表示 lambda 演算表达式的 s-exp 给
出恰当的错误信息。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.27" "ex2.28" "ex2.29" "ex2.30"] "Lambda expression (LcExp)"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.29" "ex2.30"] @eopl-index-entry[@bold{@tt{parse-expression}} "parseexpression"]]

}

@exercise[#:level 2 #:tag "ex2.31"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex2.31"] @eopl-index-entry[@elem{Prefix lists (@${\mathit{Prefix\mbox{-}list}})} "Prefixlists"]]
有时，把具体语法定义为括号包围的符号和整数序列很有用。例如，可以把
集合@term["prefix list"]{前缀列表} 定义为：

@nested[#:style 'noindent]{

@envalign*{\mathit{Prefix\mbox{-}list} &::= @tt{(@m{\mathit{Prefix\mbox{-}exp}})} \\
           \mathit{Prefix\mbox{-}exp}  &::= \mathit{Int} \\
                                       &::= @tt{- @m{\mathit{Prefix\mbox{-}exp}} @m{\mathit{Prefix\mbox{-}exp}}}}

那么 @tt{(- - 3 2 - 4 - 12 7)}是一个合法的前缀列表。有时为纪念其发明者Jan
Łukasiewicz，称之为@term["Polish prefix notation"]{波兰前缀表示法}。写一个
解析器，将前缀列表表示法转换为抽象语法：
@eopl-index[#:suffix @exer-ref-range["ex2.31"] @eopl-index-entry["Łukasiewicz, Jan" "LukasiewiczJan"]]
@eopl-index[#:suffix @exer-ref-range["ex2.31"] "Polish prefix notation"]

@eopl-code{
@racketblock[
(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))
]
}

使上例与这几个构造器生成相同抽象语法树：

@eopl-code{
@racketblock[
(diff-exp
 (diff-exp
  (const-exp 3)
  (const-exp 2))
 (diff-exp
  (const-exp 4)
  (diff-exp
   (const-exp 12)
   (const-exp 7))))
]
}

提示：想想如何写一个过程，取一列表，产生一个 @tt{prefix-exp} 和列表剩余元素组成
的列表。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex2.31"] @eopl-index-entry[@elem{Prefix lists (@${\mathit{Prefix\mbox{-}list}})} "Prefixlists"]]

}

}
