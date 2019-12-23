#lang scribble/book
@(require "style.rkt")
@(require latex-utils/scribble/math)
@(require latex-utils/scribble/utils)
@(require scribble/manual)
@(require scribble-math)
@(require scribble/example)
@(require scribble/core)

@title[#:style 'numbered #:tag "da"]{数据抽象}

@section[#:tag "sdvi"]{用接口定义数据}

每当我们想以某种方式表示一些量时，我们就新定义了一种数据类型：它的取值是其表示，
它的操作是处理其实例的过程。

实例的表示通常很复杂，所以如能避免，我们不愿关心其细节。我们可能想改变数据的表示。
最高效的表示往往难以实现，所以我们可能希望先简单实现，只在确知系统的整体性能与之
攸关时，才改用更高效的表示。不管出于什么原因，如果我们决定改变某些数据的表示方式，
我们得能定位程序中所有依赖表示方式的部分。这就需要借助技巧@emph{数据抽象}
(@emph{data abstraction}) 。

数据抽象将数据类型分为两部分：@emph{接口} (@emph{interface}) 和 @emph{实现}
(@emph{implementation})。接口告诉我们某类型表示什么数据，能对数据做什么处理，以
及可由这些处理得出的性质。@emph{实现}给出数据的具体表示方式，以及处理数据表示的
代码。

这样抽象出的数据类型称为@emph{抽象数据类型} (@emph{abstract data type})。程序的
其余部分——数据类型的@emph{客户} (@emph{client}) ——只能通过接口中指定的操作处理新
数据。这样一来，如果我们希望改变数据的表示方式，只须改变数据处理接口的实现。

这一想法并不陌生：我们写程序处理文件时，多数时候只关心能否调用过程来打开，关闭，
读取文件或对文件做其他操作。同样地，大多数时候，我们不关心整数在机器中究竟怎样表
示，只关心能否可靠地执行算数操作。

当客户只能通过接口提供的过程处理某类型的数据时，我们说客户代码与@emph{表示无关}
(@emph{representation-independent})，因为这些代码不依赖数据类型的表示。

因此，所有关于数据表示的信息必须在实现代码之中。实现最重要的部分就是指定数据如何
表示。我们用符号@${\lceil v \rceil}指代“数据 @${v} 的表示”。

要说得更直白些，来看一个简单例子：自然数类型。要表示的数据是自然数。接口由四个过
程组成：@tt{zero}，@tt{is-zero?}，@tt{successor} 和 @tt{predecessor}。当然，不是
随便几个过程都可以作为这一接口的实现。当且仅当一组过程满足如下四个方程时，可以作
为@tt{zero}，@tt{is-zero?}，@tt{successor} 和 @tt{predecessor}的实现：

 @nested[#:style 'inset]{
  @tt{(zero)} = @${\lceil 0 \rceil}

  @tt{(is-zero? @${\lceil n \rceil})} = @m{@env["cases"]{@tt{#t} & n = 0 \\
                                                         @tt{#f} & n \neq 0}}

  @tt{(successor @${\lceil n \rceil})} = @${\lceil n + 1 \rceil \quad (n \geq 0)}

  @tt{(predecessor @${\lceil n + 1 \rceil})} = @${\lceil n \rceil \quad (n \geq 0)}}

这一定义没有指明自然数应当如何表示，它只要求这些过程都产生指定的行为。即，过程
@tt{zero}必须返回@${0}的表示；给定数字@${n}的表示，过程 @tt{successor} 必须返回
数字 @${n + 1} 的表示，等等。此定义未对 @tt{(predecessor (zero))} 做出说明，所以
这一定义下，任何行为都是可以接受的。

现在可以写出处理自然数的客户程序，而且不论用哪种表示方式，都保证能得出正确的结果。
例如，不论怎样实现自然数：

@nested{@racketblock[
(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (plus (predecessor x) y)))))
]

满足 @tt{(plus @${\lceil x \rceil} @${\lceil y \rceil}) @${=} @${\lceil x +
y\rceil}}。}

大多数接口都包含：若干@emph{构造器} (@emph{constructor})，用来产生数据类型的元素；
若干@emph{观测器} (@emph{observer})，用来从数据类型的值中提取信息。这里有三个构
造器，@tt{zero}，@tt{successor} 和 @tt{predecessor}，一个观测器，@tt{is-zero?}。

可以用多种方式表示这套接口，我们考虑其中三种。

@itemlist[#:style 'ordered

 @item{@emph{单元表示法} (@emph{Unary representation})：在单元表示法中，自然数
 @${n}由@${n}个@tt{#t}组成的列表表示。所以，@${0} 表示为 @tt{()}，@${1}表示为
 @tt{(#t)}，@${2}表示为@tt{(#t #t)}，等等。可以用归纳法定义这种表示方式：

 @nested[#:style 'inset]{@${\lceil 0 \rceil = @tt{()}}

 @${\lceil n + 1 \rceil = @tt{(#t . @${\lceil n \rceil})}}}

 要满足该表示的定义，数据处理过程可以写成：

 @racketblock[(define zero (lambda () '()))
 (define is-zero? (lambda (n) (null? n)))
 (define successor (lambda (n) (cons #t n)))
 (define predecessor (lambda (n) (cdr n)))]}

 @item{@emph{Scheme数字表示法} (@emph{Scheme number representation})：在这种表示
 中，只需用Scheme内置的数字表示法（本身可能十分复杂！）。令 @${\lceil n \rceil}
 为Scheme整数 @tt{n}，则所需的四个过程可以定义为：

 @racketblock[(define zero (lambda () 0))
 (define is-zero? (lambda (n) (zero? n)))
 (define successor (lambda (n) (+ n 1)))
 (define predecessor (lambda (n) (- n 1)))]

 }

 @item{@emph{大数表示法} (@emph{Bignum representation})：在大数表示法中，数值以
 @${N}进制表示，@${N}是某个大整数。该方法以 @${0} 到 @${N-1} 之间的数字（有时叫
 做@emph{大位} (@emph{bigits}) 而非数位）组成的列表表示数值，这就很容易表示远超
 机器字长的整数。这里，为了便于使用，我们把最低位放在列表的最前端。这种表示法可
 以用归纳法定义为：

 @m{\lceil n \rceil = @env["cases"]{@tt{()} & n = 0 \\ @tt{(@m{r} . @m{\lceil q
                                    \rceil})} & n = qN + r, 0 \leqslant r < N}}

 所以，如果 @${N = 16}，那么@${\lceil 33 \rceil = @tt{(1 2)}}，@${\lceil 258
 \rceil = @tt{(2 0 1)}}，因为：

 @$${258 = 2 \times 16^0 + 0 \times 16^1 + 1 \times 16^2}}

]

这些实现都没有强制数据抽象：无法防止客户程序查看和判断表示用的是列表还是 Scheme
整数。与之相对，有些语言直接支持数据抽象：它们允许程序员创建新的接口，确保只能通
过接口提供的过程处理新的数据。如果类型的表示隐藏起来，不会因任何操作而暴露（包括
打印），那就说该类型是@emph{模糊} (@emph{opaque}) 的，否则称之为@emph{透明}
(@emph{transparent}) 的。

Scheme 没有提供标准机制来创建新的模糊类型，所以我们退而求其次：定义接口，靠客户
程序的作者小心行事，只使用接口中定义的过程。

在第8章中，我们介绍一些方法，以便加强语言的限制。

@; @exercise[#:difficulty 1 #:tag "ex2.1"]{
@nested[#:style exercise]{
 实现大数表示法的四种操作。然后用你的实现计算10的阶乘。随着参数改变，执行时间如
 何变化？随着进制改变，执行时间如何变化？解释原因。
}

@; @exercise[#:difficulty 2 #:tag "ex2.2"]{
@nested[#:style exercise]{
 客观分析上述表示法。从满足数据类型的定义上来说，它们在何种程度上成功或是失败？
}

@; @exercise[#:difficulty 2 #:tag "ex2.3"]{
@nested[#:style exercise]{

 用差分树表示所有整数（负数和非负数）。差分树是一列表，可用语法定义如下：

 @nested{@$${\mathit{Diff\mbox{-}tree} ::= @tt{one} \mid @tt{(diff
 @${\mathit{Diff\mbox{-}tree}} @${\mathit{Diff\mbox{-}tree}})}}

 列表 @tt{(one)} 表示 1。如果 @${t_1} 表示 @${n_1}，@${t_2} 表示 @${n_2}，那么
 @tt{(diff @${n_1} @${n_2})} 表示 @${n_1 - n_2}。

 所以，@tt{(one)} 和 @tt["(diff"] @tt{(one)} @tt["(diff"] @tt{(one)}
 @tt["(one)))"] 都表示1；@tt["(diff"] @tt["(diff"] @tt{(one)} @tt["(one))"]
 @tt["(one))"] 表示 @${-1}。

 @itemlist[#:style 'ordered

   @item{证明此系统中，每个数都有无限种表示方式。}

   @item{实现这种整数表示法：写出@elem[#:style question]{32页}指定的 @tt{zero}，
   @tt{is-zero?}，@tt{successor} 和 @tt{predecessor}，此外还要能表示负数。这种方
   式下，整数的任何合法表示都应该能作为你过程的参数。例如，你的过程
   @tt{successor}可以接受无限多种 @${1} 的合法表示，且都应给出一个 @${2}的合法表
   示。对 @${1} 的不同合法表示，可以允许给出不同的 @${2} 的合法表示。}

   @item{写出过程 @tt{diff-tree-plus}，用这种表示做加法。你的过程应针对不同的差
   分树进行优化，并在常数时间内得出结果（即与输入大小无关）。特别注意不可使用递
   归。}

   ] }

}

@section[#:tag "rsdt"]{数据类型的表示技巧}

使用数据抽象的程序具有表示无关性：与用来实现抽象数据类型的具体表示方式无关。甚至
可以通过重新定义接口中的一小部分过程来改变表示。在后面的章节中我们常会用到这条性
质。

本节介绍几个表示数据类型的技巧。我们用数据类型@emph{环境} (@emph{environment})
解释这些@elem[#:style question]{选择}。对有限个变量组成的集合，环境将值与其中的
每个元素关联起来。在编程语言的实现之中，环境可用来维系变量与值的关系。编译器也能
用环境记录各个变量名与变量相关信息的关系。

只要能够检查两个变量是否相等，变量能够用我们想用的任何方式表示。我们选用 Scheme
符号表示变量，但在没有符号数据类型的语言中，变量也可以用字符串，哈希表引用，甚至
数字（见3.6节）表示。

@subsection[#:tag "ei"]{环境的接口}

环境是一函数，定义域为有限个变量的集合，值域为所有 Scheme 值的集合。数学上常说的
有限函数是指有序数对组成的有限集合，我们采用这一含义，就得表示形如
@m{\{(var_1,\allowbreak val_1),\allowbreak ...,\allowbreak (var_n, val_n)\}}的所
有集合，其中，@${var_i} 是某一变量，@${val_i} 是任意 Scheme 值。有时称环境
@${env} 中变量 @${var} 的值 @${val} 为其在 @${env} 中的@emph{绑定}
(@emph{binding})。

这一数据类型的接口有三个过程，定义如下：

 @nested{
 @envalign*{&@tt{(empty-env)} &= &\ \lceil \emptyset \rceil \\
            &@tt{(apply-env @m{\lceil f \rceil} @m{var})} &= &\ f(var) \\
            &@tt{(extend-env @m{var} @m{v} @m{\lceil f \rceil})} &= &\ \lceil g \rceil \\
            &\phantom{x} &其中，&\ g(var_1) = @env["cases"]{v & 若\ var_1 = var \\
                                                          f(var_1) & 否则}
 }
 }

过程 @tt{empty-env} 不带参数，必须返回空环境的表示；@tt{apply-env} 用环境对变量
求值；@tt{(extend-env @${var} @${val} @${env})} 产生一个新的环境，除了使变量
@${var} 的值为 @${val} 外，与@${env} 相同。例如，表达式：

 @nested{
 @racketinput[
   (define e
     (extend-env 'd 6
       (extend-env 'y 8
         (extend-env 'x 7
           (extend-env 'y 14
             (empty-env))))))]

 定义了一个环境 @${e}，使 @${e(@tt{d}) = 6}，@${e(@tt{x}) = 7}，@${e(@tt{y}) =
 8}，且对任何其他变量，@${e}未定义。本例中，@tt{y}先绑定到@${14}，随后绑定到
 @${8}。这当然只是生成该环境的多种不同方法之一。}

如同前一个例子，可以将接口中的过程分为构造器和观测器。本例中，@tt{empty-env}和
@tt{extend-env}是构造器，@tt{apply-env}是唯一的观测器。

@; @exercise[#:difficulty 2 #:tag "ex2.4"]{
@nested[#:style exercise]{

 考虑数据类型@emph{堆栈} (@emph{stack})，接口包含过程 @tt{empty-stack}，
 @tt{push}，@tt{pop}, @tt{top}和@tt{empty-stack?}。按照示例中的方式写出这些操作
 的定义。哪些操作是构造器？哪些是观测器？

}

@subsection[#:tag "dsr"]{数据结构的表示}

环境的一种表示可由如下观察得到：生成每个环境都从空环境开始，然后@${n}次应用
@tt{extend-env}，其中@${n \geqslant 0}。例如，

@racketblock[
(extend-env @#,elem{@${var_n}} @#,elem{@${val_n}}
   ...
   (extend-env @#,elem{@${var_1}} @#,elem{@${val_1}}
     (empty-env)))]

所以，每个环境都能通过下列语法描述的表达式生成：

@envalign*{\mathit{Env\mbox{-}exp} &::= @tt{(empty-env)} \\ &::= @tt{(extend-env
                            @m{\mathit{Identifier}}
                            @m{\mathit{Scheme\mbox{-}value}}
                            @m{\mathit{Env\mbox{-}exp}})}}

可以用描述列表集合的语法表示环境，由此得出图2.1中的实现。数据结构@tt{env}表示一
环境，过程@tt{apply-env}查看和判断它表示哪种环境，并做适当操作：如果它表示空环境，
那就报错；如果它表示由@tt{extend-env}生成的环境，那就判断要查找的变量是否与环境
中绑定的某一变量相同，如果相同，则返回保存的值，否则在保存的环境中查找变量。

这是一种常见的代码模式。我们叫它@emph{解释器秘方} (@emph{interpreter recipe})：

@nested[#:style tip]{
 @centered{@bold{解释器秘方}}

 @nested[#:style tip-content]{
 @itemlist[#:style 'ordered

  @item{查看一段数据。}

  @item{判断它表示什么样的数据。}

  @item{提取数据的各个部分，对它们做适当操作。}

   ]}}

@nested[#:style figure]{
@racketblock[
@#,elem{@${Env = @tt{(empty-env)} \mid @tt{(extend-env @${\mathit{Var}} @${\mathit{SchemeVal}} @${\mathit{Env}})}}}
@#,elem{@${Var = Sym}}

@#,elem{@bold{@tt{empty-env}} : @${() \to \mathit{Env}}}
(define empty-env
  (lambda () (list 'empty-env)))

@#,elem{@bold{@tt{extend-env}} : @${\mathit{Var} \times \mathit{SchemeVal} \times \mathit{Env} \to \mathit{Env}}}
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

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
    (eopl:error 'apply-env "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment: ~s" env)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "环境的数据结构表示"))]
}

@; @exercise[#:difficulty 1 #:tag "ex2.5"]{
@nested[#:style exercise]{

只要能区分空环境和非空环境，并能从后者提取出数据片段，可以用任何数据结构表示环境。}
