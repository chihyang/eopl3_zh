#lang scribble/book
@(require "style.rkt")
@(require latex-utils/scribble/math)
@(require latex-utils/scribble/utils)
@(require scribble/manual)
@(require scribble-math)

@title[#:style 'numbered #:tag "da"]{数据抽象}

@section[#:tag "sdvi"]{用接口定义数据}

每当我们想以某种方式表示一些量时，我们就新定义了一种数据类型：它的取值是其表示，
它的操作是处理其实例的过程。

实例的表示通常很复杂，所以如能避免，我们不愿关心其细节。我们可能想改变数据的表示。
最高效的表示往往难以实现，所以我们可能希望先简单实现，只在确知系统的整体性能与之
攸关时，才改用更高效的表示。不管出于什么原因，如果我们决定改变某些数据的表示方式，
我们得能够定位程序中所有依赖表示方式的部分。这就需要借助技巧@emph{数据抽象}
(@emph{data abstraction}) 。

数据抽象将数据类型分为两部分：@emph{接口} (@emph{interface}) 和 @emph{实现}
(@emph{implementation})。接口告诉我们某类型表示什么数据，能对数据做什么处理，以
及由这些处理得出的性质。@emph{实现}给出数据的具体表示方式，以及处理数据表示的代
码。

这样抽象出的数据类型称为@emph{抽象数据类型} (@emph{abstract data type})。程序的
其余部分，即数据类型的@emph{客户} (@emph{client})，只能通过接口中指定的操作处理
新数据。这样一来，如果我们希望改变数据的表示方式，只须改变处理接口的实现。

这一想法并不陌生：我们写程序处理文件时，多数时候只关心能否调用过程来打开，关闭，
读取文件或对文件做其他操作。同样地，大多数时候，我们不关心整数在机器中究竟怎样表
示，我们只关心能否可靠地执行算数操作。

当客户只能通过接口提供的过程处理某类型的数据时，我们说客户代码与@emph{表示无关}
(@emph{representation-independent})，因为这些代码不依赖数据类型的表示。

因此，所有关于数据表示的信息必须在实现代码之中。实现最重要的部分就是指定数据如何
表示。我们用符号@${\lceil v \rceil}指代“数据 @${v} 的表示”。

要说得更直白些，来看一个简单例子：自然数类型。要表示的数据是自然数。接口由四个过
程组成：@tt{zero}，@tt{is-zero?}，@tt{successor} 和 @tt{predecessor}。当然，不是
随便几个过程都可以作为这一接口的实现。当且仅当一组过程满足如下四个方程时，可以作
为@tt{zero}，@tt{is-zero?}，@tt{successor} 和 @tt{predecessor}的实现：

 @tt{(zero)} = @${\lceil 0 \rceil}

 @tt{(is-zero? @${\lceil n \rceil})} = @m{@env["cases"]{@tt{#t} & n = 0 \\
                                                        @tt{#f} & n \neq 0}}

 @tt{(successor @${\lceil n \rceil})} = @${\lceil n + 1 \rceil \quad (n \geq 0)}

 @tt{(predecessor @${\lceil n + 1 \rceil})} = @${\lceil n \rceil \quad (n \geq 0)}

这一定义没有指明自然数应当如何表示。它只要求这些过程都产生指定的行为。即，过程
@tt{zero}必须返回0的表示；给定数字@${n}的表示，过程 @tt{successor} 必须返回数字
@${n + 1} 的表示，等等。此定义未对 @tt{(predecessor (zero))} 作出说明，所以这一
定义下，任何行为都是可以接受的。

现在可以写出客户程序处理自然数，而且不论用哪种表示方式，都能保证得出正确
的结果。例如：

@nested{@racketblock[
(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        (successor (plus (predecessor x) y)))))
]

满足 @tt{(plus @${\lceil x \rceil} @${\lceil y \rceil}) @${=} @${\lceil x +
y\rceil}}，不论怎样实现自然数。}

大多数接口都包含：一些@emph{构造器} (@emph{constructor})，用来产生数据类型的元素；
一些@emph{观测器} (@emph{observer})，用来从数据类型的值中提取信息。这里，我们有
三个构造器，@tt{zero}，@tt{successor} 和 @tt{predecessor}，一个观察器，
@tt{is-zero?}。

可以用多种方式表示这套接口，我们选三种。

@itemlist[#:style 'ordered

 @item{@emph{单元表示法} (@emph{Unary representation})：在单元表示法中，自然数
 @${n}由@${n}个@tt{#t}组成的列表表示。所以，0表示为@tt{()}，1表示为@tt{(#t)}，2
 表示为@tt{(#t #t)}，等等。可以用归纳法定义这种表示方式：

 @nested[#:style 'inset]{@${\lceil 0 \rceil = @tt{()}}

 @${\lceil n + 1 \rceil = @tt{(#t . @${\lceil n \rceil})}}}

 要满足该表示的定义，过程可以写成：

 @racketblock[(define zero (lambda () '()))
 (define is-zero? (lambda (n) (null? n)))
 (define successor (lambda (n) (cons #t n)))
 (define predecessor (lambda (n) (cdr n)))]}

 @item{@emph{Scheme数字表示法} (@emph{Scheme number representation})：在这种表示
 中，只需用Scheme内部的数字表示法（本身可能十分复杂！）。令 @${\lceil n \rceil}
 为Scheme整数 @tt{n}，则所需的四个过程可以定义为：

 @racketblock[(define zero (lambda () 0))
 (define is-zero? (lambda (n) (zero? n)))
 (define successor (lambda (n) (+ n 1)))
 (define predecessor (lambda (n) (- n 1)))]

 }

 @item{@emph{大数表示法} (@emph{Bignum representation})：在大数表示法中，大整数
 @${N}用其底数表示。}

]
