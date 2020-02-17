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

@title[#:style 'numbered #:tag "cps"]{续文传递风格}

在@secref{cpi}，我们把解释器中的所有主要过程调用重写成@emph{尾调用}。这样，我们
保证任何时候，不论执行的程序多大或多复杂，解释器只使用有限的控制上下文。这种性质
叫做@emph{迭代性控制行为}。

我们通过给每个过程多传一个@emph{续文}参数实现这一目标。这种编程风格叫做@emph{续
文传递风格} (@emph{continuation-passing style})或@deftech{CPS}，且不限于解释器。

本章，我们介绍一种系统性的方法，将任何过程转换为等效的迭代性控制行为过程。要这样，
需要将过程转换为续文传递风格。

@section[#:tag "s6.1"]{写出续文传递风格的程序}

除了写解释器，CPS另有它用。我们考虑老例子阶乘程序：

@racketblock[
(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))
]

阶乘的传递续文版本看起来像是这样：

@nested{
@racketblock[
(define fact
  (lambda (n)
    (fact/k n (end-cont))))

(define fact/k
  (lambda (n cont)
    (if (zero? n)
      (apply-cont cont 1)
      (fact/k (- n 1) (fact1-cont n cont)))))
]

其中，

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (end-cont) |@${val}) = |@${val}

(apply-cont (fact1-cont |@${n} |@${cont}) |@${val}) = |@${val}
= (apply-cont |@${cont} (* |@${n} |@${val}))
}|
}

}

在这一版中，@tt{fact/k}和@tt{apply-cont}的所有调用都在末尾，因此不产生控制语境。

我们可以用数据结构实现这些续文：

@racketblock[
(define-datatype continuation continuation?
  (end-cont)
  (fact1-cont
   (n integer?)
   (cont continuation?)))


(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont () val)
      (fact1-cont (saved-n saved-cont)
        (apply-cont saved-cont (* saved-n val))))))
]

这个程序还能做多种变换，比如图6.1中的寄存。

如图6.2，我们还可以将其转为跳跃式。如果我们用普通的指令式语言，我们可以把跳床改
为适当的循环。

但是，我们本章的焦点是用图5.2中的过程表示法会怎样。回忆一下，在过程表示法中，续
文由它在@tt{apply-cont}中的工作表示。过程表示法如下：

@racketblock[
(define end-cont
  (lambda ()
    (lambda (val) val)))

(define fact1-cont
  (lambda (n saved-cont)
    (lambda (val)
      (apply-cont saved-cont (* n val)))))

(define apply-cont
  (lambda (cont val)
    (cont val)))
]

@nested[#:style eopl-figure]{
@racketblock[
(define n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)

(define fact
  (lambda (arg-n)
    (set! cont (end-cont))
    (set! n arg-n)
    (fact/k)))

(define fact/k
  (lambda ()
    (if (zero? n)
      (begin
        (set! val 1)
        (apply-cont))
      (begin
        (set! cont (fact1-cont n cont))
        (set! n (- n 1))
        (fact/k)))))

(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont () val)
      (fact1-cont (saved-n saved-cont)
        (set! cont saved-cont)
        (set! val (* saved-n val))
        (apply-cont)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "寄存后的" (tt "fact/k")))]

}

我们还可以更进一步，记录程序中续文构造器的每个调用，将其替换为构造器的定义。这种
转换叫做@emph{内联} (@emph{inlining})，因为定义在行内展开。我们再内联
@tt{apply-cont}的调用，不是写@tt{(apply-cont cont val)}，而是直接写@tt{(cont
val)}。

@nested[#:style eopl-figure]{
@racketblock[
(define n 'uninitialized)
(define cont 'uninitialized)
(define val 'uninitialized)
(define pc 'uninitialized)

(define fact
  (lambda (arg-n)
    (set! cont (end-cont))
    (set! n arg-n)
    (set! pc fact/k)
    (trampoline!)
    val))

(define trampoline!
  (lambda ()
    (if (procedure? pc)
      (begin
        (pc)
        (trampoline!))
      )))

(define fact/k
  (lambda ()
    (if (zero? n)
      (begin
        (set! val 1)
        (set! pc apply-cont))
      (begin
        (set! cont (fact1-cont n cont))
        (set! n (- n 1))
        (set! pc fact/k)))))

(define apply-cont
  (lambda ()
    (cases continuation cont
      (end-cont
        ()
        (set! pc #f))
      (fact1-cont
        (n saved-cont)
        (set! cont saved-cont)
        (set! val (* n val))
        (set! pc apply-cont)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "寄存后的跳跃式" (tt "fact/k")))]

}

如果我们按这种方式内联所有用到续文的地方，我们有：

@racketblock[
(define fact
  (lambda (n)
    (fact/k n (lambda (val) val))))

(define fact/k
  (lambda (n cont)
    (if (zero? n)
      (cont 1)
      (fact/k (- n 1) (lambda (val) (cont (* n val)))))))
]

@tt{fact/k}的定义可以读作：

@nested[#:style 'inset]{
@emph{

若@${n}为0，将1传给续文。否则，求@${n-1}的@tt{fact}，求值续文取其结果@tt{val}，
然后将@tt{(* @${n} val)}的值传给当前续文。

}
}

过程@tt{fact/k}具有性质@tt{(fact/k @${n} @${g}) = (@${g} @${n!})}。对@${n}使用归纳法很容易证明这条性质。第一步，当@${n = 0}，我们计算：

@nested{

@nested[#:style 'code-inset]{
@tt{(fact/k 0 @${g}) = (@${g} 1) = (@${g} (fact 0))}
}

归纳步骤中，对某个@${n}，设@tt{(fact/k @${n} @${g}) = (@${g} @${n!})}，试证明
@tt{(fact/k @${(n + 1)} @${g}) = (@${g} @${(n + 1)!})}。要证明它，我们计算：

@nested[#:style 'code-inset]{
@verbatim|{
(fact/k |@${n + 1} |@${g})
= (fact/k |@${n} (lambda (val) (|@${g} (* |@${n + 1} val))))
= ((lambda (val) (|@${g} (* |@${n + 1} val)))   |@elem{（根据归纳假设）}
   (fact |@${n}))
= (|@${g} (* |@${n + 1} (fact |@${n})))
= (|@${g} (fact |@${n + 1}))
}|
}

归纳完毕。

}

这里，像@secref{apca}，@${g}作为上下文参数，性质@tt{(fact/k @${n} @${g}) =
(@${g} @${n!})}作为独立规范，遵循我们的原则@bold{避免神秘小工具}。

现在，我们用同样的方式转换计算斐波那契数列的@tt{fib}。我们从下面的过程开始：

@racketblock[
(define fib
  (lambda (n)
    (if (< n 2)
      1
      (+
        (fib (- n 1))
        (fib (- n 2))))))
]

这里我们两次递归调用@tt{fib}，所以我们需要一个@tt{end-cont}和两个续文构造器，每
个对应一个参数，就像处理@secref{s5.1}中的差值表达式那样。

@racketblock[
(define fib
  (lambda (n)
    (fib/k n (end-cont))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
      (apply-cont cont 1)
      (fib/k (- n 1) (fib1-cont n cont)))))
]

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (end-cont) |@${val}) = |@${val}

(apply-cont (fib1-cont |@${n} |@${cont}) |@${val1})
= (fib/k (- |@${n} 2) (fib2-cont |@${val1} |@${cont}))

(apply-cont (fib2-cont |@${val1} |@${cont}) |@${val2})
= (apply-cont |@${cont} (+ |@${val1} |@${val2}))
}|
}

在过程表示法中，我们有：

@racketblock[
(define end-cont
  (lambda ()
    (lambda (val) val)))

(define fib1-cont
  (lambda (n cont)
    (lambda (val1)
      (fib/k (- n 2) (fib2-cont val1 cont)))))

(define fib2-cont
  (lambda (val1 cont)
    (lambda (val2)
      (apply-cont cont (+ val1 val2)))))

(define apply-cont
  (lambda (cont val)
    (cont val)))
]

如果我们内联所有使用这些过程的地方，我们有：

@racketblock[
(define fib
  (lambda (n)
    (fib/k n (lambda (val) val))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
      (cont 1)
      (fib/k (- n 1)
        (lambda (val1)
          (fib/k (- n 2)
            (lambda (val2)
              (cont (+ val1 val2)))))))))
]

类似阶乘，我们可以把这个定义读作：

@nested[#:style 'inset]{
@emph{

若@${n < 2}，将1传给续文。否则，处理@${n-1}，求值续文取其结果@tt{val1}，然后处理
@${val2}，求值续文取其结果@tt{val2}，然后将@tt{(+ val1 val2)}的值传给当前续文。

}
}

用推导@tt{fact}的方式，很容易得出，对任意@${g}，@tt{(fib/k @${n} @${g}) = (@${g}
(fib @${n}))}。这里有个推广这一想法的手动转换例子：

@nested{
@racketblock[
(lambda (x)
  (cond
   ((zero? x) 17)
   ((= x 1) (f x))
   ((= x 2) (+ 22 (f x)))
   ((= x 3) (g 22 (f x)))
   ((= x 4) (+ (f x) 33 (g y)))
   (else (h (f x) (- 44 y) (g y)))))
]

变成：

@racketblock[
(lambda (x cont)
  (cond
   ((zero? x) (cont 17))
   ((= x 1) (f x cont))
   ((= x 2) (f x (lambda (v1) (cont (+ 22 v1)))))
   ((= x 3) (f x (lambda (v1) (g 22 v1 cont))))
   ((= x 4) (f x (lambda (v1)
                   (g y (lambda (v2)
                          (cont (+ v1 33 v2))))))
   (else (f x (lambda (v1)
                (g y (lambda (v2)
                       (h v1 (- 44 y) v2 cont)))))))))
]

其中，过程@tt{f}，@tt{g}和@tt{h}都以类似方式转换。

@itemlist[

 @item{在@tt{(zero? x)}这一行，我们将17返回给续文。}

 @item{在@tt{(= x 1)}这一行，我们以@elem[#:style question]{尾递归}方式调用@tt{f}。}

 @item{在@tt{(= x 2)}这一行，我们在加法的操作数位置调用@tt{f}。}

 @item{在@tt{(= x 3)}这一行，我们在过程调用的操作数位置调用@tt{f}。}

 @item{在@tt{(= x 4)}这一行，加法的操作数位置，我们有两个过程调用。}

 @item{在@tt{else}这一行，在另一个过程调用中，我们在两个操作数位置有两个过程调用。}

]

}

这些例子中，浮现出一种模式。

@nested[#:style tip]{
 @centered{@bold{CPS秘方}}

 @nested[#:style tip-content]{
 要将程序转换为续文传递风格：

 @itemlist[#:style 'ordered

 @item{给每个过程传一个额外参数（通常是@tt{cont}或@tt{k}）。}

 @item{不论过程返回常量还是变量，都将返回值值传给续文，就像上面的@tt{(cont 17)}。}

 @item{过程调用在尾部时，用同样的续文@tt{cont}调用过程。}

 @item{过程调用在操作数位置时，应在新的续文中求值，该续文给调用结果命名，继续计
 算。}

]}}

这些规则虽不正式，但解释了这种模式。

@exercise[#:level 1 #:tag "ex6.1"]{

考虑图6.2，为什么移除@tt{fact/k}定义中的@tt{(set! pc fact/k)}和@tt{apply-cont}中
定义的@tt{(set! pc apply-cont)}，程序仍能工作？

}

@exercise[#:level 1 #:tag "ex6.2"]{

对@${n}使用归纳法，证明对任意@${g}，@tt{(fib/k @${n} @${g}) = (@${g} (fib
@${n}))}。

}

@exercise[#:level 1 #:tag "ex6.3"]{

把下面每个Scheme表达式重写为续文传递风格。假设所有未知函数都已经重写成CPS风格。

@itemlist[#:style 'ordered

 @item{@tt{(lambda (x y) (p (+ 8 x) (q y)))}}

 @item{@tt{(lambda (x y u v) (+ 1 (f (g x y) (+ u v))))}}

 @item{@tt{(+ 1 (f (g x y) (+ u (h v))))}}

 @item{@tt{(zero? (if a (p x) (p y)))}}

 @item{@tt{(zero? (if (f a) (p x) (p y)))}}

 @item{@tt{(let ((x (let ((y 8)) (p y)))) x)}}

 @item{@tt{(let ((x (if a (p x) (p y)))) x)}}

]

}

@exercise[#:level 2 #:tag "ex6.4"]{

把下面的每个过程重写为续文传递风格。表示每个过程的续文时，先用数据结构表示法，然
后用过程表示法，然后用内联过程表示法。最后，写出寄存版本。照@secref{cpi}那样定义
@tt{end-cont}，验证你实现的这四个版本是尾调用。

@nested[#:style 'code-inset]{
@verbatim|{
(apply-cont (end-cont) |@${val})
= (begin
    (eopl:printf "计算结束.~%")
    (eopl:printf "这句话只能出现一次.~%")
    |@${val})
}|
}

@itemlist[#:style 'ordered

 @item{@tt{remove-first}（@secref{r-f}）}

 @item{@tt{list-sum}（@secref{apca}）}

 @item{@tt{occurs-free?}（@secref{o-f}）}

 @item{@tt{subst}（@secref{subst}）}

]

}

@exercise[#:level 1 #:tag "ex6.5"]{

当我们把表达式重写为CPS时，我们就为表达式中的过程选择了一种求值顺序。把前面的每
个例子重写为CPS，且使过程调用从右向左求值。

}

@exercise[#:level 1 #:tag "ex6.6"]{

在@tt{(lambda (x y) (+ (f (g x)) (h (j y))))}中，过程调用有多少种不同的求值顺序？
对每种求值顺序，写出对应的CPS表达式。

}

@exercise[#:level 2 #:tag "ex6.7"]{

写出图5.4，5.5和5.6中解释器的过程表示和内联过程表示。

}

@exercise[#:level 3 #:tag "ex6.8"]{

写出@secref{s5.4}解释器的过程表示和内联过程表示。这很有挑战性，因为我们实际上有
两个观测器，@tt{apply-cont}和@tt{apply-handler}。提示，考虑修改 @elem[#:style
question]{200页}的秘方，给每个过程添加两个参数，一个表示@tt{apply-cont}中 续文的
行为，一个表示@tt{apply-handler}中续文的行为。

}

有时，我们能发现更聪明的方式来表示续文。我们重新考虑用过程表示续文的@tt{fact}。
那里，我们有两个续文构造器：

@racketblock[
(define end-cont
  (lambda ()
    (lambda (val) val)))

(define fact1-cont
  (lambda (n cont)
    (lambda (val) (cont (* n val)))))

(define apply-cont
  (lambda (cont val)
    (cont val)))
]

在这个系统中，所有续文都用某个数乘以参数。@tt{end-cont}将其参数乘1，若@${cont}将
参数乘@${k}，那么@tt{(fact1 @${n} @${cont})}将其值乘@${k * n}。

所以每个续文都形如@tt{(lambda (val) (* @${k} val))}。这表示我们可以用每个续文仅
有的自由变量——数字@${k}——表示它。用这种表示方式，我们有：

@racketblock[
(define end-cont
  (lambda ()
    1))

(define fact1-cont
  (lambda (n cont)
    (* cont n)))

(define apply-cont
  (lambda (cont val)
    (* cont val)))
]

如果我们在原来@tt{fact/k}的定义中内联这些过程，并使用性质@tt{(* @${cont} 1) =
@${cont}}，可得：

@racketblock[
(define fact
  (lambda (n)
    (fact/k n 1)))

(define fact/k
  (lambda (n cont)
    (if (zero? n)
      cont
      (fact/k (- n 1) (* cont n)))))
]

但是这和@tt{fact-iter}（@${139页}）完全相同！所以我们明白了，累加器通常只是续文
的一种表示方式。这令人拍案叫绝。相当一部分经典的程序优化问题原来是这一思想的特例。

@exercise[#:level 1 #:tag "ex6.9"]{

乘法的什么性质使这种程序优化成为可能？

}

@exercise[#:level 1 #:tag "ex6.10"]{

给@tt{list-sum}设计一种简便的续文表示方式，就像上面的@tt{fact/k}那样。

}

@section[#:tag "s6.2"]{曳尾式}

@section[#:tag "s6.3"]{转换为续文传递风格}

@section[#:tag "s6.4"]{建模计算效果}
