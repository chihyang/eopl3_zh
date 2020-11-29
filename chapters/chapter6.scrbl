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

@title[#:style part-title-style-numbered #:tag "cps"]{续文传递风格}

在@secref{cpi}，我们把解释器中的所有主要过程调用重写成@emph{尾调用}。这样，我们
保证任何时候，不论执行的程序多大或多复杂，解释器只使用有限的控制上下文。这种性质
叫做@emph{迭代性控制行为}。

@eopl-index["Continuation-passing style"]
我们通过给每个过程多传一个@emph{续文}参数实现这一目标。这种编程风格
叫做@term["continuation-passing style"]{续文传递风格} 或 @term[#f]{CPS}，且不限
于解释器。

本章，我们介绍一种系统性的方法，将任一过程转换为具有迭代性控制行为的等效过程。实
现这一点需要将过程转换为续文传递风格。

@section[#:style section-title-style-numbered #:tag "s6.1"]{写出续文传递风格的程序}

@eopl-index[#:range-mark 'start "Continuation-passing style" "examples of"]
除了写解释器，CPS 还有别的作用。我们考虑“老熟人”阶乘程序：

@eopl-code{
@racketblock[
(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))
]
}

阶乘的传递续文版本是：

@nested{
@eopl-code{
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
}

其中，

@eopl-code{
@verbatim|{
(apply-cont (end-cont) |@${val}) = |@${val}

(apply-cont (fact1-cont |@${n} |@${cont}) |@${val}) = |@${val}
= (apply-cont |@${cont} (* |@${n} |@${val}))
}|
}

}

在这一版内，@tt{fact/k} 和 @tt{apply-cont} 中的所有调用都在末尾，因此不产生控制
上下文。

@eopl-index["Continuations" "data structure representation of"]
@eopl-index["Data structure representation" @eopl-index-entry["of continuations" "continuations"]]
我们可以用数据结构实现这些续文：

@eopl-code{
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
}

我们还能以多种方式转换这一程序，比如寄存它，如@figure-ref{fig-6.1} 所示。

我们甚至能将其转为跳跃式，如@figure-ref{fig-6.2} 所示。如果用普通的指令式语言，
我们自然能将跳床替换为适当的循环。

但是，本章我们主要关心，用过程表示法（如@figure-ref{fig-5.2}）时会发生什么。回忆
一下，在过程表示法中，续文用它在 @tt{apply-cont} 中的动作表示。过程表示为：
@eopl-index["Continuations" "procedural representation of"]

@eopl-code{
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
}

@eopl-figure[#:position "!ht"]{
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

@eopl-caption["fig-6.1"]{寄存后的 @tt{fact/k}}

}

我们还可以更进一步，将程序中所有调用续文构造器的地方替换为其定义。因为定义在行内
展开，这一转换叫做@term["inlining"]{内联}。我们还要内联 @tt{apply-cont} 的调用，
不再写 @tt{(apply-cont cont val)}，而是直接写 @tt{(cont val)}。

@eopl-figure{
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

@eopl-caption["fig-6.2"]{寄存后的跳跃式 @tt{fact/k}}

}

如果我们按这种方式内联所有用到续文的地方，我们得到：

@eopl-code{
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
}

@tt{fact/k} 的定义可以读作：

@nested[#:style 'inset]{
@emph{

若 @${n} 为 0，将 1 传给续文；否则，求 @${n-1} 的 @tt{fact}，求值所在的续文取其
结果 @tt{val}，然后将 @tt{(* @${n} val)} 的值传给当前续文。

}
}

过程 @tt{fact/k} 具有性质 @tt{(fact/k @${n} @${g}) = (@${g} @${n!})}。对 @${n}
使用归纳法很容易证明这条性质。第一步，当 @${n = 0}，我们计算：

@nested{

@nested[#:style small]{
@nested[#:style 'code-inset]{
@tt{(fact/k 0 @${g}) = (@${g} 1) = (@${g} (fact 0))}
}
}

归纳步骤中，对某个 @${n}，设 @tt{(fact/k @${n} @${g}) = (@${g} @${n!})}，试证明
@tt{(fact/k @${(n + 1)} @${g}) = (@${g} @${(n + 1)!})}。要证明它，我们计算：

@eopl-code{
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

像@secref{s1.3}一样，这里的 @${g} 是上下文参数；性质 @tt{(fact/k @${n} @${g}) =
(@${g} @${n!})} 作为独立规范，遵循我们的原则@elemref["no-myth"]{@bold{避免神秘小工具}}。

现在，我们用同样的方式转换计算斐波那契数列的 @tt{fib}。我们从下面的过程开始：

@eopl-code{
@racketblock[
(define fib
  (lambda (n)
    (if (< n 2)
      1
      (+
        (fib (- n 1))
        (fib (- n 2))))))
]}

这里我们两次递归调用 @tt{fib}，所以我们需要一个 @tt{end-cont} 和两个续文构造器，
二者各对应一个参数，就像处理@secref{s5.1}中的差值表达式那样。

@eopl-code{
@racketblock[
(define fib
  (lambda (n)
    (fib/k n (end-cont))))

(define fib/k
  (lambda (n cont)
    (if (< n 2)
      (apply-cont cont 1)
      (fib/k (- n 1) (fib1-cont n cont)))))
]}

@eopl-code{
@verbatim|{
(apply-cont (end-cont) |@${val}) = |@${val}

(apply-cont (fib1-cont |@${n} |@${cont}) |@${val1})
= (fib/k (- |@${n} 2) (fib2-cont |@${val1} |@${cont}))

(apply-cont (fib2-cont |@${val1} |@${cont}) |@${val2})
= (apply-cont |@${cont} (+ |@${val1} |@${val2}))
}|
}

在过程表示法中，我们有：
@eopl-index["Continuations" "procedural representation of"]

@eopl-code{
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
]}

如果我们内联所有使用这些过程的地方，可得：

@eopl-code{
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
]}

类似阶乘，我们可以把这个定义读作：

@nested[#:style 'inset]{
@emph{

若 @${n < 2}，将 1 传给续文。否则，处理 @${n-1}，求值所在的续文取其结果
@tt{val1}，然后处理 @tt{val2}，求值所在的续文取其结果 @tt{val2}，然后将 @tt{(+
val1 val2)} 的值传给当前续文。

}
}

用推导 @tt{fact} 的方式，很容易得出，对任意 @${g}，@tt{(fib/k @${n} @${g}) =
(@${g} (fib @${n}))}。这里有个假想的例子，推广了这一想法：

@nested{
@eopl-code{
@racketblock[
(lambda (x)
  (cond
   ((zero? x) 17)
   ((= x 1) (f x))
   ((= x 2) (+ 22 (f x)))
   ((= x 3) (g 22 (f x)))
   ((= x 4) (+ (f x) 33 (g y)))
   (else (h (f x) (- 44 y) (g y)))))
]}

变成：

@eopl-code{
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
]}

其中，过程 @tt{f}、@tt{g} 和 @tt{h} 都以类似方式转换。

@itemlist[

 @item{在 @tt{(zero? x)} 这一行，我们将 17 返回给续文。}

 @item{在 @tt{(= x 1)} 这一行，我们按尾递归的方式调用 @tt{f}。}

 @item{在 @tt{(= x 2)} 这一行，我们在加法的操作数位置调用 @tt{f}。}

 @item{在 @tt{(= x 3)} 这一行，我们在过程调用的操作数位置调用 @tt{f}。}

 @item{在 @tt{(= x 4)} 这一行，有两个过程调用在加法的操作数位置。}

 @item{在 @tt{else} 这一行，有两个过程调用在另一个过程调用内的操作数位置。}

]

}

从这些例子中浮现出一种模式。

@nested[#:style tip]{
 @centered{@elemtag["cps-recipe"]{@bold{CPS 秘方}}
           @eopl-index["Continuation-passing style" "transformation to"]
           @eopl-index["CPS Recipe"]}

 @nested[#:style tip-content]{
 要将程序转换为续文传递风格：

 @itemlist[#:style 'ordered

 @item{给每个过程传一个额外参数（通常是 @tt{cont} 或 @tt{k}）。}

 @item{不论过程返回常量还是变量，都将返回值传给续文，就像上面的 @tt{(cont 17)}。}

 @item{过程调用在尾部时，用同样的续文 @tt{cont} 调用过程。}

 @item{过程调用在操作数位置时，在新的续文中求过程调用的值，这个续文给调用结果命
 名，继续进行计算。}

]}}

这些规则虽不正式，但足以解释这种模式。
@eopl-index[#:range-mark 'end "Continuation-passing style" "examples of"]

@exercise[#:level 1 #:tag "ex6.1"]{

考虑@figure-ref{fig-6.2}，为什么移除 @tt{fact/k} 定义中的 @tt{(set! pc fact/k)}
和 @tt{apply-cont} 中定义的 @tt{(set! pc apply-cont)}，程序仍能正常运行？

}

@exercise[#:level 1 #:tag "ex6.2"]{

用归纳法证明：对任意 @${g}，@tt{(fib/k @${n} @${g}) = (@${g} (fib @${n}))}，归纳
变量为 @${n}。

}

@exercise[#:level 1 #:tag "ex6.3"]{

把下面每个 Scheme 表达式重写为续文传递风格。假设所有未知函数都已经重写成 CPS 风
格。

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

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex6.4"] "Continuations" "data structure representation of"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex6.4"] "Continuations" "procedural representation of"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex6.4"] "Data structure representation" @eopl-index-entry["of continuations" "continuations"]]
把下面的所有过程重写为续文传递风格。表示每个过程的续文时，先用数据结构表示法，然
后用过程表示法，然后用内联过程表示法。最后，写出寄存版本。照@secref{cpi}那样定义
@tt{end-cont}，验证你实现的这四个版本是尾调用：

@eopl-equation{
@verbatim|{
(apply-cont (end-cont) |@${val})
= (begin
    (eopl:printf "计算结束.~%")
    (eopl:printf "这句话只能出现一次.~%")
    |@${val})
}|
}

@itemlist[#:style 'ordered

 @item{@tt{remove-first}（@elemref["remove-first"]{1.2.3 节}）}

 @item{@tt{list-sum}（@elemref["list-sum"]{1.3 节}）}

 @item{@tt{occurs-free?}（@elemref["occurs-free-1?"]{1.2.4 节}）}

 @item{@tt{subst}（@elemref["subst"]{1.2.5 节}）
 @eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex6.4"] "Continuations" "data structure representation of"]
 @eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex6.4"] "Continuations" "procedural representation of"]
 @eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex6.4"]
             "Data structure representation" @eopl-index-entry["of continuations" "continuations"]]}

]

}

@exercise[#:level 1 #:tag "ex6.5"]{

当我们把表达式重写为CPS时，我们就为表达式中的过程选择了一种求值顺序。把前面的每
个例子重写为CPS，且使过程调用从右向左求值。

}

@exercise[#:level 1 #:tag "ex6.6"]{

在 @tt{(lambda (x y) (+ (f (g x)) (h (j y))))} 中，过程调用有多少种不同的求值顺
序？对每种求值顺序，写出对应的 CPS 表达式。

}

@exercise[#:level 2 #:tag "ex6.7"]{

写出@figure-ref{fig-5.4}、@countref{fig-5.5} 和 @countref{fig-5.6} 中解释器的过
程表示和内联过程表示。

}

@exercise[#:level 3 #:tag "ex6.8"]{

写出@secref{s5.4}解释器的过程表示和内联过程表示。这极富挑战性，因为我们实际上有
两个观测器，@tt{apply-cont} 和 @tt{apply-handler}。提示：考虑修改
@pageref{cps-recipe}的秘方，给每个过程添加两个参数，一个表示 @tt{apply-cont} 中
续文的行为，一个表示 @tt{apply-handler} 中续文的行为。@linebreak[]

}

有时，我们能发现更巧妙的方式表示续文。我们重新考虑用过程表示续文的 @tt{fact}。其
中，我们有两个续文构造器，写作：

@eopl-code{
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
]}

在这个系统中，所有续文都用某个数乘以参数。@tt{end-cont} 将其参数乘 1，若
@${cont} 将参数乘 @${k}，那么 @tt{(fact1 @${n} @${cont})} 将其值乘 @${k * n}。

所以每个续文都形如 @tt{(lambda (val) (* @${k} val))}。这意味着我们可以用每个续文
仅有的自由变量——数字 @${k}——表示它。用这种表示方式，我们有：

@eopl-code{
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
]}

如果我们在 @tt{fact/k} 的原始定义中内联这些过程，并使用性质 @tt{(* @${cont} 1) =
@${cont}}，可得：

@eopl-code{
@racketblock[
(define fact
  (lambda (n)
    (fact/k n 1)))

(define fact/k
  (lambda (n cont)
    (if (zero? n)
      cont
      (fact/k (- n 1) (* cont n)))))
]}

但是这和 @tt{fact-iter}（@pageref{fact-iter}）完全相同！所以我们明白了，
@eopl-index{Accumulator}累加器通常只是续文的一种表示方式。这令人印象深刻。相当一
部分经典的程序优化问题原来是这一思想的特例。

@exercise[#:level 1 #:tag "ex6.9"]{

乘法的什么性质使这种程序优化成为可能？

}

@exercise[#:level 1 #:tag "ex6.10"]{

给 @tt{list-sum} 设计一种简便的续文表示方式，就像上面的 @tt{fact/k} 那样。

}

@section[#:style section-title-style-numbered #:tag "s6.2"]{尾式}

要写出程序来做续文传递风格变换，我们需要找出输入和输出语言。我们选择 LETREC 作为
输入语言，并补充多参数过程和多声明的 @tt{letrec} 表达式。其语法如@figure-ref{fig-6.3}
所示，我们称之为 CPS-IN。为了区分这种语言和输出语言的表达式，我们把这些
叫做@term["input expression"]{输入表达式}。

@eopl-index["CPS-IN"]
要定义 CPS 变换算法的可能输出，我们要找出 CPI-IN 的子集，在这个集合中，过程调用
不产生任何控制上下文。
@eopl-index["Control context"]

回忆@secref{cpi}中的原则：

@nested{
@nested[#:style tip]{
 @centered{@bold{不是过程调用，而是操作数的求值导致控制上下文扩大。}
           @eopl-index["Control context"]}
}

那么，在

@eopl-code{
@racketblock[
(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))
]}

中，是 @tt{fact} 的调用位置@emph{作为操作数}导致了控制上下文的产生。相反，在

@eopl-code{
@racketblock[
(define fact-iter
  (lambda (n)
    (fact-iter-acc n 1)))

(define fact-iter-acc
  (lambda (n a)
    (if (zero? n) a (fact-iter-acc (- n 1) (* n a)))))
]}

中， 过程调用都不在操作数位置。我们说这些调用在@term["tail position"]{尾端}，因
为它们的值就是整个调用的结果。我们称之为@term["tail call"]{尾调用}。

}

@eopl-figure[#:position "!ht"]{

@linebreak[]
@envalign*{\mathit{Program} &::= \mathit{InpExp} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{a-program (exp1)}} \\[5pt]
            \mathit{InpExp} &::= \mathit{Number} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{const-exp (num)}} \\[5pt]
            \mathit{InpExp} &::= @tt{(- @m{\mathit{InpExp}} , @m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{diff-exp (exp1 exp2)}} \\[5pt]
            \mathit{InpExp} &::= @tt{(zero? @m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{zero?-exp (exp1)}} \\[5pt]
            \mathit{InpExp} &::= @tt{if @m{\mathit{InpExp}} then @m{\mathit{InpExp}} else @m{\mathit{InpExp}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{if-exp (exp1 exp2 exp3)}} \\[5pt]
            \mathit{InpExp} &::= \mathit{Identifier} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{var-exp (var)}} \\[5pt]
            \mathit{InpExp} &::= @tt{let @m{\mathit{Identifier}} = @m{\mathit{InpExp}} in @m{\mathit{InpExp}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{let-exp (var exp1 body)}} \\[5pt]
            \mathit{InpExp} &::= @tt{letrec @m{\{\mathit{Identifier}\ }@tt["("]@m{\{\mathit{Identifier}\}^{*(,)}}@tt[")"] = @m{\mathit{InpExp}\}^{*}} in @m{\mathit{InpExp}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{letrec-exp (p-names b-varss p-bodies letrec-body)}} \\[5pt]
            \mathit{InpExp} &::= @tt{proc (@m{\mathit{\{Identifier\}^{*(,)}}}) @m{\mathit{InpExp}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{proc-exp (vars body)}} \\[5pt]
            \mathit{InpExp} &::= @tt{(@m{\mathit{InpExp}} @m{\{\mathit{InpExp}\}^{*}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{call-exp (rator rands)}}}

@eopl-caption["fig-6.3"]{CPS-IN的语法
                         @eopl-index["CPS-IN"]}
}

再回忆一下原则@bold{尾调用不扩大续文}：

@nested[#:style tip]{
 @centered{@bold{尾调用不扩大续文}}

 @para[#:style tip-content]{若 @${exp_1} 的值作为 @${exp_2} 的值返回，则
 @${exp_1} 和@${exp_2} 应在同样的续文中执行。} }

若所有过程调用和所有包含过程调用的子表达式都在尾端，我们称一个表达式
为@term["tail form"]{尾式}。这个条件表明所有过程调用都不会产生控制上下文。

因此，在 Scheme 中，

@nested{
@eopl-code{
@racketblock[
(if (zero? x) (f y) (g z))
]}

是尾式，

@eopl-code{
@racketblock[
(if b
  (if (zero? x) (f y) (g z))
  (h u))
]}

也是尾式，但

@eopl-code{
@racketblock[
(+
  (if (zero? x) (f y) (g z))
  37)
]}

不是。因为 @tt{if} 表达式包含一个不在尾端的过程调用。

}

通常，要判定语言的尾端，我们必须理解其含义。处于尾端的子表达式具有如下性质：该表
达式求值后，其值随即成为整个表达式的值。一个表达式可能有多个尾端。例如，@tt{if}
表达式可能选择真值分支，也可能选择假值分支。对尾端的子表达式，不需要保存信息，因
此也就不会产生控制上下文。

CPS-IN 中的尾端如@figure-ref{fig-6.4} 所示。尾端每个子表达式的值都可能成为整个表
达式的值。在传递续文的解释器中，操作数位置的子表达式会产生新的续文。尾端的子表达
式在原表达式的续文中求值，如@pageref{tail-call-explain}所述。

@eopl-figure[#:position "!ht"]{
@eopl-equation{
@verbatim|{
zero?(|@${O})
-(|@${O},|@${O})
if |@${O} then |@${T} else |@${T}
let |@${Var} = |@${O} in |@${T}
letrec |@${\{Var \ @tt["("]\{Var\}^{*(,)}@tt[")"] \ @tt{=} \ T\}^{*}} in |@${T}
proc (|@${\{Var\}^{*(,)}}) = |@${T}
(|@${O} |@${O} |@${...} |@${O})
}|
}

@eopl-caption["fig-6.4"]{CPS-IN 中的尾端和操作数位置。尾端记为 @${T}。操作数位置
记为 @${O}。}
}

@eopl-index["CPS-OUT"]
我们根据这种区别设计 CPS 转换算法的目标语言 CPS-OUT。这种语言的语法
如@figure-ref{fig-6.5} 所示。这套语法定义了 CPS-IN 的子集，但略有不同。生成式的
名字总以 @tt{cps-} 开头，这样它们不会与 CPS-IN 中生成式的名字混淆。

新的语法有两个非终止符，@${\mathit{SimpleExp}} 和 @${\mathit{TfExp}}。这种设计中，
@${\mathit{SimpleExp}} 表达式不包含任何过程调用，@${\mathit{TfExp}} 表达式一定是
尾式。

因为 @${\mathit{SimpleExp}} 表达式不包含任何过程调用，它们大致可以看成只有一行的
简单代码，对我们来说，它们简单到不需使用控制堆栈。简单表达式包括 @tt{proc} 表达
式，因为 @tt{proc} 表达式立即返回一个过程值，但过程的主体必须是尾式。

尾表达式的传递续文解释器如@figure-ref{fig-6.6} 所示。由于这种语言的过程取多个参
数，我们用@exercise-ref{ex2.10} 中的 @tt{extend-env*} 创建多个绑定，并用类似方式
扩展 @tt{extend-env-rec}，得到 @tt{extend-env-rec*}。

在这个解释器中，所有递归调用都在（Scheme 的）尾端，所以运行解释器不会在 Scheme
中产生控制上下文（不全是这样：过程
@tt{value-of-simple-exp}（@exercise-ref{ex6.11}）会在 Scheme 中产生控制上下文，
但这可以避免（参见@exercise-ref{ex6.18}））。

更重要的是，解释器不会产生新的续文。过程 @tt{value-of/k} 取一个续文参数，原封不
动地传给每个递归调用。所以，我们可以很容易地移除续文参数。

当然，没有通用的方式判断一个过程的控制行为是否是迭代式的。考虑

@nested{
@eopl-code{
@racketblock[
(lambda (n)
  (if (strange-predicate? n)
    (fact n)
    (fact-iter n)))
]}

只有 @tt{strange-predicate?} 对所有足够大的 @tt{n} 都返回假时，这个过程才是迭代
式的。但即使能查看 @tt{strange-predicate?} 的代码，也可能无法判断这一条件的真假。
因此，我们最多只能寄希望于程序中的过程调用不产生控制上下文，而不论其是否执行。

}

@eopl-figure{

@linebreak[]
@nested[#:style small]{
@envalign*{\mathit{Program} &::= \mathit{TfExp} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-a-program (exp1)}} \\[5pt]
         \mathit{SimpleExp} &::= \mathit{Number} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-const-exp (num)}} \\[5pt]
         \mathit{SimpleExp} &::= \mathit{Identifier} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-var-exp (var)}} \\[5pt]
         \mathit{SimpleExp} &::= @tt{(- @m{\mathit{SimpleExp}} , @m{\mathit{SimpleExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-diff-exp (simple1 simple2)}} \\[5pt]
         \mathit{SimpleExp} &::= @tt{(zero? @m{\mathit{SimpleExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-zero?-exp (simple1)}} \\[5pt]
         \mathit{SimpleExp} &::= @tt{proc (@m{\mathit{\{Identifier\}^{*(,)}}}) @m{\mathit{TfExp}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-proc-exp (vars body)}} \\[5pt]
             \mathit{TfExp} &::= @m{\mathit{SimpleExp}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{simple-exp->exp (simple-exp1)}} \\[5pt]
            \mathit{TfExp} &::= @tt{let @m{\mathit{Identifier}} = @m{\mathit{SimpleExp}} in @m{\mathit{TfExp}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-let-exp (var simple1 body)}} \\[5pt]
             \mathit{TfExp} &::= @tt{letrec @m{\{\mathit{Identifier}\ }@tt["("]@m{\{\mathit{Identifier}\}^{*(,)}}@tt[")"] = @m{\mathit{TfExp}\}^{*}} in @m{\mathit{TfExp}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-letrec-exp (p-names b-varss p-bodies body)}} \\[5pt]
             \mathit{TfExp} &::= @tt{if @m{\mathit{SimpleExp}} then @m{\mathit{TfExp}} else @m{\mathit{TfExp}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-if-exp (simple1 body1 body2)}} \\[5pt]
             \mathit{TfExp} &::= @tt{(@m{\mathit{SimpleExp}} @m{\{\mathit{SimpleExp}\}^{*}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{call-exp (rator rands)}}}}

@eopl-caption["fig-6.5"]{CPS-OUT 的语法
                         @eopl-index["CPS-OUT"]}
}

@eopl-figure{

@racketblock[
@#,elem{@bold{@tt{value-of/k}} : @${\mathit{TfExp} \times \mathit{Env} \times \mathit{Cont} \to \mathit{FinalAnswer}}}
(define value-of/k
  (lambda (exp env cont)
    (cases tfexp exp
      (simple-exp->exp (simple)
        (apply-cont cont
          (value-of-simple-exp simple env)))
      (cps-let-exp (var rhs body)
        (let ((val (value-of-simple-exp rhs env)))
          (value-of/k body
            (extend-env (list var) (list val) env)
            cont)))
      (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of/k letrec-body
          (extend-env-rec** p-names b-varss p-bodies env)
          cont))
      (cps-if-exp (simple1 body1 body2)
        (if (expval->bool (value-of-simple-exp simple1 env))
          (value-of/k body1 env cont)
          (value-of/k body2 env cont)))
      (cps-call-exp (rator rands)
        (let ((rator-proc
                (expval->proc
                  (value-of-simple-exp rator env)))
               (rand-vals
                 (map
                   (lambda (simple)
                     (value-of-simple-exp simple env))
                   rands)))
          (apply-procedure/k rator-proc rand-vals cont))))))

@#,elem{@bold{@tt{apply-procedure/k}} : @${\mathit{Proc} \times \mathit{ExpVal} \times \mathit{Cont} \to \mathit{ExpVal}}}
(define apply-procedure/k
  (lambda (proc1 args cont)
    (cases proc proc1
      (procedure (vars body saved-env)
        (value-of/k body
          (extend-env* vars args saved-env)
          cont)))))
]

@eopl-caption["fig-6.6"]{CPS-OUT 中的尾式解释器}
}

@exercise[#:level 1 #:tag "ex6.11"]{

写出 @tt{value-of-simple-exp}，完成@figure-ref{fig-6.6} 中的解释器。

}

@exercise[#:level 1 #:tag "ex6.12"]{

判断下列表达式是否是简单的。

@itemlist[#:style 'ordered

 @item{@tt{-((f -(x,1)),1)}}

 @item{@tt{(f -(-(x,y),1))}}

 @item{@tt{if zero?(x) then -(x,y) else -(-(x,y),1)}}

 @item{@tt{let x = proc (y) (y x) in -(x,3)}}

 @item{@tt{let f = proc (x) x in (f 3)}}

]

}

@exercise[#:level 1 #:tag "ex6.13"]{

用上面@pageref{cps-recipe}的 CPS 秘方，把下列 CPS-IN 表达式翻译为续文传递风格。
用@figure-ref{fig-6.6} 中的解释器运行转换后的程序，测试它们，确保原程序和转换后
的版本对所有输入都给出同样的结果。

@itemlist[#:style 'ordered

 @item{@tt{removeall}。

 @eopl-code{
 @verbatim|{
 letrec
  removeall(n,s) =
   if null?(s)
   then emptylist
   else if number?(car(s))
        then if equal?(n,car(s))
             then (removeall n cdr(s))
             else cons(car(s),
                       (removeall n cdr(s)))
        else cons((removeall n car(s)),
                  (removeall n cdr(s)))
 }|
 }
 }

 @item{@tt{occurs-in?}。

 @eopl-code{
 @verbatim|{
 letrec
  occurs-in?(n,s) =
   if null?(s)
   then 0
   else if number?(car(s))
        then if equal?(n,car(s))
             then 1
             else (occurs-in? n cdr(s))
        else if (occurs-in? n car(s))
             then 1
             else (occurs-in? n cdr(s))
 }|
 }
 }

 @item{@tt{remfirst}。它使用前面例子中的 @tt{occurs-in?}。

 @eopl-code{
 @verbatim|{
 letrec
  remfirst(n,s) =
   letrec
    loop(s) =
     if null?(s)
     then emptylist
     else if number?(car(s))
          then if equal?(n,car(s))
               then cdr(s)
               else cons(car(s),(loop cdr(s)))
          else if (occurs-in? n car(s))
               then cons((remfirst n car(s)),
                         cdr(s))
               else cons(car(s),
                         (remfirst n cdr(s)))
   in (loop s)
 }|
 }
 }

 @item{@tt{depth}。

 @eopl-code{
 @verbatim|{
 letrec
  depth(s) =
   if null?(s)
   then 1
   else if number?(car(s))
        then (depth cdr(s))
        else if less?(add1((depth car(s))),
                      (depth cdr(s)))
             then (depth cdr(s))
             else add1((depth car(s)))
 }|
 }
 }

 @item{@tt{depth-with-let}。

 @eopl-code{
 @verbatim|{
 letrec
  depth(s) =
   if null?(s)
   then 1
   else if number?(car(s))
        then (depth cdr(s))
        else let dfirst = add1((depth car(s)))
                 drest = (depth cdr(s))
             in if less?(dfirst,drest)
                then drest
                else dfirst
 }|
 }
 }

 @item{@tt{map}。

 @eopl-code{
 @verbatim|{
 letrec
  map(f, l) = if null?(l)
              then emptylist
              else cons((f car(l)),
                        (map f cdr(l)))
  square(n) = *(n,n)
 in (map square list(1,2,3,4,5))
 }|
 }
 }

 @item{@tt{fnlrgtn}。n-list 类似 s-list（@pageref{s-list}），只不过其中的元素不
 是符号，而是数字。@tt{fnlrgtn} 取一 n-list，一个数字 @tt{n}，返回列表中（从左向
 右数）第一个大于 @tt{n} 的数字。一旦找到结果，就不再检查列表中剩余元素。例如，

 @eopl-code{
 @verbatim|{
 (fnlrgtn list(1,list(3,list(2),7,list(9)))
  6)
 }|
 }

 返回 7。
 }

 @item{@tt{every}。这个过程取一谓词，一个列表，当且仅当谓词对列表中所有元素都为
 真时，返回真。

 @eopl-code{
 @verbatim|{
 letrec
  every(pred, l) =
   if null?(l)
   then 1
   else if (pred car(l))
        then (every pred cdr(l))
        else 0
 in (every proc(n) greater?(n,5) list(6,7,8,9))
 }|
 }
 }
]

}

@exercise[#:level 1 #:tag "ex6.14"]{

补充 @tt{value-of-program} 和 @tt{apply-cont}，完成@figure-ref{fig-6.6} 中的解释
器。

}

@exercise[#:level 1 #:tag "ex6.15"]{

观察前一道练习中的解释器可知，@tt{cont} 只有一个值。根据这一观察完全移除
@tt{cont} 参数。

}

@exercise[#:level 1 #:tag "ex6.16"]{

寄存@figure-ref{fig-6.6} 中的解释器。

}

@exercise[#:level 1 #:tag "ex6.17"]{

把@figure-ref{fig-6.6} 中的解释器转换为跳跃式。

}

@exercise[#:level 2 #:tag "ex6.18"]{

修改 CPS-OUT 的语法，把简单 @tt{diff-exp} 和 @tt{zero?-exp} 的参数限制为常量和变
量。这样，语言中的 @tt{value-of-simple-exp} 就不必递归。

}

@exercise[#:level 2 #:tag "ex6.19"]{

写出 Scheme 过程 @tt{tail-form?}，它取一 CPS-IN 程序的语法树，语法
如@figure-ref{fig-6.3} 所示，判断同一字符串是否是@figure-ref{fig-6.5} 中语法定义
的尾式。

}

@section[#:style section-title-style-numbered #:tag "s6.3"]{转换为续文传递风格}

@eopl-index[#:range-mark 'start "Continuation-passing style" "transformation to"]
本节，我们开发算法，将任意程序从 CPS-IN 转换为 CPS-OUT。

就像传递续文的解释器一样，我们的翻译器@emph{跟随语法}。也像传递续文的解释器一样，
我们的翻译器再取一个表示续文的参数。多出的这个参数是一个表示续文的简单表达式。

像之前那样，我们首先给出例子，然后提出规范，最后写出程序。@figure-ref{fig-6.7}
展示了与前一节类似的 Scheme 例子，只是更加详细。

@eopl-figure[#:position "!ht"]{

@eopl-code{
@racketblock[
(lambda (x)
  (cond
    ((zero? x) 17)
    ((= x 1) (f (- x 13) 7))
    ((= x 2) (+ 22 (- x 3) x))
    ((= x 3) (+ 22 (f x) 37))
    ((= x 4) (g 22 (f x)))
    ((= x 5) (+ 22 (f x) 33 (g y)))
    (else (h (f x) (- 44 y) (g y)))))
]
}

变换为

@eopl-code{
@racketblock[
(lambda (x k)
  (cond
    ((zero? x) (k 17))
    ((= x 1) (f (- x 13) 7 k))
    ((= x 2) (k (+ 22 (- x 3) x)))
    ((= x 3) (f x (lambda (v1) (k (+ 22 v1 37)))))
    ((= x 4) (f x (lambda (v1) (g 22 v1 k))))
    ((= x 5) (f x (lambda (v1)
                    (g y (lambda (v2)
                           (k (+ 22 v1 33 v2))))))
      (else (f x (lambda (v1)
                   (g y (lambda (v2)
                          (h v1 (- 44 y) v2 k)))))))))
]
}

@eopl-caption["fig-6.7"]{CPS 变换示例（Scheme）}
}

第一种情况是常量。常量直接传给续文，就像上面 @tt{(zero? x)} 这一行。

@nested{
@eopl-equation{
@verbatim|{
(cps-of-exp |@${n} |@${K}) = (|@${K} |@${n})
}|
}

其中，@${K} 表示续文，是一个 simple-exp。

}

同样，变量直接传给续文。

@eopl-equation{
@verbatim|{
(cps-of-exp |@${var} |@${K}) = (|@${K} |@${var})
}|
}

当然，我们算法的输入输出都是抽象语法树，所以我们应该用抽象语法构造器，而不是具体
语法，就像：

@nested{
@eopl-equation{
@verbatim|{
(cps-of-exp (const-exp |@${n}) |@${K})
= (make-send-to-cont |@${K} (cps-const-exp |@${n}))

(cps-of-exp (var-exp |@${var}) |@${K})
= (make-send-to-cont |@${K} (cps-var-exp |@${var}))
}|
}

其中：

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{make-send-to-cont}} : @${\mathit{SimpleExp} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define make-send-to-cont
  (lambda (k-exp simple-exp)
    (cps-call-exp k-exp (list simple-exp))))
]}

我们需要 @tt{list}，因为在 CPS-OUT 中，每个调用表达式都取一个操作数列表。

}

但是在规范中，我们仍然使用具体语法，因为具体语法通常更容易读懂。

@eopl-index["Declaration" "of procedures"]
过程呢？我们转换@figure-ref{fig-6.7} 中那样的 @tt{(lambda (x) ...)} 过程时，为其
新增一个参数 @tt{k}，然后转换主体，并将主体的值传给续文 @tt{k}。我们
在@figure-ref{fig-6.7} 中正是这样做的。所以

@nested{
@eopl-code{
@verbatim|{
proc (|@${var_1}, ..., |@${var_n}) |@${exp}
}|
}

变成：

@eopl-code{
@verbatim|{
proc (|@${var_1}, ..., |@${var_n}, k) (cps-of-exp |@${exp} k)
}|
}

就像图中那样。但是，这还没完。我们的目标是生成代码，求 @tt{proc} 表达式的值，并
将结果传给续文 @${K}。所以 @tt{proc} 表达式的完整规范为：

@eopl-equation{
@verbatim|{
(cps-of-exp <<proc (|@${var_1}, ..., |@${var_n}) |@${exp}>> |@${K})
= (|@${K} <<proc (|@${var_1}, ..., |@${var_n}, k) (cps-of-exp |@${exp} k)>>)
}|
}

}

其中，@tt{k} 是新变量，@${K} 表示续文，是任意简单表达式。

有操作数的表达式呢？我们暂时给语言添加任意多个操作数的求和表达式。要添加这种表达
式，我们给 CPS-IN 的语法添加生成式：

@nested[#:style small]{
@envalign*{\mathit{InpExp} &::= @tt{+(@m{\mathit{\{InpExp\}^{*(,)}}})} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{sum-exp (exps)}}}}

给 CPS-OUT 的语法添加生成式：

@nested[#:style small]{
@envalign*{\mathit{SimpleExp} &::= @tt{+(@m{\mathit{\{SimpleExp\}^{*(,)}}})} \\[-3pt]
            &\mathrel{\phantom{::=}} \fbox{@tt{cps-sum-exp (simple-exps)}}}}

这个新生成式仍保持性质：简单表达式内不会出现过程调用。

@tt{(cps-of-exp @${\textnormal{\guillemotleft}}+(@${exp_1}, ...,
@${exp_n})@${\textnormal{\guillemotright}} @${K})} 可能是什么呢？可能 @${exp_1},
@${\dots}, @${exp_n} 都是简单的，就像@figure-ref{fig-6.7} 中的 @tt{(= x 2)}。那
么，整个表达式都是简单的，我们可以将它的值接传给续文。设 @${simp} 为一简单表达式，
那么有：

@eopl-equation{
@verbatim|{
(cps-of-exp <<+(|@${simp_1}, ..., |@${simp_n})>> |@${K})
= (|@${K} <<+(|@${simp_1}, ..., |@${simp_n})>>)
}|
}

如果操作数不是简单的呢？那么求值续文需要给其值命名，然后继续求和，就像上面
@tt{(= x 3)} 这行。其中的第二个是首个复杂操作数。那么我们的 CPS 转换器应具有性质：

@eopl-equation{
@verbatim|{
(cps-of-exp <<+(|@${simp_1}, |@${exp_2}, |@${simp_3}, ..., |@${simp_n})>> |@${K})
= (cps-of-exp |@${exp_2}
    <<proc (|@${var_2}) (|@${K} +(|@${simp_1}, |@${var_2}, |@${simp_3}, ..., |@${simp_n}))>>)
}|
}

如果 @${exp_2} 只是一个过程调用，那么输出和图中相同。但 @${exp_2} 可能更复杂，所
以我们递归调用 @tt{cps-of-exp} 处理 @${exp_2} 和更大的续文：

@eopl-code{
@verbatim|{
proc (|@${var_2}) (|@${K} +(|@${simp_1}, |@${var_2}, |@${simp_3}, ..., |@${simp_n}))
}|
}

而求和表达式中，还有另一种复杂操作数，就像 @tt{(= x 5)} 这种。所以，不是直接使用
续文

@nested{
@eopl-code{
@verbatim|{
proc (|@${var_2}) (|@${K} +(|@${simp_1}, |@${var_2}, ..., |@${simp_n}))
}|
}

我们还要递归处理更大的参数。我们可以把这条规则总结为：

@eopl-equation{
@verbatim|{
(cps-of-exp <<+(|@${simp_1}, |@${exp_2}, |@${exp_3}, ..., |@${exp_n})>> |@${K})
= (cps-of-exp |@${exp_2}
    <<proc (|@${var_2})
       (cps-of-exp <<+(|@${simp_1}, |@${var_2}, |@${exp_3}, ..., |@${exp_n}))>> |@${K})
}|
}

}

@tt{cps-of-exp} 的每个递归调用都保证会终止。第一个调用会终止是因为 @${exp_2} 比
原表达式小。第二个调用会终止是还是因为其参数也比原来的小：@${var_2} 总是比
@${exp_2} 小。

例如，查看 @tt{(= x 5)} 这一行，并使用 CPS-IN 的语法，我们有：

@eopl-code{
@verbatim|{
(cps-of-exp <<+((f x), 33, (g y))>> |@${K})
= (cps-of-exp <<(f x)>>
    <<proc (|@${v1})
       (cps-of-exp <<+(v1, 33, (g y))>> |@${K})>>)
= (cps-of-exp <<(f x)>>
    <<proc (|@${v1})
       (cps-of-exp <<(g y)>>
         <<proc |@${v2}
            (cps-of-exp <<+(v1, 33, v2)>> |@${K})))
= (cps-of-exp <<(f x)>>
    <<proc (|@${v1})
       (cps-of-exp <<(g y)>>
         <<proc |@${v2}
            (|@${K} <<+(v1, 33, v2)>>)))
= (f x
   proc (v1)
    (g y
     proc (v2)
      (|@${K} <<+(v1, 33, v2)>>)))
}|
}

过程调用与之类似。如果操作符和操作数都是简单的，那么我们添加续文参数，直接调用过
程，就像 @tt{(= x 2)} 这行。

@eopl-equation{
@verbatim|{
(cps-of-exp <<(|@${simp_0} |@${simp_1} ... |@${simp_n})>> |@${K})
= (|@${simp_0} |@${simp_1} ... |@${simp_n} |@${K})
}|
}

另一方面，如果某个操作数是复杂的，那么我们必须先求它的值，像 @tt{(= x 4)} 这行。

@eopl-equation{
@verbatim|{
(cps-of-exp <<(|@${simp_0} |@${simp_1} |@${exp_2} |@${exp_3} ... |@${exp_n})>> |@${K})
= (cps-of-exp |@${exp_2}
    <<proc (|@${var_2})
       (cps-of-exp <<(|@${simp_0} |@${simp_1} |@${var_2} |@${exp_3} ... |@${exp_n})>> |@${K})>>)
}|
}

而且，像之前那样，@tt{cps-of-exp} 的第二个调用递归进入过程调用之中，为每个复杂参
数调用 @tt{cps-of-exp}，直到所有参数都是简单参数。

写成 CPS-IN 的例子 @tt{(= x 5)} 可用这些规则处理如下：

@eopl-code{
@verbatim|{
(cps-of-exp <<(h (f x) -(44,y) (g y))>> |@${K})
= (cps-of-exp <<(f x)>>
    <<proc (v1)
       (cps-of-exp <<(h v1 -(44,y) (g y))>> |@${K})>>)
= (f x
   proc (v1)
    (cps-of-exp <<(h v1 -(44,y) (g y))>> |@${K}))
= (f x
   proc (v1)
    (cps-of-exp <<(g y)>>
      <<proc (v2)
         (cps-of-exp <<(h v1 -(44,y) v2)>> |@${K})))
= (f x
   proc (v1)
    (g y
     proc (v2)
      (cps-of-exp <<(h v1 -(44,y) v2)>> |@${K})))
= (f x
   proc (v1)
    (g y
     proc (v2)
      (h v1 -(44,y) v2 |@${K})))
}|
}

求和表达式和过程调用的规范遵循同样的模式：找出第一个复杂操作数，递归处理那个操作
数和修改过的操作数列表。这对任何求值操作数的表达式都有效。如果 @tt{complex-exp}
是某个需要求值操作数的 CPS-IN 表达式，那么我们有：

@nested{
@eopl-equation{
@verbatim|{
(cps-of-exp (complex-exp |@${simp_0} |@${simp_1} |@${exp_2} |@${exp_3} ... |@${exp_n}) |@${K})
= (cps-of-exp |@${exp_2}
    <<proc (|@${var_2})
       (cps-of-exp
         (complex-exp |@${simp_0} |@${simp_1} |@${exp_2} |@${exp_3} ... |@${exp_n})
         |@${K})>>)
}|
}

其中，@${var_2} 是一个新变量。

}

处理求和表达式和过程调用的唯一不同之处是在所有参数都简单时。在这种情况下，我们要
把每个参数转换为 CPS-OUT 中的 @tt{simple-exp}，并用转换结果生成一个尾式。

我们可以把这种行为封装到过程 @tt{cps-of-exps} 中，如@figure-ref{fig-6.8} 所示。
它的参数是输入表达式的列表和过程 @tt{builder}。它用@exercise-ref{ex1.23} 中的
@tt{list-index}，找出列表中第一个复杂表达式的位置。如果有这样的复杂表达式，那么
它转换该表达式，转换所在的续文给表达式的结果命名（绑定到 @tt{var} 的标识符），然
后递归处理修改后的表达式列表。

如果不存在复杂表达式，那么我们用 @tt{builder} 处理表达式列表。但这些表达式虽是简
单的，它们仍属于 CPS-IN 的语法。因此，我们用过程 @tt{cps-of-simple-exp} 把每个表
达式转换为 CPS-OUT 的语法。然后，我们把 @${\mathit{SimpleExp}} 的列表传给
@tt{builder}（@tt{list-set} 如@exercise-ref{ex1.19} 所述）。

过程 @tt{inp-exp-simple?} 取一 CPS-IN 表达式，判断表示它的字符串能否解析为
@${\mathit{SimpleExp}}。它使用@exercise-ref{ex1.24} 中的过程 @tt{every?}。若
@${lst} 中的所有元素满足 @${pred}，@tt{(every? @${pred} @${lst})} 返回 @tt{#t}，
否则返回@tt{#f}。

@tt{cps-of-simple-exp} 的代码直截了当，如@figure-ref{fig-6.9} 所示。它还将
@tt{proc-exp} 的主体翻译做 CPS 变换。若要使输出为 @${\mathit{SimpleExp}}，这是必
要的。

我们可以用 @tt{cps-of-exps} 生成求和表达式和过程调用的尾式。

@eopl-figure[#:position "!ht"]{

@racketblock[
@#,elem{@bold{@tt{cps-of-exps}} : @${\mathit{Listof(InpExp)} \times \mathit{(Listof(InpExp) \to TfExp)} \to \mathit{TfExp}}}
(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ((exps exps))
      @#,elem{@bold{@tt{cps-of-rest}} : @${\mathit{Listof(InpExp)} \to \mathit{TfExp}}}
      (let ((pos (list-index
                   (lambda (exp)
                     (not (inp-exp-simple? exp)))
                   exps)))
        (if (not pos)
          (builder (map cps-of-simple-exp exps))
          (let ((var (fresh-identifier 'var)))
            (cps-of-exp
              (list-ref exps pos)
              (cps-proc-exp (list var)
                (cps-of-rest
                  (list-set exps pos (var-exp var)))))))))))

@#,elem{@bold{@tt{inp-exp-simple?}} : @${\mathit{InpExp} \to \mathit{Bool}}}
(define inp-exp-simple?
  (lambda (exp)
    (cases expression exp
      (const-exp (num) #t)
      (var-exp (var) #t)
      (diff-exp (exp1 exp2)
        (and (inp-exp-simple? exp1) (inp-exp-simple? exp2)))
      (zero?-exp (exp1) (inp-exp-simple? exp1))
      (proc-exp (ids exp) #t)
      (sum-exp (exps) (every? inp-exp-simple? exps))
      (else #f))))
]

@eopl-caption["fig-6.8"]{@tt{cps-of-exps}}
}

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{cps-of-sum-exp}} : @${\mathit{Listof(InpExp)} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-sum-exp
  (lambda (exps k-exp)
    (cps-of-exps exps
      (lambda (simples)
        (make-send-to-cont
          k-exp
          (cps-sum-exp simples))))))
]}

@eopl-figure[#:position "!ht"]{

@racketblock[
@#,elem{@bold{@tt{cps-of-simple-exp}} : @${\mathit{InpExp} \to \mathit{SimpleExp}}}
@#,elem{@bold{用法} : @tt{设 (inp-exp-simple? exp) = #f}}
(define cps-of-simple-exp
  (lambda (exp)
    (cases expression exp
      (const-exp (num) (cps-const-exp num))
      (var-exp (var) (cps-var-exp var))
      (diff-exp (exp1 exp2)
        (cps-diff-exp
          (cps-of-simple-exp exp1)
          (cps-of-simple-exp exp2)))
      (zero?-exp (exp1)
        (cps-zero?-exp (cps-of-simple-exp exp1)))
      (proc-exp (ids exp)
        (cps-proc-exp (append ids (list 'k%00))
          (cps-of-exp exp (cps-var-exp 'k%00))))
      (sum-exp (exps)
        (cps-sum-exp (map cps-of-simple-exp exps)))
      (else
        (report-invalid-exp-to-cps-of-simple-exp exp)))))
]

@eopl-caption["fig-6.9"]{@tt{cps-of-simple-exp}}
}

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{cps-of-call-exp}} : @${\mathit{InpExp} \times \mathit{Listof(InpExp)} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-call-exp
  (lambda (rator rands k-exp)
    (cps-of-exps (cons rator rands)
      (lambda (simples)
        (cps-call-exp
          (car simples)
          (append (cdr simples) (list k-exp)))))))
]}

现在，我们可以写出 CPS 翻译器的剩余部分
（@figure-ref{fig-6.10}--@countref{fig-6.12}）。它@emph{跟随语法}。当表达式总是
简单的，如常量、变量和过程，我们直接用 @tt{make-send-to-cont} 生成代码。否则，我
们调用辅助过程，每个辅助过程都调用 @tt{cps-of-exps} 求子表达式的值，用适当的生成
器构造 CPS 输出的最内部。一个例外是 @tt{cps-of-letrec-exp}，它没有紧邻的子表达式，
所以它直接生成 CPS 输出。最后，我们调用 @tt{cps-of-exps} 翻译整个程序，它取一生
成器，该生成器直接返回一个简单表达式。

在下面的练习中，用 CPS-OUT 的语法和解释器运行输出表达式，确保它们是尾式。

@eopl-figure{

@racketblock[
@#,elem{@bold{@tt{cps-of-exp}} : @${\mathit{InpExp} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-exp
  (lambda (exp k-exp)
    (cases expression exp
      (const-exp (num)
        (make-send-to-cont k-exp (cps-const-exp num)))
      (var-exp (var)
        (make-send-to-cont k-exp (cps-var-exp var)))
      (proc-exp (vars body)
        (make-send-to-cont k-exp
          (cps-proc-exp (append vars (list 'k%00))
            (cps-of-exp body (cps-var-exp 'k%00)))))
      (zero?-exp (exp1)
        (cps-of-zero?-exp exp1 k-exp))
      (diff-exp (exp1 exp2)
        (cps-of-diff-exp exp1 exp2 k-exp))
      (sum-exp (exps)
        (cps-of-sum-exp exps k-exp))
      (if-exp (exp1 exp2 exp3)
        (cps-of-if-exp exp1 exp2 exp3 k-exp))
      (let-exp (var exp1 body)
        (cps-of-let-exp var exp1 body k-exp))
      (letrec-exp (p-names b-varss p-bodies letrec-body)
        (cps-of-letrec-exp
          p-names b-varss p-bodies letrec-body k-exp))
      (call-exp (rator rands)
        (cps-of-call-exp rator rands k-exp)))))

@#,elem{@bold{@tt{cps-of-zero?-exp}} : @${\mathit{InpExp} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-zero?-exp
  (lambda (exp1 k-exp)
    (cps-of-exps (list exp1)
      (lambda (simples)
        (make-send-to-cont
          k-exp
          (cps-zero?-exp
            (car simples)))))))
]

@eopl-caption["fig-6.10"]{@tt{cps-of-exp}，第1部分
                          @eopl-index[#:range-mark 'start "Continuation-passing style" "transformation to"]}
}

@eopl-figure{

@racketblock[
@#,elem{@bold{@tt{cps-of-diff-exp}} : @${\mathit{InpExp} \times \mathit{InpExp} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-diff-exp
  (lambda (exp1 exp2 k-exp)
    (cps-of-exps
      (list exp1 exp2)
      (lambda (simples)
        (make-send-to-cont
          k-exp
          (cps-diff-exp
            (car simples)
            (cadr simples)))))))

@#,elem{@bold{@tt{cps-of-if-exp}} : @${\mathit{InpExp} \times \mathit{InpExp} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-if-exp
  (lambda (exp1 exp2 exp3 k-exp)
    (cps-of-exps (list exp1)
      (lambda (simples)
        (cps-if-exp (car simples)
          (cps-of-exp exp2 k-exp)
          (cps-of-exp exp3 k-exp))))))

@#,elem{@bold{@tt{cps-of-let-exp}} : @${\mathit{Var} \times \mathit{InpExp} \times \mathit{InpExp} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-let-exp
  (lambda (id rhs body k-exp)
    (cps-of-exp
      (call-exp
        (proc-exp (list id) body)
        (list rhs))
      k-exp)))

@#,elem{@bold{@tt{cps-of-letrec-exp}} : @linebreak[] @${\phantom{x}\mathit{Listof(Var)} \times \mathit{Listof(Listof(Var))} \times \mathit{Listof(InpExp)} \times \mathit{InpExp} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-letrec-exp
  (lambda (p-names b-varss p-bodies letrec-body k-exp)
    (cps-letrec-exp
      p-names
      (map
        (lambda (b-vars) (append b-vars (list 'k%00)))
        b-varss)
      (map
        (lambda (p-body)
          (cps-of-exp p-body (cps-var-exp 'k%00)))
        p-bodies)
      (cps-of-exp letrec-body k-exp))))
]

@eopl-caption["fig-6.11"]{@tt{cps-of-exp}，第2部分}
}

@eopl-figure{

@racketblock[
@#,elem{@bold{@tt{cps-of-program}} : @${\mathit{InpExp} \to \mathit{TfExp}}}
(define cps-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (cps-a-program
          (cps-of-exps (list exp1)
            (lambda (new-args)
              (simple-exp->exp (car new-args)))))))))
]

@eopl-caption["fig-6.12"]{@tt{cps-of-exp}，第3部分
                          @eopl-index[#:range-mark 'end "Continuation-passing style" "transformation to"]}
}

@eopl-index[#:range-mark 'end "Continuation-passing style" "transformation to"]

@exercise[#:level 1 #:tag "ex6.20"]{

我们的过程 @tt{cps-of-exps} 迫使子表达式按从左向右的顺序求值。修改
@tt{cps-of-exps}，使子表达式从右向左求值。

}

@exercise[#:level 1 #:tag "ex6.21"]{

修改 @tt{cps-of-call-exp}，先从左向右求出操作数的值，再求操作符的值。

}

@exercise[#:level 1 #:tag "ex6.22"]{

有时，当我们生成 @tt{(@${K} @${simp})}，@${K} 已经是一个 @tt{proc-exp}。所以，不
是生成：

@eopl-code{
@verbatim|{
(proc (|@${var_1}) ... |@${simp})
}|
}

而应生成：

@eopl-code{
@verbatim|{
let |@${var_1} = |@${simp}
in ...
}|
}

那么，CPS 代码具有性质：形如

@eopl-code{
@verbatim|{
(proc (|@${var}) |@${exp_1}
 |@${simp})
}|
}

的子表达式若不在原表达式中，则不会出现在 CPS 代码中。

修改 @tt{make-send-to-cont}，生成更好的代码。新的规则何时生效？

}

@exercise[#:level 2 #:tag "ex6.23"]{

@eopl-index[#:suffix @exer-ref-range["ex6.23"] "Conditionals"]
观察可知，@tt{if} 的规则导致续文 @${K} 复制两次，所以在嵌套的 @tt{if} 中，转换后
的代码尺寸呈指数增长。运行一个例子，验证这一观察。然后，修改转换，把 @${K} 绑定
到新的变量，从而避免这种增长。

}

@exercise[#:level 2 #:tag "ex6.24"]{

给语言添加列表（@exercise-ref{ex3.10}）。记住：列表的参数不在尾端。

}

@exercise[#:level 2 #:tag "ex6.25"]{

扩展 CPS-IN，让 @tt{let} 表达式声明任意数量的变量（@exercise-ref{ex3.16}）。

}

@exercise[#:level 2 #:tag "ex6.26"]{

由 @tt{cps-of-exps} 引入的续文变量在续文中只会只会出现一次。修改
@tt{make-send-to-cont}，不是生成@exercise-ref{ex6.22} 中的

@eopl-code{
@verbatim|{
let |@${var_1} = |@${simp_1}
in |@${T}
}|
}

而是生成 @${T[simp_1/var_1]}。其中，符号 @${E_1[E_2/var]} 意为，把表达式 @${E_1}
中自由出现的所有变量 @${var} 替换为 @${E_2}。

}

@exercise[#:level 2 #:tag "ex6.27"]{

按当前方式，@tt{cps-of-let-exp} 生成一个无用的 @tt{let} 表达式（为什么？）。修改
这个过程，直接把 @tt{let} 变量作为续文变量。那么，若 @${exp_1} 是复杂的，

@eopl-equation{
@verbatim|{
(cps-of-exp <<let |@${var_1} = |@${exp_1} in |@${exp_2}>> |@${K})
= (cps-of-exp |@${exp_1} <<proc (|@${var_1}) (cps-of-exp |@${exp_2} |@${K})>>)
}|
}

}

@exercise[#:level 1 #:tag "ex6.28"]{

试想：有一个 Scheme 程序的 CPS 转换器，用它转换@secref{expr}中的第一个解释器，结
果会怎样？

}

@exercise[#:level 2 #:tag "ex6.29"]{

考虑 @tt{cps-of-exps} 的变体。

@eopl-code{
@racketblock[
(define cps-of-exps
  (lambda (exps builder)
    (let cps-of-rest ((exps exps) (acc '()))
      @#,elem{@bold{@tt{cps-of-rest}} : @${\mathit{Listof(InpExp)} \times \mathit{Listof(SimpleExp)} \to \mathit{TfExp}}}
      (cond
        ((null? exps) (builder (reverse acc)))
        ((inp-exp-simple? (car exps))
          (cps-of-rest (cdr exps)
            (cons
              (cps-of-simple-exp (car exps))
              acc)))
        (else
          (let ((var (fresh-identifier 'var)))
            (cps-of-exp (car exps)
              (cps-proc-exp (list var)
                (cps-of-rest (cdr exps)
                  (cons
                    (cps-of-simple-exp (var-exp var))
                    acc))))))))))
]
}

为什么 @tt{cps-of-exp} 的这种变体比@figure-ref{fig-6.8} 中的更高效？

}

@exercise[#:level 2 #:tag "ex6.30"]{

调用 @tt{cps-of-exps} 处理长度为 1 的表达式列表可以简化如下：

@eopl-equation{
@verbatim|{
(cps-of-exps (list |@${exp}) |@${builder})
= (cps-of-exp/ctx |@${exp} (lambda (simp) (|@${builder} (list simp))))
}|
}

其中，

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{cps-of-exp/ctx}} : @${\mathit{InpExp} \times \mathit{(SimpleExp → TfExp)} \to \mathit{TfExp}}}
(define cps-of-exp/ctx
  (lambda (exp context)
    (if (inp-exp-simple? exp)
      (context (cps-of-simple-exp exp))
      (let ((var (fresh-identifier 'var)))
        (cps-of-exp exp
          (cps-proc-exp (list var)
            (context (cps-var-exp var))))))))
]
}

这样，由于列表的参数数量已经确定，我们可以简化出现 @tt{(cps-of-exps (list ...))}
的地方。那么，诸如 @tt{cps-of-diff-exp} 可以用 @tt{cps-of-exp/ctx} 定义，而不需
要 @tt{cps-of-exps}。

@eopl-code{
@racketblock[
(define cps-of-diff-exp
  (lambda (exp1 exp2 k-exp)
    (cps-of-exp/ctx exp1
      (lambda (simp1)
        (cps-of-exp/ctx exp2
          (lambda (simp2)
            (make-send-to-cont k-exp
              (cps-diff-exp simp1 simp2))))))))
]
}

对 @tt{cps-of-call-exp} 中用到的 @tt{cps-of-exps}，我们可以用
@tt{cps-of-exp/ctx} 处理 @tt{rator}，但仍需使用 @tt{cps-of-exps} 处理 @tt{rands}。
删除翻译器中其他地方出现的 @tt{cps-of-exps}。

}

@exercise[#:level 3 #:tag "ex6.31"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex6.31"] "Continuations" "data structure representation of"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex6.31"] "Data structure representation" @eopl-index-entry["of continuations" "continuations"]]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex6.31"] "Data structure representation" @eopl-index-entry["of procedure values" "procedurevalues"]]
写一个翻译器，它取 @tt{cps-of-program} 的输出，生成一个等价程序，其中所有的续文
都用@secref{cpi}中的数据结构表示。用列表表示那些用 @tt{define-datatype} 生成的数
据结构。由于我们的语言不支持符号，你可以在首项位置使用整数标签，以此区分数据类型
的变体。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex6.31"] "Continuations" "data structure representation of"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex6.31"] "Data structure representation" @eopl-index-entry["of continuations" "continuations"]]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex6.31"] "Data structure representation" @eopl-index-entry["of procedure values" "procedurevalues"]]

}

@exercise[#:level 3 #:tag "ex6.32"]{

写一个翻译器，它类似@exercise-ref{ex6.31}，但把所有过程表示为数据结构。

}

@exercise[#:level 3 #:tag "ex6.33"]{

写一个翻译器，它取@exercise-ref{ex6.32} 的输出，将其转换为@figure-ref{fig-6.1}
那样的寄存器程序。

}

@exercise[#:level 2 #:tag "ex6.34"]{

@eopl-index[#:suffix @exer-ref-range["ex6.34" "ex6.35"] @eopl-index-entry["A-normal form (ANF)" "Anormalform"]]
我们把程序转换为 CPS 时，不仅将程序中的控制上下文变为显式的，而且还确定了操作的
执行顺序，以及所有中间结果的名字。后者叫做@term["sequentialization"]{序列化}。如
果我们不关心能否获得迭代性控制行为，我们序列化程序时可将其转换为
@term[#f]{A-normal form}，或称@term[#f]{ANF}。这里是一个 ANF 程序的例子。

@eopl-code{
@racketblock[
(define fib/anf
  (lambda (n)
    (if (< n 2)
      1
      (let ((val1 (fib/anf (- n 1))))
        (let ((val2 (fib/anf (- n 2))))
          (+ val1 val2))))))
]
}

CPS 程序传递命名中间结果的续文，从而序列化计算；ANF 程序用 @tt{let} 表达式命名所
有中间结果，从而序列化计算。

重写 @tt{cps-of-exp}，生成 ANF 程序而非 CPS 程序（对不在尾端的条件表达式，
用@exercise-ref{ex6.23} 中的方法处理）。然后，用修改后的 @tt{cps-of-exp} 处理例
@tt{fib} 的定义，验证其结果是否为 @tt{fib/anf}。最后，验证对已经是 ANF 的输入程
序，你的翻译器产生的程序与输入只有绑定变量名不同。

}

@exercise[#:level 1 #:tag "ex6.35"]{

用几个例子验证：若采用@exercise-ref{ex6.27} 中的优化方法，对 ANF 转换器
（@exercise-ref{ex6.34}）的输入和输出程序进行 CPS 变换，所得结果相同。

}

@section[#:style section-title-style-numbered #:tag "s6.4"]{建模计算效果}

CPS 的另一重要应用是提供模型，将计算效果变为显式的。计算效果——像是打印或给变量赋
值——很难用@secref{expr}使用的方程推理建模。通过 CPS 变换，我们可以将这些效果变为
显式的，就像我们在@secref{cpi}中处理非局部控制流一样。

用 CPS 建模效果时，我们的基本原则是简单表达式不应有任何效果。简单表达式不应含有
过程调用也是基于这一原则，因为过程调用可能不终止（这当然是一种效果！）。

本节，我们研究三种效果：打印，存储器（用显式引用模型），以及非标准控制流。

我们首先考虑打印。打印当然是一种效果：

@nested{
@eopl-code{
@verbatim|{
(f print(3) print(4))
}|
}

和

@eopl-code{
@verbatim|{
(f 1 1)
}|
}

即使返回同样的答案，也具有不同效果。效果还取决于参数的求值顺序。迄今为止，我们的
语言总是按从左向右的顺序求参数的值，但其他语言可能不是这样。

}

要建模这些想法，我们按照下面的方式修改 CPS 变换：

@itemlist[

 @item{我们给 CPS-IN 添加 @tt{print} 表达式：

 @nested[#:style small]{
 @envalign*{\mathit{InpExp} &::= @tt{print (@m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{print-exp (exp1)}}}}

 我们尚未写出 CPS-IN 的解释器，但解释器应当扩展，从而处理 @tt{print-exp}；它打印
 出参数的值，返回某个值（我们选任意值 38）。}

 @item{我们给 CPS-OUT 添加 @tt{printk} 表达式：

 @nested[#:style small]{
 @envalign*{\mathit{TfExp} &::= @tt{printk (@m{\mathit{SimpleExp}}) ; @m{\mathit{TfExp}}} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{cps-printk-exp (simple-exp1 body)}}}}

 表达式 @tt{printk(@${simp});@${exp}} 有一种效果：打印。因此，它必须是一个
 @${\mathit{TfExp}}，而非 @${\mathit{SimpleExp}}，且只能出现在尾端。@${exp} 的值
 成为整个 @tt{printk}表达式的值，所以 @${exp} 本身在尾端，可以是一个 @tt{tfexp}。
 那么，这部分代码可以写作：

 @eopl-code{
 @verbatim|{
 proc (v1)
  printk(-(v1,1));
   (f v1 |@${K})
 }|
 }

 要实现它，我们给 CPS-OUT 的解释器添加：

 @eopl-code{
 @racketblock[
 (printk-exp (simple body)
   (begin
     (eopl:printf "~s~%"
       (value-of-simple-exp simple env))
     (value-of/k body env cont)))
 ]}
 }

 @item{我们给 @tt{cps-of-exp} 添加一行代码，把 @tt{print} 表达式翻译为
 @tt{printk} 表达式。我们为 @tt{print} 选择任意返回值 38。所以，我们的翻译应为：

 @eopl-equation{
 @verbatim|{
 (cps-of-exp <<print(|@${simp_1})>> |@${K}) = printk(|@${simp_1}) ; (|@${K} 38)
 }|
 }

 然后，由于 @tt{print} 的参数可能是复杂的，我们用 @tt{cps-of-exps} 处理。这样，
 我们给 @tt{cps-of-exp} 新增这几行代码：

 @eopl-code{
 @racketblock[
 (print-exp (rator)
   (cps-of-exps (list rator)
     (lambda (simples)
       (cps-printk-exp
         (car simples)
         (make-send-to-cont k-exp
           (cps-const-exp 38))))))
 ]}
 }
]

来看一个更复杂的例子。

@eopl-code{
@verbatim|{
(cps-of-exp <<(f print((g x)) print(4))>> |@${K})
= (cps-of-exp <<print((g x))>>
    <<proc (v1)
       (cps-of-exp <<(f v1 print(4))>> |@${K})>>)
= (cps-of-exp <<(g x)>>
    <<proc (v2)
       (cps-of-exp <<(print v2)>>
         <<proc (v1)
            (cps-of-exp <<(f v1 print(4))>> |@${K})>>)>>)
= (g x
   proc (v2)
    (cps-of-exp <<(print v2)>>
      <<proc (v1)
        (cps-of-exp <<(f v1 print(4))>> |@${K})>>))
= (g x
   proc (v2)
    printk(v2);
    let v1 = 38
    in (cps-of-exp <<(f v1 print(4))>> |@${K}))
= (g x
   proc (v2)
    printk(v2);
    let v1 = 38
    in (cps-of-exp <<print(4)>>
         <<proc (v3)
            (cps-of-exp <<(f v1 v3)>> |@${K})>>))
= (g x
   proc (v2)
    printk(v2);
    let v1 = 38
    in printk(4);
       let v3 = 38
       in (cps-of-exp <<(f v1 v3)>> |@${K}))
= (g x
   proc (v2)
    printk(v2);
    let v1 = 38
    in printk(4);
       let v3 = 38
       in (f v1 v3 |@${k}))
}|
}

这里，我们调用 @tt{g}，调用所在的续文把结果命名为 @tt{v2}。续文打印出 @tt{v2} 的
值，把 38 传给下一续文，下一续文将 @tt{v1} 绑定到实参 38，打印出 4，然后调用下一
续文，下一续文把 @tt{v2} 绑定到实参（也是 38），然后用 @tt{v1}，@tt{v3} 和 @${K}
调用 @tt{f}。

我们按照同样的步骤建模显式引用（@secref{s4.2}）。我们给 CPS-IN 和 CPS-OUT 添加新
的语法，给 CPS-OUT 的解释器添加代码，处理新的语法，给 @tt{cps-of-exp} 添加代码，
将新的 CPS-IN 语法翻译为 CPS-OUT。对显式引用，我们需要添加创建引用，解引用和赋值
的语法。

@itemlist[

 @item{我们给 CPS-IN 添加语法：
 @eopl-index[#:range-mark 'start "Allocation" (eopl-index-entry "in store" "store")]
 @eopl-index[#:range-mark 'start "Dereferencing"]

 @nested[#:style small]{
 @envalign*{\mathit{InpExp} &::= @tt{newref (@m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{newref-exp (exp1)}} \\[5pt]
            \mathit{InpExp} &::= @tt{deref (@m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{deref-exp (exp1)}} \\[5pt]
          \mathit{InpExp} &::= @tt{setref (@m{\mathit{InpExp}} , @m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{setref-exp (exp1 exp2)}}}}
 }

 @item{我们给 CPS-OUT 添加语法：

 @nested[#:style small]{
 @envalign*{\mathit{TfExp} &::= @tt{newrefk (@m{\mathit{simple\mbox{-}exp}}, @m{\mathit{simple\mbox{-}exp}})} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{cps-newrefk-exp (simple1 simpe2)}} \\[5pt]
            \mathit{TfExp} &::= @tt{derefk (@m{\mathit{simple\mbox{-}exp}}, @m{\mathit{simple\mbox{-}exp}})} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{cps-derefk-exp (simple1 simpe2)}} \\[5pt]
            \mathit{TfExp} &::= @tt{setrefk (@m{\mathit{simple\mbox{-}exp}}, @m{\mathit{simple\mbox{-}exp}}) ; @m{\mathit{TfExp}}} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{cps-setrefk-exp (simple1 simpe2)}}}}

 @tt{newrefk} 表达式取两个参数：放入新分配单元的值，接收新位置引用的续文。
 @tt{derefk} 与之类似。由于 @tt{setrefk} 的执行通常只求效果，@tt{setrefk} 的设计
 与 @tt{printk} 类似。它将第二个参数的值赋给第一个参数的值，后者应是一个引用，然
 后尾递归式地执行第三个参数。

 在这门语言中，我们写：

@eopl-code{
@verbatim|{
newrefk(33, proc (loc1)
             newrefk(44, proc (loc2)
                          setrefk(loc1,22);
                          derefk(loc1, proc (val)
                                        -(val,1))))
}|
}

 这个程序新分配一个位置，值为 33，把 @tt{loc1} 绑定到那个位置。然后，它新分配一
 个位置，值为 44，把 @tt{loc2} 绑定到那个位置。然后，它把位置 @tt{loc1} 的内容设
 为 22。最后，它取出 @tt{loc1} 的值，把结果（应为 22）绑定到 @tt{val}，求出并返
 回@tt{-(val,1)} 的结果 21。

 要得到这种行为，我们给 CPS-OUT 的解释器添加这几行代码：

@eopl-code{
@racketblock[
(cps-newrefk-exp (simple1 simple2)
  (let ((val1 (value-of-simple-exp simple1 env))
        (val2 (value-of-simple-exp simple2 env)))
    (let ((newval (ref-val (newref val1))))
      (apply-procedure/k
        (expval->proc val2)
        (list newval)
        k-exp))))

(cps-derefk-exp (simple1 simple2)
  (apply-procedure/k
    (expval->proc (value-of-simple-exp simple2 env))
    (list
      (deref
        (expval->ref
          (value-of-simple-exp simple1 env))))
    k-exp))

(cps-setrefk-exp (simple1 simple2 body)
  (let ((ref (expval->ref
              (value-of-simple-exp simple1 env)))
         (val (value-of-simple-exp simple2 env)))
    (begin
      (setref! ref val)
      (value-of/k body env k-exp))))

]}

 }

 @item{最后，我们给 @tt{cps-of-exp} 添加这几行代码来做翻译：

@eopl-code{
@racketblock[
(newref-exp (exp1)
  (cps-of-exps (list exp1)
    (lambda (simples)
      (cps-newrefk-exp (car simples) k-exp))))

(deref-exp (exp1)
  (cps-of-exps (list exp1)
    (lambda (simples)
      (cps-derefk-exp (car simples) k-exp))))

(setref-exp (exp1 exp2)
  (cps-of-exps (list exp1 exp2)
    (lambda (simples)
      (cps-setrefk-exp
        (car simples)
        (cadr simples)
        (make-send-to-cont k-exp
          (cps-const-exp 23))))))
]}

 在最后一行，我们让 @tt{setref} 返回 23，这与 EXPLICIT-REFS 一致。
 }
]

@eopl-index[#:range-mark 'end "Allocation" (eopl-index-entry "in store" "store")]
@eopl-index[#:range-mark 'end "Dereferencing"]

@exercise[#:level 2 #:tag "ex6.36"]{

@eopl-index[#:suffix @exer-ref-range["ex6.36"] (eopl-index-entry @elem{@tt{begin} expression} "beginexpression")]
给 CPS-IN 添加 @tt{begin} 表达式（@exercise-ref{ex4.4}）。CPS-OUT 应该不需要修改。

}

@exercise[#:level 3 #:tag "ex6.37"]{

给 CPS-IN 添加隐式引用（@secref{s4.3}）。用和显式引用相同的 CPS-OUT，确保翻译器
在适当的地方插入分配和解引用。提示：回忆一下，在隐式引用出现的地方，@tt{var-exp}
不再是简单的，因为它需要读取存储器。

}

@exercise[#:level 3 #:tag "ex6.38"]{

如果一个变量不会出现在 @tt{set} 表达式的左边，它是不可变的，因此可以视为简单的。
扩展前一题的解答，按简单表达式处理所有这样的变量。@linebreak[]

}

最后是非局部控制流。我们来考虑@exercise-ref{ex5.42} 中的 @tt{letcc}。@tt{letcc}
表达式 @tt{letcc @${var} in @${body}} 将当前续文绑定到变量 @${var}。@${body} 为
该绑定的作用域。续文的唯一操作是 @tt{throw}。我们用语法 @tt{throw @${Expression}
to @${Expression}}，它需要求出两个子表达式的值。第二个表达式应返回一个续文，该续
文作用于第一个表达式的值。@tt{throw} 当前的续文则被忽略。

我们首先按照本章的方式分析这些表达式。这些表达式一定是复杂的。@tt{letcc} 的主体
部分在尾端，因为它的值就是整个表达式的值。由于 @tt{throw} 中的两个位置都需求值，
且都不是 @tt{throw} 的值（确实，@tt{throw} 没有值，因为它不会返回到紧邻的续文），
因此它们都在操作数位置。

现在，我们可以写出转换这两个表达式的规则。

@nested{
@eopl-equation{
@verbatim|{
(cps-of-exp <<letcc |@${var} in |@${body}>> |@${K})
= let |@${var} = |@${K}
  in (cps-of-exp |@${body} |@${var})

(cps-of-exp <<throw |@${simp_1} to |@${simp_2}>> |@${K})
= (|@${simp_2} |@${simp_1})
}|
}

我们仍用 @tt{cps-of-exps} 处理 @tt{throw} 可能含有的复杂参数。这里，@${K} 如期望
的那样忽略。

}

这个例子中，我们不需要给给 CPS-OUT 添加语法，因为我们正是在操作控制结构。

@exercise[#:level 1 #:tag "ex6.39"]{

在 CPS 翻译器中实现 @tt{letcc} 和 @tt{throw}。

}

@exercise[#:level 2 #:tag "ex6.40"]{

在 CPS 翻译器中添加和实现@secref{s5.4}中的 @tt{try/catch} 和 @tt{throw}。CPS-OUT
应该不需要添加任何东西，而 @tt{cps-of-exp} 改取两个续文：一个成功续文，一个错误
续文。

}
