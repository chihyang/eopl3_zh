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

我们通过给每个过程多传一个@emph{续文}参数实现这一目标。这种编程风格叫做@emph{续
文传递风格} (@emph{continuation-passing style})或 @deftech{CPS}，且不限于解释器。

本章，我们介绍一种系统性的方法，将任何过程转换为等效的迭代性控制行为过程。要这样，
需要将过程转换为续文传递风格。

@section[#:style section-title-style-numbered #:tag "s6.1"]{写出续文传递风格的程序}

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

在这一版中，@tt{fact/k}和@tt{apply-cont}的所有调用都在末尾，因此不产生控制上下文。

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

这个程序还能做多种变换，比如@figure-ref{fig-6.1} 中的寄存。

如@figure-ref{fig-6.2}，我们还可以将其转为跳跃式。如果我们用普通的指令式语言，我们可以把跳床改
为适当的循环。

但是，我们本章的焦点是用@figure-ref{fig-5.2} 中的过程表示法会怎样。回忆一下，在过程表示法中，续
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

@eopl-caption["fig-6.1"]{寄存后的 @tt{fact/k}}

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

@eopl-caption["fig-6.2"]{寄存后的跳跃式 @tt{fact/k}}

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

这里，像@secref{s1.3}，@${g}作为上下文参数，性质@tt{(fact/k @${n} @${g}) =
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
 @centered{@elemtag["cps-recipe"]{@bold{CPS秘方}}}

 @nested[#:style tip-content]{
 要将程序转换为续文传递风格：

 @itemlist[#:style 'ordered

 @item{给每个过程传一个额外参数（通常是@tt{cont}或@tt{k}）。}

 @item{不论过程返回常量还是变量，都将返回值传给续文，就像上面的@tt{(cont 17)}。}

 @item{过程调用在尾部时，用同样的续文@tt{cont}调用过程。}

 @item{过程调用在操作数位置时，应在新的续文中求值，该续文给调用结果命名，继续计
 算。}

]}}

这些规则虽不正式，但解释了这种模式。

@exercise[#:level 1 #:tag "ex6.1"]{

考虑@figure-ref{fig-6.2}，为什么移除@tt{fact/k}定义中的@tt{(set! pc fact/k)}和@tt{apply-cont}中
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

 @item{@tt{remove-first}（@elemref["remove-first"]{1.2.3 节}）}

 @item{@tt{list-sum}（@elemref["list-sum"]{1.3 节}）}

 @item{@tt{occurs-free?}（@elemref["occurs-free-1?"]{1.2.4 节}）}

 @item{@tt{subst}（@elemref["subst"]{1.2.5 节}）}

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

写出@figure-ref{fig-5.4}，@countref{fig-5.5} 和 @countref{fig-5.6} 中解释器的过
程表示和内联过程表示。

}

@exercise[#:level 3 #:tag "ex6.8"]{

写出@secref{s5.4}解释器的过程表示和内联过程表示。这极富挑战性，因为我们实际上有
两个观测器，@tt{apply-cont}和@tt{apply-handler}。提示：考虑修改@pageref{cps-recipe}的
秘方，给每个过程添加两个参数，一个表示@tt{apply-cont}中续文的行为，一个表示
@tt{apply-handler}中续文的行为。@linebreak[]

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

但是这和@tt{fact-iter}（@pageref{fact-iter}）完全相同！所以我们明白了，累加器通
常只是续文的一种表示方式。这令人拍案叫绝。相当一部分经典的程序优化问题原来是这一
思想的特例。

@exercise[#:level 1 #:tag "ex6.9"]{

乘法的什么性质使这种程序优化成为可能？

}

@exercise[#:level 1 #:tag "ex6.10"]{

给@tt{list-sum}设计一种简便的续文表示方式，就像上面的@tt{fact/k}那样。

}

@section[#:style section-title-style-numbered #:tag "s6.2"]{曳尾式}

要写出程序来做续文传递风格变换，我们需要找出输入和输出语言。我们选择LETREC作为输
入语言，补充多参数过程和多声明的@tt{letrec}表达式。其语法如@figure-ref{fig-6.3} 所示，我们称之为
CPS-IN。为了区分这种语言和输出语言的表达式，我们把这些叫做@emph{输入表达式}
(@emph{input expression})。

要定义CPS变换算法的可能输出，我们要找出CPI-IN的子集，在这个集合中，过程调用不产
生任何控制上下文。

回忆@secref{cpi}中的原则：

@nested{
@nested[#:style tip]{
 @centered{@bold{不是过程调用导致控制上下文扩大，而是操作数的求值。}}
}

那么，在

@racketblock[
(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))
]

中，是@emph{操作数}位置调用@tt{fact}导致了控制上下文的产生。相反，在

@racketblock[
(define fact-iter
  (lambda (n)
    (fact-iter-acc n 1)))

(define fact-iter-acc
  (lambda (n a)
    (if (zero? n) a (fact-iter-acc (- n 1) (* n a)))))
]

中， 过程调用都不在操作数位置。我们说这些调用在@emph{尾端} (@emph{tail
position})，因为它们的值就是整个调用的结果。我们称之为@emph{尾调用} (@emph{tail
call})。

}

@nested[#:style eopl-figure]{

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

@eopl-caption["fig-6.3"]{CPS-IN的语法}
}

再回忆一下原则@bold{尾调用不扩大续文}：

@nested[#:style tip]{
 @centered{@bold{尾调用不扩大续文}}

 @para[#:style tip-content]{若@${exp_1}的值作为@${exp_2}的值返回，则@${exp_1}和
 @${exp_2}应在同样的续文中执行。
 }
}

若每个过程调用，以及每个包含过程调用的子表达式都在尾端，我们称一个表达式为@emph{曳尾式}
(@emph{tail form})。这个条件表明所有过程调用都不会产生控制上下文。

因此，在Scheme中，

@nested{
@racketblock[
(if (zero? x) (f y) (g z))
]

是曳尾式，

@racketblock[
(if b
  (if (zero? x) (f y) (g z))
  (h u))
]

也是，但

@racketblock[
(+
  (if (zero? x) (f y) (g z))
  37)
]

不是。因为@tt{if}表达式包含一个不在尾端过程调用。

}

通常，要判定语言的尾端，我们必须理解其含义。尾端的子表达式具有如下属性：求值时，
其值立刻成为整个表达式的值。一个表达式可能有多个尾端。例如，@tt{if}表达式可能选
择真值分支，也可能选择假值分支。对尾端的子表达式，不需要保存信息，因此也就不会产
生控制上下文。

CPS-IN中的尾端如@figure-ref{fig-6.4} 所示。尾端的每个子表达式值都可以成为整个表达式的值。在传递
续文的解释器中，操作数位置的子表达式会产生新的续文。尾端的子表达式在原表达式的续
文中求值，如@pageref{tail-call-explain}所述。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
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

@eopl-caption["fig-6.4"]{CPS-IN中的尾端和操作数位置。尾端记为 @${T}。操作数位置
记为 @${O}。}
}

我们用这种区别设计CPS转换算法的目标语言CPS-OUT。这种语言的语法如@figure-ref{fig-6.5} 所示。这套
语法定义了CPS-IN的子集，但略有不同。生成式的名字总以@tt{cps-}开头，这样它们不会
与CPS-IN中生成式的名字混淆。

新的语法有两个非终止符，@${\mathit{SimpleExp}}和@${\mathit{TfExp}}。这种设计中，
@${\mathit{SimpleExp}}表达式决不包含任何过程调用，@${\mathit{TfExp}}表达式一定是
曳尾式。

因为@${\mathit{SimpleExp}}表达式决不包含任何过程调用，它们大致可以看成只有一行的
简单代码，对我们来说它们简单到不需使用控制堆栈。简单表达式包括@tt{proc}表达式，
因为@tt{proc}表达式立即返回一个过程值，但过程的主体必须是曳尾式。

曳尾表达式的传递续文解释器如@figure-ref{fig-6.6} 所示。由于这种语言的过程取多个参数，我们用@exercise-ref{ex2.10} 中的@tt{extend-env*}创建多个绑定，并用类似方式扩展
@tt{extend-env-rec}得到@tt{extend-env-rec*}。

在这个解释器中，所有递归调用都在（Scheme的）尾端，所以运行解释器不会在Scheme中产
生控制上下文。（不全是这样：过程@tt{value-of-simple-exp}（@exercise-ref{ex6.11}）会在Scheme中产
生控制上下文，但这可以避免（参见@exercise-ref{ex6.18}）。）

更重要的是，解释器不会产生新的续文。过程@tt{value-of/k}取一个续文参数，原封不动
地传给每个递归调用。所以，我们可以很容易地移除续文参数。

当然，没有通用的方式判断一个过程的控制行为是否是迭代式的。考虑

@nested{
@racketblock[
(lambda (n)
  (if (strange-predicate? n)
    (fact n)
    (fact-iter n)))
]

只有@tt{strange-predicate?}对所有足够大的@tt{n}都返回假时，这个过程才是迭代式的。
但即使能查看@tt{strange-predicate?}的代码，也可能无法判断这一条件的真假。因此，
我们最多只能希望程序中的过程调用不产生控制上下文，不论其是否执行。

}

@nested[#:style eopl-figure]{

@linebreak[]
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
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-letrec-exp (p-names b-varss p-bodies letrec-body)}} \\[5pt]
             \mathit{TfExp} &::= @tt{if @m{\mathit{SimpleExp}} then @m{\mathit{TfExp}} else @m{\mathit{TfExp}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{cps-if-exp (simple1 body1 body2)}} \\[5pt]
             \mathit{TfExp} &::= @tt{(@m{\mathit{SimpleExp}} @m{\{\mathit{SimpleExp}\}^{*}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{call-exp (rator rands)}}}

@eopl-caption["fig-6.5"]{CPS-OUT的语法}
}

@nested[#:style eopl-figure]{

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

@eopl-caption["fig-6.6"]{CPS-OUT曳尾表达式的解释器}
}

@exercise[#:level 1 #:tag "ex6.11"]{

写出@tt{value-of-simple-exp}，完成@figure-ref{fig-6.6} 中的解释器。

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

用上面@pageref{cps-recipe}的CPS秘方，把下列CPS-IN表达式翻译为续文传递风格。
用@figure-ref{fig-6.6} 中的解释器运行转换后的程序，测试它们，确保原程序和转换后
的版本对所有输入都给出同样的结果。

@itemlist[#:style 'ordered

 @item{@tt{removeall}。

 @nested[#:style 'code-inset]{
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

 @nested[#:style 'code-inset]{
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

 @item{@tt{remfirst}。它使用前面例子中的@tt{occurs-in?}。

 @nested[#:style 'code-inset]{
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

 @nested[#:style 'code-inset]{
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

 @nested[#:style 'code-inset]{
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

 @nested[#:style 'code-inset]{
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

 @item{@tt{fnlrgtn}。n-list类似s-list（@pageref{s-list}），只不过其中的元素不是
 符号，而是数字。@tt{fnlrgtn}取一n-list，一个数字@tt{n}，返回列表中（从左向右数）
 第一个大于@tt{n}的数字。一旦找到结果，就不再检查列表中剩余元素。例如，

 @nested[#:style 'code-inset]{
 @verbatim|{
 (fnlrgtn list(1,list(3,list(2),7,list(9)))
  6)
 }|
 }

 返回7。
 }

 @item{@tt{every}。这个过程取一谓词，一个列表，当且仅当谓词对列表中每个元素都为
 真时，返回真。

 @nested[#:style 'code-inset]{
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

补充@tt{value-of-program}和@tt{apply-cont}，完成@figure-ref{fig-6.6} 中的解释器。

}

@exercise[#:level 1 #:tag "ex6.15"]{

观察前一道练习中的解释器可知，@tt{cont}只有一个值。根据这一观察完全移除@tt{cont}
参数。

}

@exercise[#:level 1 #:tag "ex6.16"]{

寄存@figure-ref{fig-6.6} 中的解释器。

}

@exercise[#:level 1 #:tag "ex6.17"]{

把@figure-ref{fig-6.6} 中的解释器转换为跳跃式。

}

@exercise[#:level 2 #:tag "ex6.18"]{

修改CPS-OUT的语法，把简单@tt{diff-exp}和@tt{zero?-exp}的参数限制为常量和变量。这
样，语言中的@tt{value-of-simple-exp}就不必递归。

}

@exercise[#:level 2 #:tag "ex6.19"]{

写出Scheme过程@tt{tail-form?}，它取一CPS-IN程序的语法树，语法如@figure-ref{fig-6.3} 所示，判断同
一字符串是否是@figure-ref{fig-6.5} 中语法定义的曳尾式。

}

@section[#:style section-title-style-numbered #:tag "s6.3"]{转换为续文传递风格}

本节，我们开发算法，将CPS-IN程序转换为CPS-OUT程序。

就像传递续文的解释器一样，我们的翻译器@emph{跟随语法}，同样另取一个表示续文的参
数。多出的这个续文参数是一个简单表达式。

就像之前那样，我们给出例子，提出规范，然后写出程序。@figure-ref{fig-6.7} 展示了与前一节类似的
Scheme示例，但是更加详细。

@nested[#:style eopl-figure]{

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

变换为

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

@eopl-caption["fig-6.7"]{CPS变换示例（Scheme）}
}

第一种情况是常量。常量直接传给续文，就像上面@tt{(zero? x)}这一行一样。

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp |@${n} |@${K}) = (|@${K} |@${n})
}|
}

其中，@${K}表示续文，是某个简单表达式。

}

同样，变量直接传给续文。

@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp |@${var} |@${K}) = (|@${K} |@${var})
}|
}

当然，我们算法的输入输出是抽象语法树，所以我们应该用抽象语法构造器，而不是具体语
法，像：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp (const-exp |@${n}) |@${K})
= (make-send-to-cont |@${K} (cps-const-exp |@${n}))

(cps-of-exp (var-exp |@${var}) |@${K})
= (make-send-to-cont |@${K} (cps-var-exp |@${var}))
}|
}

其中：

@racketblock[
@#,elem{@bold{@tt{make-send-to-cont}} : @${\mathit{SimpleExp} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define make-send-to-cont
  (lambda (k-exp simple-exp)
    (cps-call-exp k-exp (list simple-exp))))
]

我们需要@tt{list}，因为在CPS-OUT中，每个调用表达式都取一个操作数列表。

}

但是在规范中，我们仍然使用具体语法，因为具体语法通常更易懂。

过程呢？转换@figure-ref{fig-6.7} 中的过程@tt{(lambda (x) ...)}时，我们给过程新增一个参数@tt{k}，
转换其主体，并将主体的值传给续文@tt{k}。我们在@figure-ref{fig-6.7} 中正是这样做的。所以

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
proc (|@${var_1}, ..., |@${var_n}) |@${exp}
}|
}

成为：

@nested[#:style 'code-inset]{
@verbatim|{
proc (|@${var_1}, ..., |@${var_n}, k) (cps-of-exp |@${exp} k)
}|
}

像图中那样。但是，这还没完。我们的目标是产生代码，求值@tt{proc}表达式，并把结果
传给续文@${K}。所以@tt{proc}表达式的完整规范为：

@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp <<proc (|@${var_1}, ..., |@${var_n}) |@${exp}>> |@${K})
= (|@${K} <<proc (|@${var_1}, ..., |@${var_n}, k) (cps-of-exp |@${exp} k)>>)
}|
}

}

其中，@tt{k}是新变量，@${K}表示续文，是任意简单表达式。

有操作数的表达式呢？我们临时给语言添加任意多个操作数的求和表达式。要添加这种表达
式，我们给CPS-IN添加生成式：

@envalign*{\mathit{InpExp} &::= @tt{+(@m{\mathit{\{InpExp\}^{*(,)}}})} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{sum-exp (exps)}}}

给CPS-OUT添加生成式：

@envalign*{\mathit{SimpleExp} &::= @tt{+(@m{\mathit{\{SimpleExp\}^{*(,)}}})} \\[-3pt]
            &\mathrel{\phantom{::=}} \fbox{@tt{cps-sum-exp (simple-exps)}}}

这个新生成式仍有该属性：过程调用决不出现在简单表达式中。

@tt{(cps-of-exp <<+(@${exp_1}, ..., @${exp_n})>> @${K})}可能是什么呢？有可能
@${exp_1}, @${\dots}, @${exp_n}都是简单的，就像@figure-ref{fig-6.7} 中@tt{(= x 2)}这种。那么，整
个表达式都是简单的，我们可以将其直接传给续文。设@${simp}为一简单表达式，那么我们
可以说：

@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp <<+(|@${simp_1}, ..., |@${simp_n})>> |@${K})
= (|@${K} <<+(|@${simp_1}, ..., |@${simp_n})>>)
}|
}

如果操作数不是简单的呢？那么求值续文需要给其值命名，然后继续求和，就像上面@tt{(=
x 3)}这行。其中，第二个操作数是第一个不简单的操作数。那么我们的CPS转换器有属性：

@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp <<+(|@${simp_1}, |@${exp_2}, |@${simp_3}, ..., |@${simp_n})>> |@${K})
= (cps-of-exp |@${exp_2}
    <<proc (|@${var_2}) (|@${K} +(|@${simp_1}, |@${var_2}, |@${simp_3}, ..., |@${simp_n}))>>)
}|
}

如果@${exp_2}只是一个过程调用，那么输出和图中相同。但@${exp_2}可能更复杂，所以我
们递归调用@tt{cps-of-exp}处理@${exp_2}和扩大的续文：

@nested[#:style 'code-inset]{
@verbatim|{
proc (|@${var_2}) (|@${K} +(|@${simp_1}, |@${var_2}, |@${simp_3}, ..., |@${simp_n}))
}|
}

但是，求和表达式中，还有一种复杂操作数，就像@tt{(= x 5)}这种。所以，不是直接使用
续文

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
proc (|@${var_2}) (|@${K} +(|@${simp_1}, |@${var_2}, ..., |@${simp_n}))
}|
}

我们还要递归处理更大的参数。我们可以把这条规则总结为：

@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp <<+(|@${simp_1}, |@${exp_2}, |@${exp_3}, ..., |@${exp_n})>> |@${K})
= (cps-of-exp |@${exp_2}
    <<proc (|@${var_2})
       (cps-of-exp <<+(|@${simp_1}, |@${var_2}, |@${exp_3}, ..., |@${exp_n}))>> |@${K})
}|
}

}

每次调用@tt{cps-of-exp}都保证会终止。第一次调用会终止是因为@${exp_2}比原表达式小。
第二次调用会终止是还是因为其参数比原参数小：@${var_2}总是比@${exp_2}小。

例如，查看@tt{(= x 5)}这一行，用CPS-IN的语法，我们有：

@nested[#:style 'code-inset]{
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

过程调用也这样处理。如果操作符操作数都是简单的，我们只需添加续文参数，调用过程，
就像@tt{(= x 2)}这行。

@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp <<(|@${simp_0} |@${simp_1} ... |@${simp_n})>> |@${K})
= (|@${simp_0} |@${simp_1} ... |@${simp_n} |@${K})
}|
}

如果某个操作数不是简单的，那么我们必须先求其值，像@tt{(= x 4)}这行。

@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp <<(|@${simp_0} |@${simp_1} |@${exp_2} |@${exp_3} ... |@${exp_n})>> |@${K})
= (cps-of-exp |@${exp_2}
    <<proc (|@${var_2})
       (cps-of-exp <<(|@${simp_0} |@${simp_1} |@${var_2} |@${exp_3} ... |@${exp_n})>> |@${K})>>)
}|
}

像之前那样，第二次调用@tt{cps-of-exp}进入过程调用之中，递归处理每个复杂参数，直
到所有参数都是简单参数。

这里是这些规则如何处理@tt{(= x 5)}这一行的例子，写成CPS-IN。

@nested[#:style 'code-inset]{
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
数和修改过的操作数列表。这对求值操作数的任何表达式都有效。如果@tt{complex-exp}是
某个需要求值操作数的CPS-IN表达式，那么我们有：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp (complex-exp |@${simp_0} |@${simp_1} |@${exp_2} |@${exp_3} ... |@${exp_n}) |@${K})
= (cps-of-exp |@${exp_2}
    <<proc (|@${var_2})
       (cps-of-exp
         (complex-exp |@${simp_0} |@${simp_1} |@${exp_2} |@${exp_3} ... |@${exp_n})
         |@${K})>>)
}|
}

其中，@${var_2}是一个新变量。

}

处理求和表达式和过程调用时唯一不同之处，是所有参数都简单时。在这种情况下，我们要
把每个参数转换为CPS-OUT中的@tt{simple-exp}，并用结果生成一个曳尾式。

我们可以把这种行为装入过程@tt{cps-of-exps}中，如@figure-ref{fig-6.8} 所示。它用@exercise-ref{ex1.23} 中的
@tt{list-index}，找出列表中第一个复杂表达式的位置。如果找到复杂表达式，则变换该
表达式，变换时的续文给表达式的结果命名（绑定到@tt{var}的标识符），然后递归处理修
改后的表达式列表。

如果不存在复杂表达式，那么我们用@tt{builder}处理表达式列表。这些表达式虽然是简单
的，但它们仍属CPS-IN的语法。因此，我们用过程@tt{cps-of-simple-exp}把每个表达式转
换为CPS-OUT的语法。然后，我们把@${SimpleExp}的列表传给@tt{builder}。
（@tt{list-set}如@exercise-ref{ex1.19} 所述。）

过程@tt{inp-exp-simple?}取一CPS-IN表达式，判断表示它的字符串能否解析为
@${SimpleExp}。它使用@exercise-ref{ex1.24} 中的过程@tt{every?}。若@${lst}中的每个元素满足
@${pred}，@tt{(every? @${pred} @${lst})}返回@tt{#t}，否则返回@tt{#f}。

我们可以用@tt{cps-of-exps}生成求和表达式和过程调用的曳尾式。

@nested[#:style eopl-figure]{

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

@racketblock[
@#,elem{@bold{@tt{cps-of-sum-exp}} : @${\mathit{Listof(InpExp)} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-sum-exp
  (lambda (exps k-exp)
    (cps-of-exps exps
      (lambda (simples)
        (make-send-to-cont
          k-exp
          (cps-sum-exp simples))))))
]

@nested[#:style eopl-figure]{

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

@racketblock[
@#,elem{@bold{@tt{cps-of-call-exp}} : @${\mathit{InpExp} \times \mathit{Listof(InpExp)} \times \mathit{SimpleExp} \to \mathit{TfExp}}}
(define cps-of-call-exp
  (lambda (rator rands k-exp)
    (cps-of-exps (cons rator rands)
      (lambda (simples)
        (cps-call-exp
          (car simples)
          (append (cdr simples) (list k-exp)))))))
]

现在，我们可以写出CPS翻译器剩余部分（@figure-ref{fig-6.10}-@countref{fig-6.12}）。
它 @tt{紧随语法}。当表达式一定是简单的，如常量、变量和过程，我们直接用
@tt{make-send-to-cont} 生成代码。否则，我们调用辅助过程，每个辅助过程调用
@tt{cps-of-exps} 求值它的子表达式，用适当的生成器构造CPS子表达式输出的内部。一个
例外是 @tt{cps-of-letrec-exp}，它没有@elem[#:style question]{紧挨着的}子表达式，
所以它直接生成CPS输出。最后，我们调用 @tt{cps-of-exps} 翻译整个程序，使用的生成
器直接返回@elem[#:style question]{一简单表达式作为值}。

在下面的练习中，用COS-OUT的语法和解释器运行输出表达式，确保它们是曳尾式。

@nested[#:style eopl-figure]{

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

@eopl-caption["fig-6.10"]{@tt{cps-of-exp}，第1部分}
}

@nested[#:style eopl-figure]{

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

@nested[#:style eopl-figure]{

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

@eopl-caption["fig-6.12"]{@tt{cps-of-exp}，第3部分}
}

@exercise[#:level 1 #:tag "ex6.20"]{

过程@tt{cps-of-exps}使子表达式从左向右求值。修改@tt{cps-of-exps}，使子表达式从右
向左求值。

}

@exercise[#:level 1 #:tag "ex6.21"]{

修改@tt{cps-of-call-exp}，先从左向右求出操作数的值，再求操作符的值。

}

@exercise[#:level 1 #:tag "ex6.22"]{

有时，生成@tt{(@${K} @${simp})}时，@${K}已经是一个@tt{proc-exp}。所以，不是生成：

@nested[#:style 'code-inset]{
@verbatim|{
(proc (|@${var_1}) ... |@${simp})
}|
}

而应生成：

@nested[#:style 'code-inset]{
@verbatim|{
let |@${var_1} = |@${simp}
in ...
}|
}

那么，CPS代码具有性质：形如

@nested[#:style 'code-inset]{
@verbatim|{
(proc (|@${var}) |@${exp_1}
 |@${simp})
}|
}

的表达式若不在原表达式中，则不会出现在CPS代码中。

修改@tt{make-send-to-cont}，生成更好的代码。新的规则何时生效？

}

@exercise[#:level 2 #:tag "ex6.23"]{

观察可知，@tt{if}的规则导致续文@${K}复制两次，所以嵌套@tt{if}中，转换后的代码尺
寸会以指数增加。运行一个例子，验证这一观察。然后，修改转换过程，把@${K}绑定到新
的变量，避免这种增加。

}

@exercise[#:level 2 #:tag "ex6.24"]{

给语言添加列表（@exercise-ref{ex3.10}）。记住，列表的参数不在尾端。

}

@exercise[#:level 2 #:tag "ex6.25"]{

扩展CPS-IN，让@tt{let}表达式声明任意数量的变量（@exercise-ref{ex3.16}）。

}

@exercise[#:level 2 #:tag "ex6.26"]{

由@tt{cps-of-exps}引入的变量在续文中只会只会出现一次。修改@tt{make-send-to-cont}，
不是生称@exercise-ref{ex6.22} 中的

@nested[#:style 'code-inset]{
@verbatim|{
let |@${var_1} = |@${simp_1}
in |@${T}
}|
}

而是生成@${T[simp_1/var_1]}。其中，@${E_1[E_2/var]}意为，把表达式@${E_1}中出现的
每个自由变量@${var}替换为@${E_2}。

}

@exercise[#:level 2 #:tag "ex6.27"]{

如前所述，@tt{cps-of-let-exp}生成一个无用的@tt{let}表达式。（为什么？）修改这个
过程，直接把@tt{let}变量作为续文变量。那么，若@${exp_1}是复杂的，

@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp <<let |@${var_1} = |@${exp_1} in |@${exp_2}>> |@${K})
= (cps-of-exp |@${exp_1} <<proc (|@${var_1}) (cps-of-exp |@${exp_2} |@${K})>>)
}|
}

}

@exercise[#:level 1 #:tag "ex6.28"]{

试想：有一个Scheme程序的CPS转换器，用它转换@secref{expr}中的第一个解释器，结果会
怎样？

}

@exercise[#:level 2 #:tag "ex6.29"]{

考虑@tt{cps-of-exps}的变体。

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

为什么@tt{cps-of-exp}的这种变体比@figure-ref{fig-6.8} 中的更高效？

}

@exercise[#:level 2 #:tag "ex6.30"]{

调用@tt{cps-of-exps}处理长度为1的表达式列表如下：

@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exps (list |@${exp}) |@${builder})
= (cps-of-exp/ctx |@${exp} (lambda (simp) (|@${builder} (list simp))))
}|
}

其中，

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

这样，我们可以简化@tt{(cps-of-exps (list ...))}，因为列表参数的数目已经确定。那
么，诸如@tt{cps-of-diff-exp}可以不用@tt{cps-of-exps}，而用@tt{cps-of-exp/ctx}定
义。

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

对 @tt{cps-of-call-exp} 中的 @tt{cps-of-exps}，我们可以用 @tt{cps-of-exp/ctx} 处
理 @tt{rator}，但仍需用 @tt{cps-of-exps} 处理 @tt{rands}。删除翻译器中出现的其他
@elem[#:style htt]{cps-of-exps}。

}

@exercise[#:level 3 #:tag "ex6.31"]{

写一个翻译器，它取 @tt{cps-of-program} 的输出，产生一个等价程序，其中所有的续文
都用@secref{cpi}中的数据结构表示。用列表表示那些用 @elem[#:style
htt]{define-datatype} 生成的数据结构。由于我们的语言不支持符号，你可以在首项位置
使用整数标签，以此区分数据类型的不同变体。

}

@exercise[#:level 3 #:tag "ex6.32"]{

写一个翻译器，它类似@exercise-ref{ex6.31}，但把所有过程表示为数据结构。

}

@exercise[#:level 3 #:tag "ex6.33"]{

写一个翻译器，它取@exercise-ref{ex6.32} 的输出，将其转换为@figure-ref{fig-6.1} 那样的寄存器程序。

}

@exercise[#:level 2 #:tag "ex6.34"]{

我们把程序转换为CPS时，不仅仅是产生一个明确控制上下文的程序，我们还明确了操作的顺
序，以及每个中间结果的名字。后者叫做@emph{序列化} (@emph{sequentialization})。如
果我们不关心迭代性控制行为，我们序列化程序时可将其转换为@emph{单调式}
(@emph{A-normal form})，或称@emph{ANF}。这里是ANF程序的一个例子。

@racketblock[
(define fib/anf
  (lambda (n)
    (if (< n 2)
      1
      (let ((val1 (fib/anf (- n 1))))
        (let ((val2 (fib/anf (- n 2))))
          (+ val1 val2))))))
]

CPS程序序列化计算时，命名中间结果，传递续文；而ANF程序序列化计算时，用@tt{let}表
达式命名所有中间结果。

重写@tt{cps-of-exp}，生成ANF程序而非CPS程序。（对不在尾端的条件表达式，用@exercise-ref{ex6.23} 中的办法处理。）然后，用修改后的@tt{cps-of-exp}处理例子程序
@tt{fib}的定义，验证结果是否为@tt{fib/anf}。最后，验证对已经是ANF的输入程序，除
绑定变量名不同外，你的翻译器产生的程序与输入相同。

}

@exercise[#:level 1 #:tag "ex6.35"]{

用几个例子验证：若采用@exercise-ref{ex6.27} 中的优化方法，对ANF转换器（@exercise-ref{ex6.34}）的输入和输出
程序进行CPS变换，所得结果相同。

}

@section[#:style section-title-style-numbered #:tag "s6.4"]{建模计算效果}

CPS的另一重要应用是提供一个模型，显露计算效果。计算效果——像是打印或给变量赋值——
很难用@secref{expr}中使用的方程推理建模。通过CPS变换，我们可以显露这些效果，就像
我们在@secref{cpi}中处理非局部控制流一样。

用CPS建模效果时，我们的基本原则是简单表达式不应有任何效果。简单表达式不应含有过
程调用也是因为这一原则，因为过程调用可能不终止（这当然是一种效果！）。

本节，我们研究三种效果：打印，存储器（用显式存储模型），以及非标准控制流。

我们首先考虑打印。打印当然是一种效果：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(f print(3) print(4))
}|
}

和

@nested[#:style 'code-inset]{
@verbatim|{
(f 1 1)
}|
}

效果不同，但是它们返回同样的答案。效果还取决于参数求值顺序。迄今为止，我们的语言
总是从左向右求值参数，但其他语言可能不是这样。

}

要建模这些想法，我们按照下面的方式修改CPS变换：

@itemlist[

 @item{我们给CPS-IN添加@tt{print}表达式：

 @envalign*{\mathit{InpExp} &::= @tt{print (@m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{print-exp (exp1)}}}

 我们还没有写出CPS-IN的解释器，但我们应扩展解释器处理@tt{print-exp}，它打印出参
 数的值，返回某个值（我们随便选38）。}

 @item{我们给CPS-OUT添加@tt{printk}表达式：

 @envalign*{\mathit{TfExp} &::= @tt{printk (@m{\mathit{SimpleExp}}) ; @m{\mathit{TfExp}}} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{cps-printk-exp (simple-exp1 body)}}}

 表达式@tt{printk(@${simp}) ; @${exp}}有一效果：打印。因此，它必须是一个
 @${TfExp}，而不是@${SimpleExp}，而且只能出现在尾端。@${exp}的值成为整个
 @tt{printk}表达式的值，所以@${exp}本身在尾端，可以是一个@tt{tfexp}。那么，这部
 分代码可以写作：

 @nested[#:style 'code-inset]{
 @verbatim|{
 proc (v1)
  printk(-(v1,1));
   (f v1 |@${K})
 }|
 }

 要实现它，我们给CPS-OUT的解释器添加：

 @racketblock[
 (printk-exp (simple body)
   (begin
     (eopl:printf "~s~%"
       (value-of-simple-exp simple env))
     (value-of/k body env cont)))
 ]
 }

 @item{我们给@tt{cps-of-exp}添加一行，把@tt{print}表达式翻译为@tt{printk}表达式。
 我们给@tt{print}随便选了一个返回值38。所以，我们的翻译为：

 @nested[#:style 'code-inset]{
 @verbatim|{
 (cps-of-exp <<print(|@${simp_1})>> |@${K}) = printk(|@${simp_1}) ; (|@${K} 38)
 }|
 }

 然后，由于@tt{print}的参数可能是复杂的，我们用@tt{cps-of-exps}处理。这样，我们
 给@tt{cps-of-exp}新添这几行：

 @racketblock[
 (print-exp (rator)
   (cps-of-exps (list rator)
     (lambda (simples)
       (cps-printk-exp
         (car simples)
         (make-send-to-cont k-exp
           (cps-const-exp 38))))))
 ]
 }
]

来看一个更复杂的例子。

@nested[#:style 'code-inset]{
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

这里，我们调用@tt{g}，其续文把结果命名为@tt{v2}。续文打印出@tt{v2}的值，把38传给
下一续文，下一续文将@tt{v1}绑定到实参38，打印出4，然后调用下一续文，下一续文把
@tt{v2}绑定到实参（也是38），然后用@tt{v1}，@tt{v3}和@${K}调用@tt{f}。

我们按照同样的步骤建模显式引用（@secref{s4.2}）。我们给CPS-IN和CPS-OUT添加新的语
法，给CPS-OUT的解释器添加新行处理新的语法，给@tt{cps-of-exp}添加新行，将新的
CPS-IN语法翻译为CPS-OUT。对显式引用，我们需要添加创建引用，索值和赋值的语法。

@itemlist[

 @item{我们给CPS-IN添加语法：

 @envalign*{\mathit{InpExp} &::= @tt{newref (@m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{newref-exp (exp1)}} \\[5pt]
            \mathit{InpExp} &::= @tt{deref (@m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{deref-exp (exp1)}} \\[5pt]
          \mathit{InpExp} &::= @tt{setref (@m{\mathit{InpExp}} , @m{\mathit{InpExp}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{setref-exp (exp1 exp2)}}}
 }

 @item{我们给CPS-IN添加语法：

 @envalign*{\mathit{TfExp} &::= @tt{newrefk (@m{\mathit{simple\mbox{-}exp}}, @m{\mathit{simple\mbox{-}exp}})} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{cps-newrefk-exp (simple1 simpe2)}} \\[5pt]
            \mathit{TfExp} &::= @tt{derefk (@m{\mathit{simple\mbox{-}exp}}, @m{\mathit{simple\mbox{-}exp}})} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{cps-derefk-exp (simple1 simpe2)}} \\[5pt]
            \mathit{TfExp} &::= @tt{setrefk (@m{\mathit{simple\mbox{-}exp}}, @m{\mathit{simple\mbox{-}exp}}) ; @m{\mathit{TfExp}}} \\[-3pt]
         &\mathrel{\phantom{::=}} \fbox{@tt{cps-setrefk-exp (simple1 simpe2)}}}

 @tt{newrefk}表达式取两个参数：要放入新分配单元的值，接收指向新位置的引用的续文。
 @tt{derefk}与之类似。由于@tt{setrefk}的执行通常只求效果，@tt{setrefk}的设计与
 @tt{printk}类似。它将第二个参数的值赋给第一个参数的值，后者应是一个引用，然后尾
 递归，求出第三个参数的值。

 在这门语言中，我们写：

@nested[#:style 'code-inset]{
@verbatim|{
newrefk(33, proc (loc1)
             newrefk(44, proc (loc2)
                          setrefk(loc1,22);
                          derefk(loc1, proc (val)
                                        -(val,1))))
}|
}

 这个程序新分配一个位置，值为33，把@tt{loc1}绑定到那个位置。然后，它新分配一个位
 置，值为44，把@tt{loc2}绑定到那个位置。然后，它把位置@tt{loc1}的内容设为22。最
 后，它取出@tt{loc1}的值，把结果（应为22）绑定到@tt{val}，求出并返回
 @tt{-(val,1)}的结果21。

 要得到这种行为，我们给CPS-OUT的解释器添加这几行：

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

]

 }

 @item{最后，我们给@tt{cps-of-exp}添加这些行来做翻译：

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
]

 在最后一行，我们让@tt{setref}返回23，就像EXPLICIT-REFS中一样。
 }
]

@exercise[#:level 2 #:tag "ex6.36"]{

给CPS-IN添加@tt{begin}表达式（@exercise-ref{ex4.4}）。CPS-OUT应该不需要修改。

}

@exercise[#:level 3 #:tag "ex6.37"]{

给CPS-IN添加隐式引用（@secref{s4.3}）。用和显式引用相同的CPS-OUT，确保翻译器在适
当的地方插入分配和索值。提示：回忆一下，在隐式引用出现的地方，@tt{var-exp}不再是
简单的，因为它读取存储器。

}

@exercise[#:level 3 #:tag "ex6.38"]{

如果一个变量决不出现在@tt{set}表达式的左边，它是不可变的，因此可以视为简单的。扩
展前一题的解答，按简单表达式处理所有这样的变量。@linebreak[]

}

最后是非局部控制流。我们来考虑@exercise-ref{ex5.42} 中的@tt{letcc}。@tt{letcc}表达式@tt{letcc
@${var} in @${body}}将当前续文绑定到变量@${var}。@${body}为该绑定的作用域。续文
的唯一操作是@tt{throw}。我们用语法@tt{throw @${Expression} to @${Expression}}，
它求值两个子表达式。第二个表达式应返回一个续文，作用于第一个表达式。@tt{throw}当
前的续文则忽略。

我们首先按照本章的方式分析这些表达式。这些表达式决不简单。@tt{letcc}的主体部分在
尾端，因为它的值就是整个表达式的值。由于@tt{throw}中的两个位置都需求值，且都不是
@tt{throw}的值（确实，@tt{throw}没有值，因为它不返回到紧邻的续文），它们都是操作
数位置。

现在，我们可以写出转换这两个表达式的规则。

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(cps-of-exp <<letcc |@${var} in |@${body}>> |@${K})
= let |@${var} = |@${K}
  in (cps-of-exp |@${body} |@${var})

(cps-of-exp <<throw |@${simp_1} to |@${simp_2}>> |@${K})
= (|@${simp_2} |@${simp_1})
}|
}

我们仍用@tt{cps-of-exps}处理@tt{throw}可能含有的复杂参数。这里，@${K}如期望的那
样忽略。

}

这个例子中，我们没给CPS-OUT添加语法，因为我们只是在操作控制结构。

@exercise[#:level 1 #:tag "ex6.39"]{

在CPS翻译器中实现@tt{letcc}和@tt{throw}。

}

@exercise[#:level 2 #:tag "ex6.40"]{

在CPS翻译器中添加和实现@secref{s5.4}中的@tt{try/catch}和@tt{throw}。CPS-OUT应该
不需要添加任何东西，而@tt{cps-of-exp}改取两个续文：一个成功续文，一个错误续文。

}
