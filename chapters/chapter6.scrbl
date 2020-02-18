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

要写出程序来做续文传递风格变换，我们需要找出输入和输出语言。我们选择LETREC作为输
入语言，补充多参数过程和多声明的@tt{letrec}表达式。其语法如图6.3所示，我们称之为
CPS-IN。为了区分这种语言和输出语言的表达式，我们把这些叫做 @emph{输入表达式}
(@emph{input expression})。

要定义CPS变换算法的可能输出，我们要找出CPI-IN的子集，在这个集合中，过程调用不产
生任何控制语境。

回忆@secref{cpi}中的原则：

@nested{
@nested[#:style tip]{
 @centered{@bold{不是过程调用导致控制语境扩大，而是操作数的求值。}}
}

那么，在

@racketblock[
(define fact
  (lambda (n)
    (if (zero? n) 1 (* n (fact (- n 1))))))
]

中，是@emph{操作数}位置调用@tt{fact}导致了控制语境的产生。相反，在

@racketblock[
(define fact-iter
  (lambda (n)
    (fact-iter-acc n 1)))

(define fact-iter-acc
  (lambda (n a)
    (if (zero? n) a (fact-iter-acc (- n 1) (* n a)))))
]

中， 过程调用都不在操作数位置。我们说这些调用在@emph{尾端} (@emph{tail
position})，因为它们的值就是整个调用的结果。我们称之为 @emph{尾调用} (@emph{tail
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

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "CPS-IN的语法"))]
}

再回忆一下原则@bold{尾调用不扩大续文}：

@nested[#:style tip]{
 @centered{@bold{尾调用不扩大续文}}

 @para[#:style tip-content]{若@${exp_1}的值作为@${exp_2}的值返回，则@${exp_1}和
 @${exp_2}应在同样的续文中执行。
 }
}

若每个过程调用，以及每个包含过程调用的子表达式都在尾端，我们称一个表达式为
@emph{曳尾式} (@emph{tail form})。这个条件表明所有过程调用都不会产生控制语境。

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
生控制语境。

CPS-IN中的尾端如图6.4所示。尾端的每个子表达式值都可以成为整个表达式的值。在传递
续文的解释器中，操作数位置的子表达式会产生新的续文。尾端的子表达式在原表达式的续
文中求值，如@elem[#:style question]{152页}所述。

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

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "CPS-IN中的尾端和操作数位置。尾端记为" @${T} "。操作数位置记为"
        @${O} "。"))]
}

我们用这种区别设计CPS转换算法的目标语言CPS-OUT。这种语言的语法如图6.5所示。这套
语法定义了CPS-IN的子集，但略有不同。生成式的名字总以@tt{cps-}开头，这样它们不会
与CPS-IN中生成式的名字混淆。

新的语法有两个非终止符，@${\mathit{SimpleExp}}和@${\mathit{TfExp}}。这种设计中，
@${\mathit{SimpleExp}}表达式决不包含任何过程调用，@${\mathit{TfExp}}表达式一定是
曳尾式。

因为@${\mathit{SimpleExp}}表达式决不包含任何过程调用，它们大致可以看成只有一行的
简单代码，对我们来说它们简单到不需使用控制堆栈。简单表达式包括@tt{proc}表达式，
因为@tt{proc}表达式立即返回一个过程值，但过程的主体必须是曳尾式。

曳尾表达式的传递续文解释器如图6.6所示。由于这种语言的过程取多个参数，我们用练习
2.10中的@tt{extend-env*}创建多个绑定，并用类似方式扩展@tt{extend-env-rec}得到
@tt{extend-env-rec*}。

在这个解释器中，所有递归调用都在（Scheme的）尾端，所以运行解释器不会在Scheme中产
生控制语境。（不全是这样：过程@tt{value-of-simple-exp}（练习6.11）会在Scheme中产
生控制语境，但这可以避免（参见练习6.18）。）

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
我们最多只能希望程序中的过程调用不产生控制语境，不论其是否执行。

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

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "CPS-OUT的语法"))]
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

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "CPS-OUT曳尾表达式的解释器"))]
}

@exercise[#:level 1 #:tag "ex6.11"]{

写出@tt{value-of-simple-exp}，完成图6.6中的解释器。

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

用上面@elem[#:style question]{200页}的CPS秘方，把下列CPS-IN表达式翻译为续文传递
风格。用图6.6中的解释器运行转换后的程序，测试它们，确保原程序和转换后的版本对所
有输入都给出同样的结果。

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

 @item{@tt{fnlrgtn}。n-list类似s-list（@elem[#:style question]{第9页}），只不过
 其中的元素不是符号，而是数字。@tt{fnlrgtn}取一n-list，一个数字@tt{n}，返回列表
 中（从左向右数）第一个大于@tt{n}的数字。一旦找到结果，就不再检查列表中剩余元素。
 例如，

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

@section[#:tag "s6.3"]{转换为续文传递风格}

@section[#:tag "s6.4"]{建模计算效果}
