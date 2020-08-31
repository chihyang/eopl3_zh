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

@title[#:style part-title-style-numbered #:tag "state"]{状态}

@section[#:style section-title-style-numbered #:tag "s4.1"]{计算的效果}

到目前为止，我们只考虑了计算产生的@emph{值} (@emph{value})，但是计算也有@emph{效
果} (@emph{effect})：它可以读取，打印，修改内存或者文件系统的状态。在现实世界中，
我们@emph{总是}对效果很感兴趣：如果一次计算不显示答案，那对我们完全没用！

产生值和产生效果有何不同？效果是@emph{全局性} (@emph{global})的，整个计算都能看
到。效果感染整个计算（故意用双关语）。

我们主要关心一种效果：给内存中的位置赋值。赋值与绑定有何区别？我们已经知道，绑定
是局部的，但变量赋值有可能是全局的。那是在本不相关的几部分计算之间@emph{共享}
(@emph{share})值。如果两个过程知道内存中的同一位置，它们就能共享信息。如果把信息
留在已知位置，同一个过程就能在当前调用和后续调用之间共享信息。

我们把内存建模为从@emph{位置} (@emph{location})到值集合的的有限映射，称值集合
为@emph{可存储值} (@emph{storable values})。出于历史原因，我们称之为@emph{存储器}
(@emph{store})。通常，一种语言中的可存储值与表达值相同，但不总是这样。这个选择是
语言设计的一部分。

代表内存位置的数据结构叫做@emph{引用} (@emph{reference})。位置是内存中可用来存值
的地方，引用是指向那个地方的数据结构。位置和引用的区别可以这样类比：位置就像文件，
引用就像一个URL。URL指向一个文件，文件包含一些数据。类似地，引用指代一个位置，位
置包含一些数据。

引用有时候又叫@emph{左值} (@emph{L-values})。这名字反映了这种数据结构与赋值语句
左边变量的联系。类似地，表达值，比如赋值语句右边表达式的值，叫做@emph{右值}
(@emph{R-values})。

我们考虑两种带有存储器的语言设计。我们把这些设计叫做@emph{显式引用}
(@emph{explicit reference})和@emph{隐式引用} (@emph{implicit reference})。

@section[#:style section-title-style-numbered #:tag "s4.2"]{EXPLICIT-REFS：显式引用语言}

在这种设计中，我们添加引用，作为另一种表达值。那么，我们有：

@nested{
@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} + \mathit{Ref(ExpVal)} \\
\mathit{DenVal} &= \mathit{ExpVal}
}

这里，@${\mathit{Ref(ExpVal)}}表示包含表达值的位置引用集合。

}

我们沿用语言中的绑定数据结构，但是添加三个新操作，用来创建和使用引用。

@itemlist[

 @item{@tt{newref}，分配新的位置，返回其引用。}

 @item{@tt{deref}，@emph{解引用} (@emph{deference}) ：返回引用指向位置中的内容。}

 @item{@tt{setref}，改变引用指向位置中的内容。}

]

我们把得到的语言称作 EXPLICIT-REFS。让我们用这些构造器写几个程序。

下面是两个过程 @tt{even} 和 @tt{odd}。它们取一参数，但是忽略它，并根据位置
@tt{x} 处的内容是偶数还是奇数返回 1 或 0。它们不是通过直接传递数据来通信，而是改
变共享变量的内容。

这个程序判断 13 是否为奇数，并返回 1。过程 @tt{even} 和 @tt{odd} 不引用它们的实
参，而是查看绑定到 @tt{x} 的位置中的内容。

@nested{

@nested[#:style 'code-inset]{
@verbatim|{
let x = newref (0)
in letrec even(dummy)
           = if zero? (deref(x))
             then 1
             else begin
                   setref(x, -(deref(x), 1));
                   (odd 888)
                  end
          odd(dummy)
           = if zero? (deref(x))
             then 0
             else begin
                   setref(x, -(deref(x), 1));
                   (even 888)
                  end
   in begin setref(x,13); (odd 888) end
}|
}

这个程序使用多声明的 @tt{letrec}（@exercise-ref{ex3.32}）和 @tt{begin} 表达式（@exercise-ref{ex4.4}）。
@tt{begin} 表达式按顺序求每个子表达式的值，并返回最后一个的值。

}

为了同我们的单参数语言保持一致，我们给 @tt{even} 和 @tt{odd} 传一个无用参数；如
果我们的过程支持任意数量的参数（@exercise-ref{ex3.21}），就可以不给这些过程传递参数。

当两个过程需要分享很多量时，这种通信方式很方便；只须给某些随调用而改变的量赋值。
同样地，一个过程可能通过一长串调用间接调用另一过程。它们可以通过一个共享变量直接
交换数据，居间的过程不需要知道它。如此，以共享变量通信可作为一种隐藏信息的方式。

赋值的另一用途是通过私有变量创建隐藏状态。这里是一个例子。

@nested[#:style 'code-inset]{
@verbatim|{
|@elemtag["g-counter"]{}let g = let counter = newref(0)
        in proc (dummy)
            begin
             setref(counter, -(deref(counter), -1));
             deref(counter)
            end
in let a = (g 11)
   in let b = (g 11)
      in -(a,b)
}|
}

这里，过程 @tt{g} 保留了一个私有变量，用来存储 @tt{g} 被调用的次数。因此，第一次
调用 @tt{g} 返回 1，第二次返回 2，整个程序的值为 @${-1}。

@nested[#:style samepage]{
下图是@tt{g}绑定时所在的环境。

@nested{
@centered{
@(image "../images/g-bound"
  #:scale 0.95
  #:suffixes (list ".pdf" ".svg")
  "g绑定时的环境")
}

可以认为，这是在@tt{g}的不同调用之间共享信息。Scheme过程@tt{gensym}用这种技术创
建唯一符号。

}
}

@exercise[#:level 1 #:tag "ex4.1"]{

如果程序写成下面这样会怎样？

@nested[#:style 'code-inset]{
@verbatim|{
let g = proc (dummy)
          let counter = newref(0)
          in begin
             setref(counter, -(deref(counter), -1));
             deref(counter)
          end
in let a = (g 11)
   in let b = (g 11)
      in -(a,b)
}|
}

}

在EXPLICIT-REFS中，我们可以存储任何表达值。引用也是表达值。这意味着我们可以在一
个位置存储引用。考虑下面的程序：

@nested[#:style 'code-inset]{
@verbatim|{
let x = newref(newref(0))
in begin
    setref(deref(x), 11);
    deref(deref(x))
end
}|
}

这段程序分配了一个新位置，内容为0。然后，它将@tt{x}绑定到一个位置，其内容为指向
第一个位置的引用。因此，@tt{deref(x)}的值是第一个位置的引用。那么程序求值
@tt{setref}时，会修改第一个位置，整个程序返回11。

@subsection[#:style section-title-style-numbered #:tag "s4.2.1"]{存储器传递规范}

在我们的语言中，任何表达式都可以有效果。要定义这些效果，我们需要描述每次求值使用
什么样的存储器，以及求值如何修改存储器。

在规范中，我们用 @deftech[#:key "sigma_for_store"]{@${\sigma}} 表示任一存储器。
我们用 @${\text{[}l=v\text{]}\sigma} 表示存储器，它与 @${\sigma} 类似，只是将位
置 @${l} 映射到@${v}。有时，指代 @${\sigma} 的某个具体值时，我们称之为存储器的
@emph{状态} (@emph{state})。

我们使用@emph{存储器传递规范} (@emph{store-passing specifications})。在存储器传
递规范中，存储器直接作为参数传递给@tt{value-of}，并作为@tt{value-of}的结果返回。
那么我们可以写：

@nested{

@$${@tt{(value-of @${exp_1} @${\rho} @${\sigma_0})} = @tt{(@${val_1},@${\sigma_1})}}

它断定表达式@${exp_1}在环境为@${\rho}，存储器状态为@${\sigma_0}求值时，返回值
@${val_1}，并且可能把存储器改成不同的状态@${\sigma_1}。

}

那么我们可以写

@nested{

@$${@tt{(value-of (const-exp @${n}) @${\rho} @${\sigma})} = @tt{(@${n},@${\sigma})}}

来指定@tt{const-exp}这样无效果的操作。这表明求值该表达式时，存储器不变。

}

@tt{diff-exp}的规范展示了如何定义有序行为。

@nested{

@$${\infer{@tt{(value-of (diff-exp @${exp_1} @${exp_2}) @${\rho} @${\sigma_0})} =
           @tt{(@${\lceil\lfloor val_1 \rfloor - \lfloor val_2 \rfloor\rceil},@${\sigma_2})}}
          {\begin{alignedat}{-1}
             @tt{(value-of (diff-exp @${exp_1}) @${\rho} @${\sigma_0})} &= @tt{(@${val_1},@${\sigma_1})} \\
             @tt{(value-of (diff-exp @${exp_2}) @${\rho} @${\sigma_1})} &= @tt{(@${val_2},@${\sigma_2})}
           \end{alignedat}}}

这里，我们从状态为@${\sigma_0}的存储器开始，首先求值@${exp_1}。@${exp_1}返回值为
@${val_1}，而且它可能有效果，把存储器状态改为@${\sigma_1}。然后我们求值@${exp_2}，
这时存储器的状态由@${exp_1}修改过，也就是@${\sigma_1}。@${exp_2}同样返回一个值
@${val_2}，并把存储器状态改为@${\sigma_2}。之后，整个表达式返回@${val_1 - val2}，
不再更改存储器，所以存储器状态留在@${\sigma_2}。

}

再来看看条件表达式。

@$${
\infer{\begin{alignedat}{-1}
         &@tt{ (value-of (if-exp @${exp_1} @${exp_2} @${exp_3}) @${\rho} @${\sigma_0}) } \\
         &\hphantom{xx}= \begin{cases}
                          @tt{(value-of @${exp_2} @${\rho} @${\sigma_1})} & 若 @tt{(expval->bool @${val_1})} = @tt{#t} \\
                          @tt{(value-of @${exp_3} @${\rho} @${\sigma_1})} & 若 @tt{(expval->bool @${val_1})} = @tt{#f} \hphantom{x}
                        \end{cases}
       \end{alignedat}}
      {@tt{(value-of @${exp_1} @${\rho} @${\sigma_0})} = @tt{(@${val_1},@${\sigma_1})}}
}

从状态@${\sigma_0}开始，@tt{if-exp}求值条件表达式@${exp_1}，返回值为@${val_1}，
存储器状态改为@${\sigma_1}。整个表达式的结果可能是@${exp_2}或@${exp_3}的结果，二
者都在当前环境@${\rho}中求值，此时存储器状态是@${exp_1}留下的@${\sigma_1}。

@exercise[#:level 1 #:tag "ex4.2"]{

写出@tt{zero?-exp}的规范。

}

@exercise[#:level 1 #:tag "ex4.3"]{

写出@tt{call-exp}的规范。

}

@exercise[#:level 2 #:tag "ex4.4"]{

写出@tt{begin}表达式的规范。

@nested{
@$${\mathit{Expression} ::= @tt{begin @${\mathit{Expression}} @${\{}@tt{; }@${\mathit{Expression}}@${\}^{*}} end}}
}

@tt{begin}表达式可能包含一个或多个子表达式，由分号分隔。这些子表达式按顺序求值，
最后一个的作为返回值。

}

@exercise[#:level 2 #:tag "ex4.5"]{

写出@tt{list}（@exercise-ref{ex3.10}）的规范。

}

@subsection[#:style section-title-style-numbered #:tag "s4.2.2"]{指定显式引用操作}

在EXPLICIT-REFS中，我们必须定义三个操作：@tt{newref}，@tt{deref}和@tt{setref}。
它们的语法为：

@envalign*{
        \mathit{Expression} &::= @tt{newref (@m{\mathit{Expression}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{newref-exp (exp1)}} \\[5pt]
        \mathit{Expression} &::= @tt{deref (@m{\mathit{Expression}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{deref-exp (exp1)}} \\[5pt]
        \mathit{Expression} &::= @tt{setref (@m{\mathit{Expression}} , @m{\mathit{Expression}})} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{setref-exp (exp1 exp2)}}}

这些操作的行为定义如下。

@$${
\infer{@tt{(value-of (newref-exp @${exp}) @${\rho} @${\sigma_0}) =
           ((ref-val @${l}),[@${l}=@${val}]@${\sigma_1})}}
      {@tt{(value-of @${exp} @${\rho} @${\sigma_0})} = @tt{(@${val},@${\sigma_1})} \quad l \notin dom(\sigma_1)}
}

这条规则是说@tt{newref-exp}求操作数的值。它扩展得到的存储器：新分配一个位置@${l}，
把它的参数值@${val}放到那个位置。然后它返回新位置@${l}的引用。这是说@${l}不在
@${\sigma_1}的定义域内。

@$${
\infer{@tt{(value-of (deref-exp @${exp}) @${\rho} @${\sigma_0}) =
           (@${\sigma_1(l)},@${\sigma_1})}}
      {@tt{(value-of @${exp} @${\rho} @${\sigma_0})} = @tt{(@${val},@${\sigma_1})}}
}

这条规则是说@tt{deref-exp}求操作数的值，把存储器状态改为@${\sigma_1}。参数的值应
是位置@${l}的引用。然后@tt{deref-exp}返回@${\sigma_1}中@${l}处的内容，不再更改存
储器。

@$${
\infer{@tt{(value-of (setref-exp @${exp_1} @${exp_2}) @${\rho} @${\sigma_0}) =
           (@${\lceil 23 \rceil},[@${l}=@${val}]@${\sigma_2})}}
      {\begin{gathered}
        @tt{(value-of @${exp_1} @${\rho} @${\sigma_0})} = @tt{(@${l},@${\sigma_1})} \\
        @tt{(value-of @${exp_2} @${\rho} @${\sigma_1})} = @tt{(@${val},@${\sigma_2})}
       \end{gathered}}
}

这条规则是说@tt{setref-exp}从左到右求操作数的值。第一个操作数的值必须是某个位置
@${l}的引用。然后@tt{setref-exp}把第二个参数的值@${val}放到位置@${l}处，从而更新
存储器。@tt{setref-exp}应该返回什么呢？它可以返回任何值。为了突出这种随机性，我
们让它返回23。因为我们对@tt{setref-exp}的返回值不感兴趣，我们说这个表达式的
执行@emph{求效果} (@emph{for effect})而不求值。

@exercise[#:level 1 #:tag "ex4.6"]{

修改上面的规则，让@tt{setref-exp}返回右边表达式的值。

}

@exercise[#:level 1 #:tag "ex4.7"]{

修改上面的规则，让@tt{setref-exp}返回位置原有内容。

}

@subsection[#:style section-title-style-numbered #:tag "s4.2.3"]{实现}

我们当前使用的规范语言可以轻松描述有效果的计算的行为，但是它没有体现存储器的一个
要点：引用最终指向现实世界存在的内存中一个真实的位置。因为我们只有一个现实世界，
我们的程序只能记录存储器的一个状态@${\sigma}。

在我们的实现中，我们利用这一事实，用Scheme中的存储器建模存储器。这样，我们就能用
Scheme中的效果建模效果。

我们用一个Scheme值表示存储器状态，但是我们不像规范建议的那样直接传递和返回它，相
反，我们在一个全局变量中记录状态，实现代码中的所有过程都能访问它。这很像在示例程
序@tt{even/odd}中，我们使用共享位置，而不是直接传递参数。使用单一全局变量时，我
们也几乎不需要理解Scheme中的效果。

我们还是要确定如何用Scheme值建模存储器。我们的选择可能是最简单的模型：以表达值列
表作为存储器，以代表列表位置的数字表示引用。分配新引用就是给列表末尾添加新值；更
新存储器则是尽量复制列表中需要的部分。代码如图4.1和4.2所示。

这种表示法极其低效。一般的内存操作大致要在常数时间内完成，但是用我们的表示法，这
些操作所需的时间与存储器大小成正比。当然，真正实现起来不会这么做，但是这足以达到
我们的目的。

我们给数据类型表达值新增一种变体@tt{ref-val}，然后修改@tt{value-of-program}，在
每次求值之前初始化存储器。

@nested{
@racketblock[
@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{SchemeVal}}}
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))
]

现在，我们可以写出@tt{value-of}中与@tt{newref}，@tt{deref}和@tt{setref}相关的语
句。这些语句如图4.3所示。

}

我们可以给该系统添加一些辅助过程，把环境、过程和存储器转换为更易读的形式，也可以
增强系统，在代码中的关键位置打印消息。我们还使用过程把环境、过程和存储器转换为更
易读的形式。得出的日志详细描述了系统的动作。典型例子如图4.4和4.5所示。此外，这个
跟踪日志还标明差值表达式的参数从左到右进行求值。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{empty-store}} : @${() \to \mathit{Sto}}}
(define empty-store
  (lambda () '()))

@#,elem{@bold{用法} : Scheme变量，包含存储器当前的状态。初始值无意义。}
(define the-store 'uninitialized)

@#,elem{@bold{@tt{get-store}} : @${() \to \mathit{Sto}}}
(define get-store
  (lambda () the-store))

@#,elem{@bold{@tt{initialize-store!}} : @${() \to \mathit{Unspecified}}}
@#,elem{@bold{用法} : @tt{(initialize-store!)}将存储器设为空。}
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

@#,elem{@bold{@tt{reference?}} : @${\mathit{SchemeVal} \to \mathit{Bool}}}
(define reference?
  (lambda (v)
    (integer? v)))

@#,elem{@bold{@tt{newref}} : @${\mathit{ExpVal} \to \mathit{Ref}}}
(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

@#,elem{@bold{@tt{deref}} : @${\mathit{Ref} \to \mathit{ExpVal}}}
(define deref
  (lambda (ref)
    (list-ref the-store ref)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "拙劣的存储器模型"))]
}

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{setref!}} : @${\mathit{Ref} \times \mathit{ExpVal} \to \mathit{Unspecified}}}
@#,elem{@bold{用法} : 把位置@tt{ref}处的值设为@tt{val}，此外@tt{the-store}与原状态相同。}
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ((setref-inner
                 @#,elem{@bold{用法} : 返回一列表，在位置@tt{ref1}处值为@tt{val}，此外与@tt{store1}相同。}
                 (lambda (store1 ref1)
                   (cond
                     ((null? store1)
                      (report-invalid-reference ref the-store))
                     ((zero? ref1)
                      (cons val (cdr store1)))
                     (else
                       (cons
                         (car store1)
                         (setref-inner
                           (cdr store1) (- ref1 1))))))))
        (setref-inner the-store ref)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "拙劣的存储器模型，续"))]
}

@exercise[#:level 1 #:tag "ex4.8"]{

指出我们实现的存储器中，到底是哪些操作花费了线性时间而非常数时间。

}

@exercise[#:level 1 #:tag "ex4.9"]{

用Scheme向量表示存储器，从而实现常数时间操作。用这种表示方法会失去什么？

}

@exercise[#:level 1 #:tag "ex4.10"]{

实现@exercise-ref{ex4.4}中定义的@tt{begin}表达式。

}

@exercise[#:level 1 #:tag "ex4.11"]{

实现@exercise-ref{ex4.5}中的@tt{list}。

}

@exercise[#:level 3 #:tag "ex4.12"]{

像解释器中展示的，我们对存储器的理解基于Scheme效果的含义。具体地说，我们得知道在
Scheme程序中这些效果@emph{何时}产生。我们可以写出更贴合规范的解释器，从而避免这
种依赖。在这个解释器中，@tt{value-of}同时返回值和存储器，就像规范中那样。这个解
释器的片段如图4.6所示。我们称之为@emph{传递存储器的解释器} (@emph{store-passing
interpreter})。补全这个解释器，处理整个EXPLICIT-REFS语言。

过程可能修改存储器时，不仅返回通常的值，还要返回一个新的存储器。它们包含在名为
@tt{answer}的数据类型之中。完成这个@tt{value-of}的定义。

}

@exercise[#:level 3 #:tag "ex4.13"]{

扩展前一道练习中的解释器，支持多参数过程。

}

@nested[#:style eopl-figure]{
@codeblock[#:indent 11]{
(newref-exp
 (exp1)
 (let ((v1 (value-of exp1 env)))
   (ref-val (newref v1))))

(deref-exp
 (exp1)
 (let ((v1 (value-of exp1 env)))
   (let ((ref1 (expval->ref v1)))
     (deref ref1))))

(setref-exp
 (exp1 exp2)
 (let ((ref (expval->ref (value-of exp1 env))))
   (let ((val2 (value-of exp2 env)))
     (begin
       (setref! ref val2)
       (num-val 23)))))
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para (tt "value-of") "的显式引用操作语句"))]
}

@nested[#:style eopl-figure]{
@verbatim|{

> (run "
let x = newref(22)
in let f = proc (z) let zz = newref(-(z,deref(x)))
                    in deref(zz)
   in -((f 66), (f 55))")

进入 let x
newref: 分配位置 0
进入 let x 主体，环境 =
((x #(struct:ref-val 0))
 (i #(struct:num-val 1))
 (v #(struct:num-val 5))
 (x #(struct:num-val 10)))
存储器 =
((0 #(struct:num-val 22)))

进入 let f
进入 let f 主体，环境 =
((f
  (procedure
   z
   ...
   ((x #(struct:ref-val 0))
    (i #(struct:num-val 1))
    (v #(struct:num-val 5))
    (x #(struct:num-val 10)))))
 (x #(struct:ref-val 0))
 (i #(struct:num-val 1))
 (v #(struct:num-val 5))
 (x #(struct:num-val 10)))
存储器 =
((0 #(struct:num-val 22)))

进入 proc z 主体，环境 =
((z #(struct:num-val 66))
 (x #(struct:ref-val 0))
 (i #(struct:num-val 1))
 (v #(struct:num-val 5))
 (x #(struct:num-val 10)))
存储器 =
((0 #(struct:num-val 22)))
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "EXPLICIT-REFS的求值跟踪日志"))]
}

@nested[#:style eopl-figure]{
@verbatim|{

进入 let zz
newref: 分配位置 1
进入 let zz 主体，环境 =
((zz #(struct:ref-val 1))
 (z #(struct:num-val 66))
 (x #(struct:ref-val 0))
 (i #(struct:num-val 1))
 (v #(struct:num-val 5))
 (x #(struct:num-val 10)))
存储器 =
((0 #(struct:num-val 22)) (1 #(struct:num-val 44)))

进入 proc z 主体，环境 =
((z #(struct:num-val 55))
 (x #(struct:ref-val 0))
 (i #(struct:num-val 1))
 (v #(struct:num-val 5))
 (x #(struct:num-val 10)))
存储器 =
((0 #(struct:num-val 22)) (1 #(struct:num-val 44)))

进入 let zz
newref: 分配位置 2
进入 let zz 主体，环境 =
((zz #(struct:ref-val 2))
 (z #(struct:num-val 55))
 (x #(struct:ref-val 0))
 (i #(struct:num-val 1))
 (v #(struct:num-val 5))
 (x #(struct:num-val 10)))
存储器 =
((0 #(struct:num-val 22))
 (1 #(struct:num-val 44))
 (2 #(struct:num-val 33)))

#(struct:num-val 11)
>
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "EXPLICIT-REFS的求值跟踪日志，续"))]
}

@nested[#:style eopl-figure]{
@racketblock[
(define-datatype answer answer?
  (an-answer
    (val exp-val?)
    (store store?)))

@#,elem{@bold{@tt{value-of}} : @${\mathit{Exp} \times \mathit{Env} \times \mathit{Sto} \to \mathit{ExpVal}}}
(define value-of
  (lambda (exp env store)
    (cases expression exp
      (const-exp (num)
        (an-answer (num-val num) store))
      (var-exp (var)
        (an-answer
          (apply-store store (apply-env env var))
          store))
      (if-exp (exp1 exp2 exp3)
        (cases answer (value-of exp1 env store)
          (an-answer (val new-store)
            (if (expval->bool val)
              (value-of exp2 env new-store)
              (value-of exp3 env new-store)))))
      (deref-exp
        (exp1)
        (cases answer (value-of exp1 env store)
          (an-answer (v1 new-store)
            (let ((ref1 (expval->ref v1)))
              (an-answer (deref ref1) new-store)))))
      ...)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "@exercise-ref{ex4.12}，传递存储器的解释器"))]
}

@section[#:style section-title-style-numbered #:tag "s4.3"]{IMPLICIT-REFS：隐式引用语言}

显式引用设计清晰描述了内存的分配、索值和修改，因为所有这些操作直接出现在程序员的
代码之中。

大多数编程语言以同样的方式处理分配、索值和修改，并把它们打包为语言的一部分。这样，
程序员不需要担心何时执行这些操作，因为它们存在于语言内部。

在这种设计中，每个变量都表示一个引用。指代值是包含表达值的位置的引用。引用不再是
表达值。它们只能作为变量绑定。

@nested{
@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)}
}

每次绑定操作都会分配一个位置：在每个过程调用处，在@tt{let}和@tt{letrec}中。

}

当变量出现在表达式中，我们首先在环境中搜索标识符，找出绑定的位置，然后在存储器中
找出那个位置的值。因此对@tt{var-exp}，我们有个@exact-elem{“}双层@exact-elem{”}
系统。

一个位置的内容可用@tt{set}表达式修改，语法为：

@envalign*{
        \mathit{Expression} &::= @tt{set @m{\mathit{Identifier}} = @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{assign-exp (var exp1)}}}

这里的@${\mathit{Identifier}}不是表达式的一部分，所以无法对其索值。在这种设计中，
我们说变量是@emph{可变的} (@emph{mutable})，意为可以修改。

这种设计叫做@emph{按值调用} (@emph{call-by-value})，或@emph{隐式引用}
(@emph{implicit reference})。大多数编程语言，包括Scheme，都使用这种设计的一些变
体。

图4.7是这种设计的两个示例程序。因为引用不再是表达值，我们不能做链式引用，像
@secref{s4.2}中的那个例子那样。

@nested[#:style eopl-figure]{
@nested[#:style 'code-inset]{
@verbatim|{
let x = 0
in letrec even(dummy)
           = if zero?(x)
             then 1
             else begin
                   set x = -(x,1);
                   (odd 888)
                  end
          odd(dummy)
           = if zero?(x)
             then 0
             else begin
                   set x = -(x,1);
                   (even 888)
                  end
   in begin set x = 13; (odd -888) end

let g = let count = 0
        in proc (dummy)
            begin
             set count = -(count,-1);
             count
            end
in let a = (g 11)
   in let b = (g 11)
      in -(a,b)
}|
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "IMPLICIT-REFS中的" (tt "odd") "和" (tt "even")))]
}

@subsection[#:style section-title-style-numbered #:tag "s4.3.1"]{规范}

我们可以轻松写出索值和@tt{set}的规则。现在，环境总是把变量绑定到位置，所以当变量
作为表达式时，我们需要索取它的值：

@$${@tt{(value-of (var-exp @${var}) @${\rho} @${\sigma})} = @tt{(@${\sigma(\rho(var))},@${\sigma})}}

赋值就像我们预想的那样：我们在环境中搜索式子左侧的标识符，获取一个位置，在环境中
求值式子右侧的表达式，修改指定位置的内容。就像@tt{setref}，@tt{set}表达式的返回
值是任意的。我们让它返回表达值27。

@$${
\infer{@tt{(value-of (assign-exp @${var} @${exp_1}) @${\rho} @${\sigma_0}) =
           (@${\lceil 27 \rceil},[@${\rho(var)}=@${val_1}]@${\sigma_1})}}
      {@tt{(value-of @${exp_1} @${\rho} @${\sigma_0})} = @tt{(@${val_1},@${\sigma_1})}}
}

我们还要重写过程调用和@tt{let}规则，体现出对存储器的修改。对过程调用，规则变成：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
(apply-procedure (procedure |@${var} |@${body} |@${\rho}) |@${val} |@${\sigma})
= (value-of |@${body} [|@${var=l}]|@${\rho} [|@${l=val}]|@${\sigma})
}|
}

其中，@${l}是不在@${\sigma}定义域中的某一位置。

}

@tt{(let-exp @${var} @${exp_1} @${body})}的规则类似。式子右边的@${exp_1}求值，环
境中的变量@${var}绑定到包含@${exp_1}值的新位置，@tt{let}表达式的主体在该环境中求
值，并作为表达式的值。

@exercise[#:level 1 #:tag "ex4.14"]{

写出@tt{let}的规则。

}

@subsection[#:style section-title-style-numbered #:tag "s4.3.2"]{实现}

现在我们准备修改解释器。在@tt{value-of}中，我们取出每个@tt{var-exp}的值，就像规
则描述的那样：

@nested{

@codeblock[#:indent 11]{(var-exp (var) (deref (apply-env env var)))}

@tt{assign-exp}的代码也显而易见：

@codeblock[#:indent 11]{
(assign-exp (var exp1)
  (begin
    (setref!
      (apply-env env var)
      (value-of exp1 env))
    (num-val 27)))
}

}

创建引用呢？新的位置应在每个绑定处创建。语言中只有四个创建新绑定的地方：初始环境
中，@tt{let}中，过程调用中，以及@tt{letrec}中。

在初始环境中，我们直接分配新位置。

对@tt{let}，我们修改@tt{value-of}中相应的行，分配包含值的新位置，并把变量绑定到
指向该位置的引用。

@codeblock[#:indent 11]{
(let-exp (var exp1 body)
  (let ((val1 (value-of exp1 env)))
    (value-of body
      (extend-env var (newref val1) env))))
}

对过称调用，我们照样修改@tt{apply-procedure}，调用@tt{newref}。

@racketblock[
@#,elem{@bold{@tt{apply-procedure}} : @${\mathit{Proc} \times \mathit{ExpVal} \to \mathit{ExpVal}}}
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body
          (extend-env var (newref val) saved-env))))))
]

最后，要处理@tt{letrec}，我们换掉@tt{apply-env}中的语句@tt{extend-env-rec}，返回
一个引用，指向包含适当闭包的位置。因为我们用的是声明多过程的@tt{letrec}（练习
3.32），@tt{extend-env-rec}取一个过程名列表，一个绑定变量列表，一个过程主体列表，
以及一个已保存的环境。过程@tt{location}取一变量，一个变量列表，若变量存在于列表
中，返回列表中的变量位置，否则返回@tt{#f}。

@codeblock[#:indent 11]{
(extend-env-rec (p-names b-vars p-bodies saved-env)
  (let ((n (location search-var p-names)))
    (if n
      (newref
        (proc-val
          (procedure
            (list-ref b-vars n)
            (list-ref p-bodies n)
            env)))
      (apply-env saved-env search-var))))
}

图4.8用前述辅助组件，展示了IMPLICIT-REFS求值的简单例子。

@nested[#:style eopl-figure]{
@verbatim|{

> (run "
let f = proc (x) proc (y)
         begin
          set x = -(x,-1);
          -(x,y)
         end
in ((f 44) 33)")
newref: 分配位置 0
newref: 分配位置 1
newref: 分配位置 2
进入 let f
newref: 分配位置 3
进入 let f 主体，环境 =
((f 3) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 (procedure x ... ((i 0) (v 1) (x 2)))))

newref: 分配位置 4
进入 proc x 主体，环境 =
((x 4) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 (procedure x ... ((i 0) (v 1) (x 2))))
 (4 #(struct:num-val 44)))

newref: 分配位置 5
进入 proc y 主体，环境 =
((y 5) (x 4) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 (procedure x ... ((i 0) (v 1) (x 2))))
 (4 #(struct:num-val 44))
 (5 #(struct:num-val 33)))

#(struct:num-val 12)
>
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "IMPLICIT-REFS的简单求值"))]
}

@exercise[#:level 1 #:tag "ex4.15"]{

在图4.8中，环境中的变量为什么绑定到平常的整数，而不是图4.5中的那样的表达值？

}

@exercise[#:level 1 #:tag "ex4.16"]{

既然变量是可变的，我们可以靠赋值产生递归过程。例如：

@nested[#:style 'code-inset]{
@verbatim|{
letrec times4(x) = if zero?(x)
                   then 0
                   else -((times4 -(x,1)), -4)
in (times4 3)
}|
}

可以替换为：

@nested[#:style 'code-inset]{
@verbatim|{
let times4 = 0
in begin
    set times4 = proc (x)
                   if zero?(x)
                   then 0
                   else -((times4 -(x,1)), -4);
    (times4 3);
   end
}|
}

手动跟踪这个程序，验证这种转换可行。

}

@exercise[#:level 2 #:tag "ex4.17"]{

写出规则并实现多参数过程和声明多变量的@tt{let}。

}

@exercise[#:level 2 #:tag "ex4.18"]{

写出规则并实现多过程的@tt{letrec}表达式。

}

@exercise[#:level 2 #:tag "ex4.19"]{

修改多过程@tt{letrec}的实现，每个闭包只需生成一次，并且只分配一个位置。本题类似
@exercise-ref{ex3.35}。

}

@exercise[#:level 2 #:tag "ex4.20"]{

在本节的语言中，所有变量都是可变的，就像在Scheme中一样。另一种设计是同时允许可变
和不可边的变量绑定：

@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)} + \mathit{ExpVal}
}

只有变量绑定可变时，变量才能赋值。当表达值是引用时，索值自动进行。

修改本节的语言，让@tt{let}像之前那样引入不可变变量，可变变量则由@tt{letmutable}
表达式引入，语法为：

@$${\mathit{Expression} ::= @tt{letmutable @${\mathit{Identifier}} = @${\mathit{Expression}} in @${\mathit{Expression}}}}

}

@exercise[#:level 2 #:tag "ex4.21"]{

之前，我们建议用赋值让两个相去很远的过程交换信息，这样二者之间的过程不需要知道，
程序更加模块化。这样的赋值经常是暂时性的，只在执行函数调用时生效。向语言
添加@emph{动态赋值} (@emph{dynamic assignment})（又称@emph{流式绑定}
(@emph{fluid binding})）组件，完成这一操作。生成式为：

@envalign*{
        \mathit{Expression} &::= @tt{setdynamic @m{\mathit{Identifier}} = @m{\mathit{Expression}} during @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{setdynamic-exp (var exp1 body)}}}

@tt{setdynamic}表达式的效果是暂时把@${var}赋为@${exp_1}的值，求@${body}的值，重
新给@${var}赋其原值，然后返回@${body}的值。变量@${var}必须已绑定。例如，在下列表
达式中：

@nested[#:style 'code-inset]{
@verbatim|{
let x = 11
in let p = proc (y) -(y,x)
   in -(setdynamic x = 17 during (p 22),
        (p 13))
}|
}

@${x}是过程@tt{p}中的自由变量，在调用@tt{(p 22)}中值为17，在调用@tt{(p 13)}中重
设为11，所以表达式的值为@${5-2=3}。

}

@exercise[#:level 2 #:tag "ex4.22"]{

目前为止我们的语言都是以@emph{表达式为主} (@emph{expression-oriented})的：我们感
兴趣的主要是表达式这种句法类别，还有它们的值。扩展语言，为@emph{语句为主}
(@emph{statement-oriented})的简单语言建立模型，其规范概述如下。一定要@emph{遵循
语法}，分别写出过程来处理程序、语句和表达式。

@nested[#:style hangindent]{

@bold{值} 与IMPLICIT-REFS相同。

}

@nested[#:style hangindent]{

@bold{语法} 使用下列语法：

@nested{
@envalign*{
        \mathit{Program} &::= \mathit{Statement} \\[-3pt]
      \mathit{Statement} &::= @tt{@m{\mathit{Identifier}} = @m{\mathit{Expression}}} \\[-3pt]
                         &::= @tt{print @m{\mathit{Expression}}} \\[-3pt]
                         &::= \{\{\mathit{Statement}^{*(;)}\}\} \\[-3pt]
                         &::= @tt{if @m{\mathit{Expression}} @m{\mathit{Statement}} @m{\mathit{Statement}}} \\[-3pt]
                         &::= @tt{while @m{\mathit{Expression}} @m{\mathit{Statement}}} \\[-3pt]
                         &::= @tt{var @m{\{\mathit{Identifier}\}^{*(;)}} ; @m{\mathit{Statement}}} \\[-3pt]
                         }
}

非终止符@${\mathit{Expression}}指的是表达式语言IMPLICIT-REFS，可能有一些扩展。

}

@nested[#:style hangindent]{

@bold{语义} 程序是一个语句。语句不返回值，而是修改并打印存储器。

赋值语句工作如常。打印语句求值实参并打印结果。@tt{if}语句工作如常。块语句由
@${\mathit{Statement}}中的最后一个生成式定义，把每个绑定变量绑定到一个未初始化的
引用，然后执行块主体。这些绑定的作用域是其主体。

用如下断言写出语句的规范：

@$${@tt{(result-of @${stmt} @${\rho} @${\sigma_0})} = \sigma_0}

}

@nested[#:style hangindent]{

@bold{例子} 这里是一些例子。

@codeblock{
(run "var x,y; {x = 3; y = 4; print +(x,y)}") % 例1
7
(run "var x,y,z; {x = 3;                      % 例2
                  y = 4;
                  z = 0;
                  while not(zero?(x))
                    {z = +(z,y); x = -(x,1)};
                  print z}")
12
(run "var x; {x = 3;                          % 例3
              print x;
              var x; {x = 4; print x};
              print x}")
3
4
3
(run "var f,x; {f = proc(x,y) *(x,y);         % 例4
                x = 3;
                print (f 4 x)}")
12
}

例3解释了块语句的作用域。

例4解释了语句和表达式的交互。过程值创建并存储于变量@tt{f}。最后一行用实参4和
@tt{x}调用这个过程；因为@tt{x}绑定到一个引用，取其值得3。

}

}

@exercise[#:level 1 #:tag "ex4.23"]{

为@exercise-ref{ex4.22}中语言添加@tt{read}语句，形如@tt{read @${var}}。这种语句从输入读取一个
非负数，存入给定的变量中。

}

@exercise[#:level 1 #:tag "ex4.24"]{

@tt{do-while}语句类似@tt{while}，但是条件判断在其主体@emph{之后}执行。给@exercise-ref{ex4.22}
中的语言添加@tt{do-while}语句。

}

@exercise[#:level 1 #:tag "ex4.25"]{

扩展@exercise-ref{ex4.22}语言中的块语句，允许初始化变量。在你的解答中，变量的作用域是否包含
同一个块语句中后续声明的变量？

}

@exercise[#:level 3 #:tag "ex4.26"]{

扩展前一道练习中的解答，允许同一块语句中声明的过程互递归。考虑给语言增加限制，块
中的过程声明要在变量声明之后。

}

@exercise[#:level 3 #:tag "ex4.27"]{

扩展前一道练习中的解答，增加@emph{子程序} (@emph{subroutine})。我们把子程序当过
程用，但是它不返回值，且其主体为语句而非表达式。此外，增加新语句子程序调用，扩展
块语法，允许同时声明过程和子程序。这会如何影响指代值和表达值？如果在子程序调用中
使用过程会怎样？反过来呢？

}

@section[#:style section-title-style-numbered #:tag "s4.4"]{MUTABLE-PAIRS：可变序对语言}

在@exercise-ref{ex3.9}中，我们给语言添加了列表，但它们是不可变的：不像Scheme中，有
@tt{set-car!}和@tt{set-cdr!}处理它们。

现在，我们给IMPLICIT-REFS添加可变序对。序对是表达值，具有如下操作：

@envalign*{
@bold{@tt{make-pair}}  &: \mathit{Expval} \times \mathit{Expval} \to \mathit{MutPair} \\
@bold{@tt{left}} &: \mathit{MutPair} \to \mathit{Expval} \\
@bold{@tt{right}}   &: \mathit{MutPair} \to \mathit{Expval} \\
@bold{@tt{setleft}}  &: \mathit{MutPair} \times \mathit{Expval} \to \mathit{Unspecified} \\
@bold{@tt{setright}}  &: \mathit{MutPair} \times \mathit{Expval} \to \mathit{Unspecified}
}

序对包含两个位置，二者均可自行赋值。这给出 @elem[#:style question]{定义域方程}：

@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} + \mathit{MutPair} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)} \\
\mathit{MutPair} &= \mathit{Ref(ExpVal)} \times \mathit{Ref(ExpVal)}
}

我们把这种语言叫做MUTABLE-PAIRS。

@subsection[#:style section-title-style-numbered #:tag "s4.4.1"]{实现}

我们可以直接用前例中的@tt{reference}数据类型实现可变序对。代码如图4.9所示。

一旦完成之后，向语言添加这些就很直接了。我们给表达值数据类型新增一种变体
@tt{mutpair-val}，给@tt{value-of}新增5行代码。这些如图4.10所示。我们随便选取
@tt{setleft}的返回值为82，@tt{setright}的返回值为83。用前述辅助组件得到的示例跟
踪日志如图4.11所示。

@nested[#:style eopl-figure]{
@racketblock[
(define-datatype mutpair mutpair?
  (a-pair
    (left-loc reference?)
    (right-loc reference?)))

@#,elem{@bold{@tt{make-pair}} : @${\mathit{ExpVal} \times \mathit{ExpVal} \to \mathit{MutPair}}}
(define make-pair
  (lambda (val1 val2)
    (a-pair
      (newref val1)
      (newref val2))))

@#,elem{@bold{@tt{left}} : @${\mathit{MutPair} \to \mathit{ExpVal}}}
(define left
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
        (deref left-loc)))))

@#,elem{@bold{@tt{right}} : @${\mathit{MutPair} \to \mathit{ExpVal}}}
(define right
  (lambda (p)
    (cases mutpair p
      (a-pair (left-loc right-loc)
        (deref right-loc)))))

@#,elem{@bold{@tt{setleft}} : @${\mathit{MutPair} \times \mathit{ExpVal} \to \mathit{Unspecified}}}
(define setleft
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
        (setref! left-loc val)))))

@#,elem{@bold{@tt{setright}} : @${\mathit{MutPair} \times \mathit{ExpVal} \to \mathit{Unspecified}}}
(define setright
  (lambda (p val)
    (cases mutpair p
      (a-pair (left-loc right-loc)
        (setref! right-loc val)))))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "可变序对的拙劣实现"))]
}

@nested[#:style eopl-figure]{
@codeblock[#:indent 11]{
(newpair-exp (exp1 exp2)
  (let ((val1 (value-of exp1 env))
        (val2 (value-of exp2 env)))
    (mutpair-val (make-pair val1 val2))))

(left-exp (exp1)
  (let ((val1 (value-of exp1 env)))
    (let ((p1 (expval->mutpair val1)))
      (left p1))))

(right-exp (exp1)
  (let ((val1 (value-of exp1 env)))
    (let ((p1 (expval->mutpair val1)))
      (right p1))))

(setleft-exp (exp1 exp2)
  (let ((val1 (value-of exp1 env))
        (val2 (value-of exp2 env)))
    (let ((p (expval->mutpair val1)))
      (begin
        (setleft p val2)
        (num-val 82)))))

(setright-exp (exp1 exp2)
  (let ((val1 (value-of exp1 env))
        (val2 (value-of exp2 env)))
    (let ((p (expval->mutpair val1)))
      (begin
        (setright p val2)
        (num-val 83)))))
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "给解释器添加可变序对模块"))]
}

@subsection[#:style section-title-style-numbered #:tag "s4.4.2"]{可变序对的另一种表示}

把可变序对表示为两个引用没有利用@tt{MutPair}的已知信息。序对中的两个位置虽然能够
各自赋值，但它们不是独立分配的。我们知道它们会一起分配：如果序对的左侧是一个位置，
那么右侧是下一个位置。所以我们还可以用左侧的引用表示序对。代码如图4.13所示。其他
不需要修改。

@nested[#:style eopl-figure]{
@verbatim|{

> (run "let glo = pair(11,22)
in let f = proc (loc)
            let d1 = setright(loc, left(loc))
            in let d2 = setleft(glo, 99)
               in -(left(loc),right(loc))
   in (f glo)")
;; 为 init-env 分配单元
newref: 分配位置 0
newref: 分配位置 1
newref: 分配位置 2
进入 let glo
;; 为序对分配单元
newref: 分配位置 3
newref: 分配位置 4
;; 为 glo 分配单元
newref: 分配位置 5
进入 let glo 主体，环境 =
((glo 5) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 #(struct:num-val 11))
 (4 #(struct:num-val 22))
 (5 #(struct:mutpair-val #(struct:a-pair 3 4))))

进入 let f
;; 为 f 分配单元
newref: 分配位置 6
进入 let f 主体，环境 =
((f 6) (glo 5) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 #(struct:num-val 11))
 (4 #(struct:num-val 22))
 (5 #(struct:mutpair-val #(struct:a-pair 3 4)))
 (6 (procedure loc ... ((glo 5) (i 0) (v 1) (x 2)))))
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "MUTABLE-PAIRS求值的跟踪日志"))]
}

@nested[#:style eopl-figure]{
@verbatim|{

;; 为 loc 分配单元
newref: 分配位置 7
进入 proc loc 主体，环境 =
((loc 7) (glo 5) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 #(struct:num-val 11))
 (4 #(struct:num-val 22))
 (5 #(struct:mutpair-val #(struct:a-pair 3 4)))
 (6 (procedure loc ... ((glo 5) (i 0) (v 1) (x 2))))
 (7 #(struct:mutpair-val #(struct:a-pair 3 4))))

#(struct:num-val 88)
>
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "MUTABLE-PAIRS求值的跟踪日志，续"))]
}

同样地，堆中的任何聚合性对象都可以用其第一个位置的指针表示。但是，如果不提供区域
的长度信息，指针本身不能指代一片内存区域（见@exercise-ref{ex4.30}）。缺乏长度信息是经典安全问
题的一大来源，比如写数组越界。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@bold{@tt{mutpair?}} : @${\mathit{SchemeVal} \to \mathit{Bool}}}
(define mutpair?
  (lambda (v)
    (reference? v)))

@#,elem{@bold{@tt{make-pair}} : @${\mathit{ExpVal} \times \mathit{ExpVal} \to \mathit{MutPair}}}
(define make-pair
  (lambda (val1 val2)
    (let ((ref1 (newref val1)))
      (let ((ref2 (newref val2)))
        ref1))))

@#,elem{@bold{@tt{left}} : @${\mathit{MutPair} \to \mathit{ExpVal}}}
(define right
  (lambda (p)
    (deref (+ 1 p))))

@#,elem{@bold{@tt{right}} : @${\mathit{MutPair} \to \mathit{ExpVal}}}
(define left
  (lambda (p)
    (deref p)))

@#,elem{@bold{@tt{setleft}} : @${\mathit{MutPair} \times \mathit{ExpVal} \to \mathit{Unspecified}}}
(define setright
  (lambda (p val)
    (setref! (+ 1 p) val)))

@#,elem{@bold{@tt{setright}} : @${\mathit{MutPair} \times \mathit{ExpVal} \to \mathit{Unspecified}}}
(define setright
  (lambda (p val)
    (setref! (+ 1 p) val)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "可变序对的另一种表示"))]
}

@exercise[#:level 2 #:tag "ex4.28"]{

写出五个可变序对操作的规则定义。

}

@exercise[#:level 2 #:tag "ex4.29"]{

给语言添加数组。添加新操作符@tt{newarray}、@tt{arrayref}、@tt{arrayset}，来创建、
索值和更新数组。这引出：

@envalign*{
\mathit{ArrVal} &= \mathit{(Ref(ExpVal))} \\
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} + \mathit{ArrVal} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)}
}

由于数组中的位置是连续的，用上述第二种表示。下列程序的结果是什么？

@nested[#:style 'code-inset]{
@verbatim|{
let a = newarray(2,-99)
    p = proc (x)
         let v = arrayref(x,1)
         in arrayset(x,1,-(v,-1))
in begin arrayset(a,1,0); (p a); (p a); arrayref(a,1) end
}|
}

这里@tt{newarray(2,-99)}要创建长度为2的数组，数组中的每个位置包含@${-99}。
@tt{begin}表达式定义如@exercise-ref{ex4.4}。让数组索引从0开始，那么长度为2的数组索引为0和1。

}

@exercise[#:level 2 #:tag "ex4.30"]{

给@exercise-ref{ex4.29}的语言添加过程@tt{arraylength}，返回数组的长度。你的过程运行时间应为常
数。@tt{arrayref}和@tt{arrayset}一定要查验索引，确保其值在数组长度之内。

}

@section[#:style section-title-style-numbered #:tag "s4.5"]{传参变体}

当过程主体执行时，其形参绑定到一个指代值。那个值从哪儿来？它一定是过程调用传入的
实际参数值。我们已见过两种传参方式：

@itemlist[

 @item{自然传参，指代值与实际参数的表达值相同（@pageref{pass-by-value}）。}

 @item{按值调用，指代值是一个引用，指向一个位置，该位置包含实际参数的表达值
 （@secref{s4.3}）。这是最常用的传参方式。}

]

本节中，我们探讨其他一些传参机制。

@subsection[#:style section-title-style-numbered #:tag "s4.5.1"]{按址调用}

考虑下面的表达式：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
let p = proc (x) set x = 4
in let a = 3
   in begin (p a); a end
}|
}

按值调用时，绑定到@tt{x}的指代值是一个引用，它包含的初始值与绑定到@tt{a}的引用相
同，但这些引用互不相干。那么赋值给@tt{x}不会影响引用@tt{a}的内容，所以，整个表达
式的值是3。

}

按值调用时，当过程给某个参数赋新值，调用者无法获悉。当然，如果传给调用者的参数像
@secref{s4.4}那样包含可变序对，那么调用者能看到@tt{setleft}和@tt{setright}的效果
的效果，但看不到@tt{set}的效果。

虽然这样隔离调用者和受调者符合通常期望，但有时给过程传递一个位置，并要求过程给这
个位置赋值也很有用。要这样做，可以给过程传递一个引用，指向调用者变量的位置，而不
是变量的内容。这种传参机制叫做@emph{按址调用} (@emph{call-by-reference})。如果操
作数正是变量引用，那就传递变量的位置。然后，过程的形式参数绑定到这个位置。如果操
作数是其他类型的表达式，那么形式参数绑定到一个新位置，其值为操作数的值，就像按值
调用一样。在上例中使用按址调用，给@tt{x}赋值4等效于给@tt{a}赋值4，所以整个表达式
返回4，而不是3。

按址调用过程，且实际参数为变量时，传递的不是按值调用中变量所在位置的内容，而是那
个变量的@emph{位置}。例如，考虑：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
let p = proc (x) set x = 44
in let g = proc (y) (f y)
   in let z = 55
      in begin (g z); z end
}|
}

调用过程@tt{g}时，@tt{y}绑定到@tt{z}的位置，而不是那个位置处的内容。类似地，调用
@tt{f}时，@tt{x}绑定到同一个位置。所以，@tt{x}、@tt{y}和@tt{z}都绑定到同一位置，
@tt{set x = 44}的效果是把那个位置的内容设为44。因此，整个表达式的值是44。执行这
个表达式的跟踪日志如图4.14和图4.15所示。在本例中，@tt{x}、@tt{y}和@tt{z}最终都绑
定到位置5。

}

按址调用的常见用法是返回多个值。一个过程以通常方式返回一个值，并给按址传递的参数
赋其他值。另一种例子，考虑变量换值问题：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
let swap = proc (x) proc (y)
            let temp = x
            in begin
                set x = y;
                set y = temp
               end
in let a = 33
   in let b = 44
      in begin
          ((swap a) b);
          -(a,b)
         end
}|
}

使用按址调用，这会交换@tt{a}和@tt{b}的值，所以它返回11。但如果用我们已有的按值调
用解释器执行这个程序，它会返回@${-11}，因为在换值过程内部赋值对变量@tt{a}和
@tt{b}毫无影响。

}

按址调用中，变量仍然指代表达值的引用，就像按值调用一样：

@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)} + \mathit{ExpVal}
}

唯一需要改变的是新位置的分配。按值调用中，求值每个操作数都要分配新位置；按址调用
中，只在求值@emph{非变量}操作数时才分配新位置。

这很容易实现。函数@tt{apply-procedure}必须修改，因为不是每个过程调用都要分配新位
置。@tt{value-of}中的@tt{call-exp}能作出判断，因此分配的责任上移其中。

@racketblock[
@#,elem{@bold{@tt{apply-procedure}} : @${\mathit{Proc} \times \mathit{ExpVal} \to \mathit{ExpVal}}}
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body
          (extend-env var val saved-env))))))
]

然后我们修改@tt{value-of}中的@tt{call-exp}，引入新函数@tt{value-of-operand}来做
必要的判断。

@codeblock[#:indent 11]{
(call-exp
 (rator rand)
 (let ((proc (expval->proc (value-of rator env)))
       (arg (value-of-operand rand env)))
   (apply-procedure proc arg)))
}

过程@tt{value-of-operand}检查操作数是否为变量。如果是，则返回那个变量指代的引用，
然后传给过程@tt{apply-procedure}。否则，求值操作数，返回指向新位置的引用，位置中
包含该值。

@racketblock[
@#,elem{@bold{@tt{value-of-operand}} : @${\mathit{Exp} \times \mathit{Env} \to \mathit{Ref}}}
(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp (var) (apply-env env var))
      (else
        (newref (value-of exp env))))))
]

我们也可以按照同样地方式修改@tt{let}，但我们不这样做，所以语言中仍会保留按值调用
功能。

多个按址调用参数可以指向同一个位置，如下列程序所示。

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
let b = 3
in let p = proc (x) proc(y)
            begin
             set x = 4;
             y
            end
   in ((p b) b)
}|
}

它的值为4，因为@tt{x}和@tt{y}指向同一个位置，即@tt{b}的绑定。这种现象
叫做@emph{变量别名} (@emph{variable aliasing})。这里@tt{x}和@tt{y}是同一个位置的
别名（名字）。通常，我们不希望给一个变量赋值会改变另一个的值，所以别名会导致程序
难以理解。

}

@nested[#:style eopl-figure]{
@verbatim|{

> (run "
let f = proc (x) set x = 44
in let g = proc (y) (f y)
   in let z = 55
      in begin
          (g z);
          z
         end")
newref: 分配位置 0
newref: 分配位置 1
newref: 分配位置 2
进入 let f
newref: 分配位置 3
进入 let f 主体，环境 =
((f 3) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 (procedure x ... ((i 0) (v 1) (x 2)))))

进入 let g
newref: 分配位置 4
进入 let g 主体，环境 =
((g 4) (f 3) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
(1 #(struct:num-val 5))
(2 #(struct:num-val 10))
(3 (procedure x ... ((i 0) (v 1) (x 2))))
(4 (procedure y ... ((f 3) (i 0) (v 1) (x 2)))))

进入 let z
newref: 分配位置 5
进入 let z 主体，环境 =
((z 5) (g 4) (f 3) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 (procedure x ... ((i 0) (v 1) (x 2))))
 (4 (procedure y ... ((f 3) (i 0) (v 1) (x 2))))
 (5 #(struct:num-val 55)))
}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "CALL-BY-REFERENCE的简单求值"))]
}

@nested[#:style eopl-figure]{
@verbatim|{

进入 proc y 主体，环境 =
((y 5) (f 3) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 (procedure x ... ((i 0) (v 1) (x 2))))
 (4 (procedure y ... ((f 3) (i 0) (v 1) (x 2))))
 (5 #(struct:num-val 55)))

进入 proc x 主体，环境 =
((x 5) (i 0) (v 1) (x 2))
存储器 =
((0 #(struct:num-val 1))
 (1 #(struct:num-val 5))
 (2 #(struct:num-val 10))
 (3 (procedure x ... ((i 0) (v 1) (x 2))))
 (4 (procedure y ... ((f 3) (i 0) (v 1) (x 2))))
 (5 #(struct:num-val 55)))

#(struct:num-val 44)
>}|

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "CALL-BY-REFERENCE的简单求值，续"))]
}

@exercise[#:level 1 #:tag "ex4.31"]{

写出CALL-BY-REFERENCE的规则定义。

}

@exercise[#:level 1 #:tag "ex4.32"]{

扩展语言CALL-BY-REFERENCE，支持多参数过程。

}

@exercise[#:level 2 #:tag "ex4.33"]{

扩展语言CALL-BY-REFERENCE，同时支持按值调用过程。

}

@exercise[#:level 1 #:tag "ex4.34"]{

给语言添加按址调用的@tt{let}，名为@tt{letref}。写出规范并实现。

}

@exercise[#:level 2 #:tag "ex4.35"]{

在按值调用框架下，我们仍能享受按址调用的便利。扩展语言IMPLICIT-REF，添加新表达式：

@envalign*{
        \mathit{Expression} &::= @tt{ref @m{\mathit{Identifier}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{ref-exp (var)}}}

这与语言EXPLICIT-REF不同。因为引用只能从变量获得。这就使我们能用按值调用语言写出
类似@tt{swap}的程序。下列表达式的值是什么？

@nested[#:style 'code-inset]{
@verbatim|{
let a = 3
    b = 4
in let swap = proc (x) proc (y)
               let temp = deref(x)
               in begin
                   setref(x,deref(y));
                   setref(y,temp)
                  end
   in begin ((swap ref a) ref b); -(a,b) end
}|
}

这里我们用支持多声明的@tt{let}（@exercise-ref{ex3.16}）。这种语言的表达值和指代值是什么？

}

@exercise[#:level 1 #:tag "ex4.36"]{

大多数语言支持数组，在按址调用中，数组引用通常像变量引用那样处理。如果操作数是数
组引用，那就不给被调过程传递它的内容，而是传递引用指向的位置。比如，需要调用交换
过程的常见情形是对换数组元素，传递数组引用就能用上交换过程。给按址调用语言添加练
习4.29中的数组操作符，扩展@tt{value-of-operand}，处理这种情况。使下例中的过程调
用能够如愿工作：

@centered{@code{((swap (arrayref a i)) (arrayref a j))}}

要是下面这样呢？

@centered{@code{((swap (arrayref a (arrayref a i))) (arrayref a j))}}

}

@exercise[#:level 2 #:tag "ex4.37"]{

@emph{按值和结果调用} (@emph{call-by-value-result})是按址调用的一种变体。在按值和
结果调用中，实际参数必须是变量。传递参数时，形式参数绑定到新的引用，初值为实际参
数的值，就像按值调用一样。然后照常执行过程主体。但过程主体返回时，新引用处的值复
制到实际参数指代的引用中。因为这样可以改进内存非陪，这可能比按址调用更高效。实现
按值和结果调用，写出一个过程，采用按址调用与按值和结果调用产生不同的答案。

}

@subsection[#:style section-title-style-numbered #:tag "s4.5.2"]{懒求值：按名调用和按需调用}

迄今为止，我们讨论的所有参数传递机制都是@emph{即时} (@emph{eager})的：它们总是找
出每个操作数的值。现在我们来看另一种截然不同的传参机制，名叫@emph{懒求值}
(@emph{lazy evaluation})。在懒求值中，操作数的值直到过程主体需要时才会求取。如果
主体从未引用相关参数，就不需求值。

这可以避免永不终止。例如，考虑：

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
letrec infinite-loop (x) = infinite-loop(-(x,-1))
in let f = proc (z) 11
   in (f (infinite-loop 0))
}|
}

这里@tt{infinite-loop}是一个过程，调用时永不终止。@tt{f}是一个过程，调用时不引用
它的参数，总返回11。我们考虑过的任何一种传参机制都无法使这个程序终止。但在懒求值
中，这个程序将返回11，因为操作数@tt{(infinite-loop 0)}没有求值。

}

现在，我们修改我们的语言，使用懒求值。在懒求值中，如无必要，我们不求操作数表达式
的值。因此，我们将过程的绑定变量与未求值的操作数关联起来。当过程主体需要绑定变量
的值时，先求值相关操作数。操作数不经求值就传给过程，我们有时称之为@emph{冻结}
(@emph{frozen})，过程求值操作数则称为@emph{解冻} (@emph{thawed})。

当然，我们还要加入过程求值时的环境。要这样，我们引入一种新的数据类型，@emph{值箱}
(@emph{thunk})。@elem[#:style question]{值箱}包含一个表达式，一个环境。

@nested{
@racketblock[
(define-datatype thunk thunk?
  (a-thunk
   (exp1 expression?)
   (env environment?)))
]

但过程需要用绑定变量的值时，会求相应值箱的值。

}

我们面对的情况稍稍复杂一些，因为我们需要同时容纳懒求值、计算效果和即时求值
（@tt{let}要用）。因此，我们把指代值定为内存位置的引用，位置包含表达值或者值箱。

@envalign*{
\mathit{DenVal} &= \mathit{Ref(ExpVal + Thunk)} \\
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc}
}

我们的位置分配策略与按址调用类似：如果操作数是变量，那么我们传递指代的引用。否则，
我们给未求值的参数在新位置放一个值箱，传递该位置的引用。

@racketblock[
@#,elem{@bold{@tt{value-of-operand}} : @${\mathit{Exp} \times \mathit{Env} \to \mathit{Ref}}}
(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp (var) (apply-env env var))
      (else
        (newref (a-thunk exp env))))))
]

求值@tt{var-exp}时，我们首先找到变量绑定的位置。如果该位置是一个表达值，那么返回
这个值，作为@tt{var-exp}的值。如果它包含一个值箱，那么我们求取并返回值箱的值。这
叫@emph{按名调用} (@emph{call by name})。

@codeblock[#:indent 11]{
(var-exp (var)
  (let ((ref1 (apply-env env var)))
    (let ((w (deref ref1)))
      (if (expval? val)
        val
        (value-of-thunk val)))))
}

过程@tt{value-of-thunk}定义如下：

@racketblock[
@#,elem{@bold{@tt{value-of-thunk}} : @${\mathit{Thunk} \to \mathit{ExpVal}}}
(define value-of-thunk
  (lambda (th)
    (cases thunk th
      (a-thunk (exp1 saved-env)
        (value-of exp1 saved-env)))))
]

或者，一旦发现值箱的值，我们可以把表达值放到同一个位置，这样就不需要再次求值箱的
值。这种方式叫做@emph{按需调用} (@emph{call by need})。

@nested{
@codeblock[#:indent 11]{
(var-exp (var)
  (let ((ref1 (apply-env env var)))
    (let ((w (deref ref1)))
      (if (expval? w)
        w
        (let ((val1 (value-of-thunk w)))
          (begin
            (setref! ref1 val1)
            val1))))))
}

这里用到了一种名为@emph{助记法} (@emph{memoization})的通用策略。

}

各种形式的懒求值引人之处在于，即使没有计算效果，通过它也能以相当简单的方式思考程
序。把过程调用替换为过程的主体，把每个形参的引用替换为对应的操作数，就能建模过程
调用。这种求值策略是lambda演算的基础，名为 @emph{@${\beta}-推导}
(@emph{@${\beta}-reduction})。

不幸的是，按名调用和按需调用使求值顺序难以确定，而这对理解有效果的程序极为关键。
但是没有效果时，这不成问题。所以懒求值盛行于函数式编程语言（没有计算效果），在别
处却难觅踪影。

@exercise[#:level 1 #:tag "ex4.38"]{

下面的例子展示了@exercise-ref{ex3.25}在按需调用中的变体。@exercise-ref{ex3.25}中的原始程序在按需调用中可行
吗？如果下面的程序在按值调用中运行呢？为什么？

@nested[#:style 'code-inset]{
@verbatim|{
let makerec = proc (f)
               let d = proc (x) (f (x x))
               in (f (d d))
in let maketimes4 = proc (f)
                     proc (x)
                      if zero?(x)
                      then 0
                      else -((f -(x,1)), -4)
   in let times4 = (makerec maketimes4)
      in (times4 3)
}|
}

}

@exercise[#:level 1 #:tag "ex4.39"]{

没有计算效果的话，按名调用和按需调用总是给出同样的答案。设计一个例子，让按名调用
和按需调用给出不同的答案。

}

@exercise[#:level 1 #:tag "ex4.40"]{

修改@tt{value-of-operand}，避免为常量和过程生成值箱。

}

@exercise[#:level 2 #:tag "ex4.41"]{

写出按名调用和按需调用的规则定义。

}

@exercise[#:level 2 #:tag "ex4.42"]{

给按需调用解释器添加懒求值@tt{let}。

}
