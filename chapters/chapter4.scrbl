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

@eopl-index["Effects, computational"]
@eopl-index[#:range-mark 'start "References"]
到目前为止，我们只考虑了计算产生的@term["value"]{值}，但是计算也
有@term["effect"]{效果}：它可以读取，打印，修改内存或者文件系统的状态。在现实世
界中，我们@emph{总是}对效果很感兴趣：如果一次计算不显示答案，那对我们完全没用！

产生值和产生效果有何区别？效果是@term["global"]{全局性} 的，整个计算都能看到。效
果@emph{感染}整个计算（故意用双关语）。

@eopl-index["Binding" (eopl-index-entry "of variables" "variables")]
@eopl-index["Shared variables"]
我们主要关心一种效果：给内存中的位置赋值。赋值与绑定有何区别？我们已经知道，绑定
是局部的，但@eopl-index["Assignment"]变量赋值有可能是全局的。那是在本不相关的几
部分计算之间@term["share"]{共享} 值。如果两个过程知道内存中的同一位置，它们就能
共享信息。如果把信息留在已知位置，同一个过程就能在当前调用和后续调用之间共享信息。

@eopl-index["Location"]
@eopl-index["Storable values"]
我们把内存建模为从@term["location"]{位置} 到值集合的的有限映射，称值集合
为@term["storable values"]{可存储值}。出于历史原因，我们称之为@term["store"]{存
储器}。通常，一种语言中的可存储值与表达值相同，但不总是这样。这个选择是语言设计
的一部分。@eopl-index["Store"]

代表内存位置的数据结构叫做@term["reference"]{引用}。位置是内存中可用来存值的地方，
引用是指向那个地方的数据结构。位置和引用的区别可以这样类比：位置就像文件，引用就
像一个URL。URL指向一个文件，文件包含一些数据。类似地，引用指代一个位置，位置包含
一些数据。

@eopl-index["L-values"]
引用有时候又叫@term["L-values"]{左值}。这名字反映了这种数据结构与赋值语句左边变
量的联系。类似地，表达值，比如赋值语句右边表达式的值，叫做@term["R-values"]{右值}。
@eopl-index["R-values"]

我们考虑两种带有存储器的语言设计。这些设计叫做@term["explicit reference"]{显式引
用} 和@term["implicit reference"]{隐式引用}。
@eopl-index[#:range-mark 'end "References"]

@section[#:style section-title-style-numbered #:tag "s4.2"]{EXPLICIT-REFS：显式引用语言}

@eopl-index[#:range-mark 'start "EXPLICIT-REFS"]
@eopl-index[#:range-mark 'start "References" "explicit"]
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

 @item{@tt{newref}，分配新的位置，返回其引用。
 @eopl-index["Allocation" (eopl-index-entry "in store" "store")]}

 @item{@tt{deref}，@term["deference"]{解引用} ：返回引用指向位置处的内容。
 @eopl-index["Dereferencing"]}

 @item{@tt{setref}，改变引用指向位置处的内容。
 @eopl-index[#:range-mark 'start "Mutation"]}

]

我们把得到的语言称作 EXPLICIT-REFS。让我们用这些结构写几个程序。

下面是两个过程 @tt{even} 和 @tt{odd}。它们取一参数，但是忽略它，并根据位置
@tt{x} 处的内容是偶数还是奇数返回 1 或 0。它们不是通过直接传递数据来通信，而是改
变共享变量的内容。@eopl-index["Shared variables"]

这个程序判断 13 是否为奇数，并返回 1。过程 @tt{even} 和 @tt{odd} 不引用它们的实
参，而是查看绑定到 @tt{x} 的位置中的内容。

@nested{

@eopl-code{
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

这个程序使用多声明的 @tt{letrec}（@exercise-ref{ex3.32}）和 @tt{begin} 表达式
（@exercise-ref{ex4.4}）@eopl-index[(eopl-index-entry @elem{@tt{begin}
expression} "beginexpression")]。@tt{begin} 表达式按顺序求每个子表达式的值，并返
回最后一个表达式的值。

}

为了同我们的单参数语言保持一致，我们给 @tt{even} 和 @tt{odd} 传一个无用参数；如
果我们的过程支持任意数量的参数（@exercise-ref{ex3.21}），这些过程的参数就可以去
掉。

当两个过程需要分享很多量时，这种通信方式很方便；只需给某些随调用而改变的量赋值。
同样地，一个过程可能通过一长串调用间接调用另一过程。二者可以通过一个共享变量直接
交换数据，居间的过程不需要知道它。因此，以共享变量通信可作为一种隐藏信息的方式。

@eopl-index["Private variables"]
赋值的另一用途是通过私有变量创建隐藏状态。例如：

@eopl-code{
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
调用 @tt{g} 返回 1，第二次返回 2，整个程序的值为 -1。

@eopl-index[#:range-mark 'start "Shared variables"]
下图是 @tt{g} 绑定时所在的环境。可以认为，这是在 @tt{g} 的不同调用之间共享信息。
Scheme 过程 @tt{gensym} 用这种技术创建唯一符号。

@eopl-figure*[#:position "!ht"]{
@centered{
@(image "../images/g-bound"
  #:scale 0.95
  #:suffixes (list ".pdf" ".svg")
  "g绑定时的环境")
}
@eopl-index[#:range-mark 'end "Shared variables"]}

@exercise[#:level 1 #:tag "ex4.1"]{

这个程序如果写成下面这样会怎样？

@eopl-code{
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
个位置存储引用。考虑下面的程序：@eopl-index["Storable values"]

@eopl-code{
@verbatim|{
let x = newref(newref(0))
in begin
    setref(deref(x), 11);
    deref(deref(x))
end
}|
}

这段程序分配了一个新位置，内容为 0。然后，它将 @tt{x} 绑定到一个位置，其内容为指
向第一个位置的引用。因此，@tt{deref(x)} 的值是第一个位置的引用。那么程序求
@tt{setref} 的值时，会修改第一个位置，整个程序返回 11。

@subsection[#:style section-title-style-numbered #:tag "s4.2.1"]{存储器传递规范}

在我们的语言中，任何表达式都可以有效果。要定义这些效果，我们需要描述每次求值使用
什么样的存储器，以及求值如何修改存储器。

在规范中，我们用 @${\sigma} 表示任一存储器，用 @${\text{[}l=v\text{]}\sigma} 表
示另一存储器，除了将位置 @${l} 映射到 @${v}外，它与 @${\sigma} 相同。有时，涉及
@${\sigma} 的某个具体值时，我们称之为存储器的@term["state"]{状态}。

我们使用@term["store-passing specifications"]{存储器传递规范}。在存储器传递规范
中，存储器作为显式参数传递给 @tt{value-of}，并作为 @tt{value-of} 的结果返回。那
么我们可以写：@eopl-index["Store-passing specifications"]

@nested{

@$${@tt{(value-of @${exp_1} @${\rho} @${\sigma_0})} = @tt{(@${val_1},@${\sigma_1})}}

它断言在环境为 @${\rho}，存储器状态为 @${\sigma_0} 时，表达式 @${exp_1} 的返回值
为 @${val_1}，并且可能把存储器修改为另一状态 @${\sigma_1}。

}

这样我们就能写出 @tt{const-exp} 之类的无效果操作：

@nested{

@$${@tt{(value-of (const-exp @${n}) @${\rho} @${\sigma})} = @tt{(@${n},@${\sigma})}}

以此表明求表达式的值不会修改存储器。

}

@tt{diff-exp} 的规范展示了如何定义有顺序的行为。

@nested{

@$${\infer{@tt{(value-of (diff-exp @${exp_1} @${exp_2}) @${\rho} @${\sigma_0})} =
           @tt{(@${\lceil\lfloor val_1 \rfloor - \lfloor val_2 \rfloor\rceil},@${\sigma_2})}}
          {\begin{alignedat}{-1}
             @tt{(value-of (diff-exp @${exp_1}) @${\rho} @${\sigma_0})} &= @tt{(@${val_1},@${\sigma_1})} \\
             @tt{(value-of (diff-exp @${exp_2}) @${\rho} @${\sigma_1})} &= @tt{(@${val_2},@${\sigma_2})}
           \end{alignedat}}}

这里，我们从状态为 @${\sigma_0} 的存储器开始，首先求 @${exp_1} 的值。@${exp_1}
返回值为 @${val_1}，但它可能有效果，把存储器状态修改为 @${\sigma_1}。然后我们从
@${exp_1} 修改过的存储器——也就是 @${\sigma_1}——开始，求 @${exp_2} 的值。
@${exp_2} 同样返回一个值 @${val_2}，并把存储器状态修改为 @${\sigma_2}。之后，整
个表达式返回 @${val_1 - val2}，对存储器不再有任何效果，所以存储器状态留在
@${\sigma_2}。

}

再来试试条件表达式。

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

一个 @tt{if-exp} 从状态 @${\sigma_0} 开始，求条件表达式 @${exp_1} 的值，返回值
@${val_1}，将存储器状态修改为 @${\sigma_1}。整个表达式的结果可能是 @${exp_2} 或
@${exp_3} 的结果，二者都在当前环境 @${\rho} 和 @${exp_1} 留下的存储器状态
@${\sigma_1} 中求值。

@exercise[#:level 1 #:tag "ex4.2"]{

写出 @tt{zero?-exp} 的规范。

}

@exercise[#:level 1 #:tag "ex4.3"]{

写出 @tt{call-exp} 的规范。

}

@exercise[#:level 2 #:tag "ex4.4"]{

@eopl-index[#:suffix @exer-ref-range["ex4.4"] (eopl-index-entry @elem{@tt{begin} expression} "beginexpression")]
写出 @tt{begin} 表达式的规范。

@nested{
@$${\mathit{Expression} ::= @tt{begin @${\mathit{Expression}} @${\{}@tt{; }@${\mathit{Expression}}@${\}^{*}} end}}
}

@tt{begin} 表达式包含一个或多个分号分隔的子表达式，按顺序求这些子表达的值，并返
回最后一个的结果。

}

@exercise[#:level 2 #:tag "ex4.5"]{

@eopl-index[#:suffix @exer-ref-range["ex4.5"] @eopl-index-entry[@elem{@tt{list} expression} "listexpression"]]
写出 @tt{list}（@exercise-ref{ex3.10}）的规范。

}

@subsection[#:style section-title-style-numbered #:tag "s4.2.2"]{定义显式引用操作}

@eopl-index[#:range-mark 'start "Allocation" (eopl-index-entry "in store" "store")]
在 EXPLICIT-REFS 中，我们必须定义三个操作：@tt{newref}、@tt{deref} 和
@tt{setref}。它们的语法为：

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
      {@tt{(value-of @${exp} @${\rho} @${\sigma_0})} = @tt{(@${val},@${\sigma_1})} \quad l \notin \text{dom}(\sigma_1)}
}

这条规则是说：@tt{newref-exp} 求出操作数的值，得到一个存储器，然后分配一个新位置
@${l}，将参数值 @${val} 放到这一位置，以此来扩展那个存储器。然后它返回新位置
@${l} 的引用。这意味着 @${l} 不在 @${\sigma_1} 的定义域内。
@eopl-index["Dereferencing"]

@$${
\infer{@tt{(value-of (deref-exp @${exp}) @${\rho} @${\sigma_0}) =
           (@${\sigma_1(l)},@${\sigma_1})}}
      {@tt{(value-of @${exp} @${\rho} @${\sigma_0})} = @tt{(@${val},@${\sigma_1})}}
}

这条规则是说：@tt{deref-exp} 求出操作数的值，然后把存储器状态改为 @${\sigma_1}。
参数的值应是位置 @${l} 的引用。然后 @tt{deref-exp} 返回 @${\sigma_1} 中 @${l} 处
的内容，不再更改存储器。

@$${
\infer{@tt{(value-of (setref-exp @${exp_1} @${exp_2}) @${\rho} @${\sigma_0}) =
           (@${\lceil 23 \rceil},[@${l}=@${val}]@${\sigma_2})}}
      {\begin{gathered}
        @tt{(value-of @${exp_1} @${\rho} @${\sigma_0})} = @tt{(@${l},@${\sigma_1})} \\
        @tt{(value-of @${exp_2} @${\rho} @${\sigma_1})} = @tt{(@${val},@${\sigma_2})}
       \end{gathered}}
}

@eopl-index["Mutation"]
这条规则是说：@tt{setref-exp} 从左到右求操作数的值。第一个操作数的值必须是某个位
置 @${l} 的引用；然后 @tt{setref-exp} 把第二个参数的值 @${val} 放到位置 @${l} 处，
以此更新存储器。@tt{setref-exp} 应该返回什么呢？它可以返回任何值。为了强调这一选
择的随意性，我们让它返回 23。因为我们对 @tt{setref-exp} 的返回值不感兴趣，我们说
这个表达式的执行@term["for effect"]{求效果} 而不求值。
@eopl-index["Effects, computational"]
@eopl-index["Execution for effect"]

@exercise[#:level 1 #:tag "ex4.6"]{

修改上面的规则，让 @tt{setref-exp} 返回右边表达式的值。

}

@exercise[#:level 1 #:tag "ex4.7"]{

修改上面的规则，让 @tt{setref-exp} 返回位置的原内容。
@eopl-index[#:range-mark 'end "Allocation" (eopl-index-entry "in store" "store")]

}

@subsection[#:style section-title-style-numbered #:tag "s4.2.3"]{实现}

@eopl-index[#:range-mark 'start "Store"]
迄今为止，我们使用的规范语言可以轻松描述有效果计算的期望行为，但是它没有体现存储
器的一个要点：引用最终指向现实世界的内存中某一真实的位置。因为我们只有一个现实世
界，我们的程序只能记录存储器的一个状态 @${\sigma}。

在我们的实现中，我们利用这一事实，用 Scheme 中的存储器建模存储器。这样，我们就能
用 Scheme 中的效果建模效果。

我们用一个 Scheme 值表示存储器状态，但是我们不像规范建议的那样直接传递和返回它，
相反，我们在一个全局变量中记录状态，实现代码中的所有过程都能访问它。这很像示例程
序 @tt{even/odd} 使用共享位置，而不是直接传递参数。使用单一全局变量时，我们也几
乎不需要理解 Scheme 中的效果。

我们还是要选择如何用 Scheme 值建模存储器。我们选择的可能是最简单的模型：以表达值
列表作为存储器，以代表列表位置的数字表示引用。分配新引用就是给列表末尾添加新值；
更新存储器则建模为按需复制列表的一大部分。代码如@figure-ref{fig-4.1} 和
@countref{fig-4.2} 所示。

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,elem{@bold{@tt{empty-store}} : @${() \to \mathit{Sto}}}
(define empty-store
  (lambda () '()))

@#,emph{@bold{用法} : Scheme 变量，包含存储器当前的状态。初始值无意义。}
(define the-store 'uninitialized)

@#,elem{@bold{@tt{get-store}} : @${() \to \mathit{Sto}}}
(define get-store
  (lambda () the-store))

@#,elem{@bold{@tt{initialize-store!}} : @${() \to \mathit{Unspecified}}}
@#,emph{@bold{用法} : @tt{(initialize-store!)} 将存储器设为空。}
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

@eopl-caption["fig-4.1"]{拙劣的存储器模型}
}

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,elem{@bold{@tt{setref!}} : @${\mathit{Ref} \times \mathit{ExpVal} \to \mathit{Unspecified}}}
@#,emph{@bold{用法} : 除了把位置 @tt{ref} 的值设为 @tt{val}，@tt{the-store} 与原状态相同。}
(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec
        ((setref-inner
           @#,emph{@bold{用法} : 返回一列表，除了位置 ref1 处}
           @#,emph{值为 val，与 store1 相同。}
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

@eopl-caption["fig-4.2"]{拙劣的存储器模型，续}
}

这种表示极其低效。一般的内存操作大致在常数时间内完成，但是采用我们的表示，这些操
作所需的时间与存储器大小成正比。当然，真正实现起来不会这么做，但这足以达到我们的
目的。

我们给表达值数据类型新增一种变体 @tt{ref-val}，然后修改 @tt{value-of-program}，
在每次求值之前初始化存储器。

@nested{
@eopl-code{
@racketblock[
@#,elem{@bold{@tt{value-of-program}} : @${\mathit{Program} \to \mathit{SchemeVal}}}
(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (exp1)
        (value-of exp1 (init-env))))))
]
}

现在，我们可以写出 @tt{value-of} 中与 @tt{newref}、@tt{deref} 和 @tt{setref} 相
关的语句。这些语句如@figure-ref{fig-4.3} 所示。
}

我们可以给该系统添加一些@elemtag["trace-instrument"]{辅助过程}，把环境、过程和存
储器转换为更易读的形式，也可以改善系统，在代码中的关键位置打印消息。我们还使用过
程把环境、过程和存储器转换为更易读的形式。得出的日志详细描述了系统的动作。典型例
子如@figure-ref{fig-4.4} 和 @countref{fig-4.5} 所示。此外，这一跟踪日志还表明，
差值表达式的参数按从左到右的顺序求值。
@eopl-index[#:range-mark 'end "EXPLICIT-REFS"]
@eopl-index[#:range-mark 'end "References" "explicit"]
@eopl-index[#:range-mark 'end "Store"]

@exercise[#:level 1 #:tag "ex4.8"]{

指出我们实现的存储器中，到底是哪些操作花费了线性时间而非常数时间。

}

@exercise[#:level 1 #:tag "ex4.9"]{

用 Scheme 向量表示存储器，从而实现常数时间操作。用这种表示会失去什么？

}

@exercise[#:level 1 #:tag "ex4.10"]{

@eopl-index[#:suffix @exer-ref-range["ex4.10"] (eopl-index-entry @elem{@tt{begin} expression} "beginexpression")]
实现@exercise-ref{ex4.4} 中定义的 @tt{begin} 表达式。

}

@exercise[#:level 1 #:tag "ex4.11"]{

@eopl-index[#:suffix @exer-ref-range["ex4.11"] @eopl-index-entry[@elem{@tt{list} expression} "listexpression"]]
实现@exercise-ref{ex4.5} 中的 @tt{list}。

}

@exercise[#:level 3 #:tag "ex4.12"]{

像解释器中展示的，我们对存储器的理解基于 Scheme 效果的含义。具体地说，我们得知道
在 Scheme 程序中这些效果@emph{何时}产生。我们可以写出更贴合规范的解释器，从而避
免这种依赖。在这一解释器中，@tt{value-of} 同时返回值和存储器，就像规范中那样。这
一解释器的片段如@figure-ref{fig-4.6} 所示。我们称之为@term["store-passing
interpreter"]{传递存储器的解释器}。补全这个解释器，处理整个 EXPLICIT-REFS 语言。

过程可能修改存储器时，不仅返回通常的值，还要返回一个新存储器。它们包含在名为
@tt{answer} 的数据类型之中。完成这个 @tt{value-of} 的定义。

}

@exercise[#:level 3 #:tag "ex4.13"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.13"] "Multiple-argument procedures"]
扩展前一道练习中的解释器，支持多参数过程。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.13"] "Multiple-argument procedures"]

}

@eopl-figure[#:position "!ht"]{
@codeblock[#:indent racket-block-offset]{
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

@eopl-caption["fig-4.3"]{@tt{value-of} 的显式引用操作语句
                         @eopl-index["Dereferencing"]}
}

@eopl-figure{
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

@eopl-caption["fig-4.4"]{EXPLICIT-REFS的求值跟踪日志}
}

@eopl-figure{
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

@eopl-caption["fig-4.5"]{EXPLICIT-REFS的求值跟踪日志，续}
}

@eopl-figure[#:position "!ht"]{
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

@eopl-caption["fig-4.6"]{@exercise-ref{ex4.12}，传递存储器的解释器}
}

@section[#:style section-title-style-numbered #:tag "s4.3"]{IMPLICIT-REFS：隐式引用语言}

@eopl-index[#:range-mark 'start "IMPLICIT-REFS"]
@eopl-index[#:range-mark 'start "Mutation"]
@eopl-index[#:range-mark 'start "References" "implicit"]
显式引用设计清晰描述了内存的分配、解引用和变更，因为显而易见，这些操作都在程序员
的代码之中。

大多数编程语言都用共同的方式处理分配、解引用和变更，并把它们打包为语言的一部分。
这样，由于这些操作存在于语言内部，程序员不需要担心何时执行它们。
@eopl-index[#:range-mark 'end "Mutation"]

在这种设计中，每个变量都表示一个引用。指代值是包含表达值的位置的引用。引用不再是
表达值，只能作为变量绑定。@eopl-index["Allocation" (eopl-index-entry "in store" "store")]

@nested{
@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)}
}

每次绑定操作都会分配一个位置：在每个过程调用处，在 @tt{let} 和 @tt{letrec} 中。

}

当变量出现在表达式中，我们首先在环境中查找标识符，找到绑定的位置，然后在存储器中
找出那个位置的值。因此对 @tt{var-exp}，我们有个@exact-elem{“}二级@exact-elem{”}系统。

一个位置的内容可用 @tt{set} 表达式修改，语法为：

@envalign*{
        \mathit{Expression} &::= @tt{set @m{\mathit{Identifier}} = @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{assign-exp (var exp1)}}}

这里的 @${\mathit{Identifier}} 不是表达式的一部分，所以无法解引用。在这种设计中，
我们说变量是@term["mutable"]{可变的}，意为可以修改。

@eopl-index[(eopl-index-entry "Call-by-value" "Callbyvalue")]
这种设计叫做@term["call-by-value"]{按值调用}，或@term["implicit reference"]{隐式
引用}。大多数编程语言，包括 Scheme，都采纳这一设计的某种变体。

@figure-ref{fig-4.7} 是这种设计的两个示例程序。因为引用不再是表达值，我们不能
像@secref{s4.2}中的例子那样做链式引用。

@eopl-figure[#:position "!ht"]{
@eopl-code{
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

@eopl-caption["fig-4.7"]{IMPLICIT-REFS中的 @tt{odd} 和 @tt{even}}
}

@subsection[#:style section-title-style-numbered #:tag "s4.3.1"]{规范}

我们可以轻松写出解引用和 @tt{set} 的规则。现在，环境总是把变量绑定到位置，所以当
变量作为表达式时，我们需要将其解引用：

@$${@tt{(value-of (var-exp @${var}) @${\rho} @${\sigma})} = @tt{(@${\sigma(\rho(var))},@${\sigma})}}

赋值就像我们预想的那样：我们在环境中查找式子左侧的标识符，获取一个位置，在环境中
求右边表达式的值，修改指定位置的内容。就像 @tt{setref}，@tt{set} 表达式的返回值
任意。我们让它返回表达值 27。

@$${
\infer{@tt{(value-of (assign-exp @${var} @${exp_1}) @${\rho} @${\sigma_0}) =
           (@${\lceil 27 \rceil},[@${\rho(var)}=@${val_1}]@${\sigma_1})}}
      {@tt{(value-of @${exp_1} @${\rho} @${\sigma_0})} = @tt{(@${val_1},@${\sigma_1})}}
}

@eopl-index[#:range-mark 'start "Parameter passing"]
我们还要重写过程调用和 @tt{let} 规则，体现出对存储器的修改。对过程调用，规则变成：
@eopl-index["Binding" (eopl-index-entry @tt{proc} "proc")]
@eopl-index["Body" (eopl-index-entry @tt{proc} "proc")]

@nested{
@eopl-equation{
@verbatim|{
(apply-procedure (procedure |@${var} |@${body} |@${\rho}) |@${val} |@${\sigma})
= (value-of |@${body} [|@${var=l}]|@${\rho} [|@${l=val}]|@${\sigma})
}|
}

其中，@${l} 是不在 @${\sigma} 定义域中的某一位置。
@eopl-index[#:range-mark 'end "Parameter passing"]
}

@eopl-index["Binding" (eopl-index-entry @tt{let} "let")]
@eopl-index["Body" (eopl-index-entry @tt{let} "let")]
@tt{(let-exp @${var} @${exp_1} @${body})} 的规则类似。我们首先求右边 @${exp_1}
的值，然后将该值放入一个新位置，将变量 @${var} 绑定到这个位置，在得到的环境中求
@tt{let} 主体的值，作为整个表达式的值。

@exercise[#:level 1 #:tag "ex4.14"]{

写出 @tt{let} 的规则。

}

@subsection[#:style section-title-style-numbered #:tag "s4.3.2"]{实现}

现在我们着手修改解释器。在 @tt{value-of} 中，我们取出每个 @tt{var-exp} 的值，就像
规则描述的那样：

@nested{

@eopl-code{
@codeblock[#:indent racket-block-offset]{(var-exp (var) (deref (apply-env env var)))}
}

@tt{assign-exp} 的代码也显而易见：

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(assign-exp (var exp1)
  (begin
    (setref!
      (apply-env env var)
      (value-of exp1 env))
    (num-val 27)))
}
}
}

创建引用呢？新的位置应在每一新绑定处创建。这门语言中只有四个地方创建新绑定：初始
环境中、@tt{let} 中、过程调用以及 @tt{letrec} 中。

在初始环境中，我们直接分配新位置。

@eopl-index["Binding" (eopl-index-entry @tt{let} "let")]
@eopl-index["Body" (eopl-index-entry @tt{let} "let")]
对 @tt{let}，我们修改 @tt{value-of} 中相应的行，分配包含值的新位置，并把变量绑定
到指向该位置的引用。

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(let-exp (var exp1 body)
  (let ((val1 (value-of exp1 env)))
    (value-of body
      (extend-env var (newref val1) env))))
}
}

@eopl-index[#:range-mark 'start "Binding" (eopl-index-entry @tt{proc} "proc")]
@eopl-index[#:range-mark 'start "Body" (eopl-index-entry @tt{proc} "proc")]
@eopl-index[#:range-mark 'start "Parameter passing"]
对过程调用，我们同样修改 @tt{apply-procedure}，调用 @tt{newref}。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{apply-procedure}} : @${\mathit{Proc} \times \mathit{ExpVal} \to \mathit{ExpVal}}}
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body
          (extend-env var (newref val) saved-env))))))
]
@eopl-index[#:range-mark 'end "Binding" (eopl-index-entry @tt{proc} "proc")]
@eopl-index[#:range-mark 'end "Body" (eopl-index-entry @tt{proc} "proc")]
@eopl-index[#:range-mark 'end "Parameter passing"]
}

@eopl-index["Binding" (eopl-index-entry @tt{letrec} "letrec")]
@eopl-index["Body" (eopl-index-entry @tt{letrec} "letrec")]
最后，要处理 @tt{letrec}，我们替换 @tt{apply-env} 中的 @tt{extend-env-rec} 从句，
令其返回一个引用，指向包含适当闭包的位置。由于我们使用多声明的
@tt{letrec}（@exercise-ref{ex3.32}），@tt{extend-env-rec} 取一个过程名列表，一个
绑定变量列表，一个过程主体列表，以及已保存的环境。过程 @tt{location} 取一变量，
一个变量列表。若变量存在于列表中，@tt{location} 返回变量在列表中的位置；若不存在，
返回 @tt{#f}。

@eopl-code{
@codeblock[#:indent racket-block-offset]{
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
}

@figure-ref{fig-4.8} 用@elemref["trace-instrument"]{前面}介绍的辅助组件，展示了
IMPLICIT-REFS 求值的简单例子。
@eopl-index[#:range-mark 'end "IMPLICIT-REFS"]
@eopl-index[#:range-mark 'end "References" "implicit"]

@eopl-figure{
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

@eopl-caption["fig-4.8"]{IMPLICIT-REFS的简单求值}
}

@exercise[#:level 1 #:tag "ex4.15"]{

在@figure-ref{fig-4.8} 中，环境中的变量为什么绑定到一般的整数，而不
是@figure-ref{fig-4.5} 中那样的表达值？

}

@exercise[#:level 1 #:tag "ex4.16"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.16"] "Recursive programs" "design and implementation of"]
既然变量是可变的，我们可以靠赋值产生递归过程。例如：

@eopl-code{
@verbatim|{
letrec times4(x) = if zero?(x)
                   then 0
                   else -((times4 -(x,1)), -4)
in (times4 3)
}|
}

可以替换为：

@eopl-code{
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

手动跟踪这个程序，验证这种翻译可行。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.16"] "Recursive programs" "design and implementation of"]

}

@exercise[#:level 2 #:tag "ex4.17"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.17"] "Multiple-argument procedures"]
写出规则并实现多参数过程和声明多变量的 @tt{let}。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.17"] "Multiple-argument procedures"]

}

@exercise[#:level 2 #:tag "ex4.18"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.18" "ex4.19"] "Multiple-procedure declaration"]
写出规则并实现声明多过程的 @tt{letrec} 表达式。

}

@exercise[#:level 2 #:tag "ex4.19"]{

@eopl-index[#:suffix @exer-ref-range["ex4.19"] "Closures"]
修改声明多过程的 @tt{letrec} 实现，让每个闭包只生成一次，并且只分配一个位置。本
题类似@exercise-ref{ex3.35}。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.18" "ex4.19"] "Multiple-procedure declaration"]

}

@exercise[#:level 2 #:tag "ex4.20"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.20"] @eopl-index-entry[@elem{@tt{letmutable} expression} "letccexpression"]]
在本节的语言中，就像在 Scheme 中一样，所有变量都是可变的。另一种设计是同时允许可
变和不可变的变量绑定：
@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)} + \mathit{ExpVal}
}
只有变量绑定可变时，才能赋值。当指代值是引用时，解引用自动进行。

修改本节的语言，让 @tt{let} 像之前那样引入不可变变量，可变变量则由
@tt{letmutable} 表达式引入，语法为：
@$${\mathit{Expression} ::= @tt{letmutable @${\mathit{Identifier}} = @${\mathit{Expression}} in @${\mathit{Expression}}}}
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.20"] @eopl-index-entry[@elem{@tt{letmutable} expression} "letccexpression"]]

}

@exercise[#:level 2 #:tag "ex4.21"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.21"] "Assignment"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.21"] "Binding" "fluid"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.21"] "Fluid binding"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.21"] @eopl-index-entry[@elem{@tt{setdynamic} expression} "setdynamicexpression"]]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.21"] "Shared variables"]
之前，我们建议两个相去很远的过程通过赋值交换信息，避免居间的过程知晓，从而使程序
更加模块化。这样的赋值常常应该是临时的，只在执行函数调用时生效。向语言
添加@term["dynamic assignment"]{动态赋值}（又称@term["fluid binding"]{流式绑定}）
组件，完成这一操作。生成式为：
@eopl-index["Dynamic assignment"]

@envalign*{
        \mathit{Expression} &::= @tt{setdynamic @m{\mathit{Identifier}} = @m{\mathit{Expression}} during @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{setdynamic-exp (var exp1 body)}}}

@tt{setdynamic} 表达式的效果是把@${exp_1} 的值临时赋给 @${var}，求 @${body} 的值，
重新给 @${var} 赋其原值，然后返回 @${body} 的值。变量 @${var} 必需已绑定。例如，
在下列表达式中：

@eopl-code{
@verbatim|{
let x = 11
in let p = proc (y) -(y,x)
   in -(setdynamic x = 17 during (p 22),
        (p 13))
}|
}

@${x} 是过程 @tt{p} 中的自由变量，在调用 @tt{(p 22)} 中值为 17，在调用 @tt{(p
13)} 中重设为 11，所以表达式的值为 @${5-2=3}。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.21"] "Assignment"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.21"] "Binding" "fluid"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.21"] "Fluid binding"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.21"] @eopl-index-entry[@elem{@tt{setdynamic} expression} "setdynamicexpression"]]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.21"] "Shared variables"]

}

@exercise[#:level 2 #:tag "ex4.22"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.22" "ex4.27"] "Statements"]
迄今为止，我们的语言都是@term["expression-oriented"]{面向表达式}的：我们感兴趣的
主要是表达式这种句法类别和它们的值。扩展语言，建模简单
的@term["statement-oriented"]{面向语句} 的语言，其规范概述如下。一定要@emph{遵循
语法}，分别写出过程来处理程序、语句和表达式。

@nested[#:style hangindent]{

@bold{值}@hspace[1]同 IMPLICIT-REFS。

}

@nested[#:style hangindent]{

@bold{语法}@hspace[1]使用下列语法：

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

非终结符 @${\mathit{Expression}} 指的是 IMPLICIT-REFS 语言中的表达式，可能稍有扩
展。

}

@nested[#:style hangindent]{

@bold{语义}@hspace[1]程序是一个语句。语句不返回值，而是修改和打印存储器。

赋值语句仍按通常的方式执行。打印语句求出实参的值，打印结果。@tt{if} 语句仍按通常
的方式执行。块语句定义见 @${\mathit{Statement}} 中的最后一个生成式。它把每个声明
变量绑定到一个未初始化的引用，然后执行块主体。这些绑定的作用域是块主体。

用如下断言写出语句的规范：

@$${@tt{(result-of @${stmt} @${\rho} @${\sigma_0})} = \sigma_0}

}

@nested[#:style hangindent]{

@bold{例子}@hspace[1]这里是一些例子。

@verbatim|{
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
}|

例 3 解释了块语句的作用域。

例 4 解释了语句和表达式的交互。过程值创建并存储于变量 @tt{f}。最后一行用实参 4
和 @tt{x} 调用这个过程；因为 @tt{x} 绑定到一个引用，解引用得 3。

}

}

@exercise[#:level 1 #:tag "ex4.23"]{

@eopl-index[#:suffix @exer-ref-range["ex4.23"] @eopl-index-entry[@elem{@tt{read} statement} "readstatement"]]
给@exercise-ref{ex4.22} 中的语言添加 @tt{read} 语句，形如 @tt{read @${var}}。这
一语句从输入读取一个非负数，存入指定的变量中。

}

@exercise[#:level 1 #:tag "ex4.24"]{

@eopl-index[#:suffix @exer-ref-range["ex4.24"] (eopl-index-entry @elem{@tt{do-while} statement} "dowhilestatement")]
@tt{do-while} 语句类似 @tt{while}，但是条件判断在其主体@emph{之后}执行。
给@exercise-ref{ex4.22} 中的语言添加 @tt{do-while} 语句。

}

@exercise[#:level 1 #:tag "ex4.25"]{

扩展@exercise-ref{ex4.22} 语言中的块语句，允许初始化变量。在你的解答中，变量的作
用域是否包含同一个块语句中后续声明的变量？

}

@exercise[#:level 3 #:tag "ex4.26"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.26"] "Mutual recursion"]
@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.26"] "Recursive programs" "mutual recursion"]
扩展前一道练习中的解答，允许同一块语句中声明的过程互递归。考虑给语言增加限制，块
中的过程声明要在变量声明之后。
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.26"] "Mutual recursion"]
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.26"] "Recursive programs" "mutual recursion"]

}

@exercise[#:level 3 #:tag "ex4.27"]{

@eopl-index["Subroutines"]
扩展前一道练习中的解答，增加@term["subroutine"]{子程序}。我们把子程序当过程用，
但是它不返回值，且其主体为语句而非表达式。然后增加子程序调用，作为一种新语句。扩
展块的语法，允许同时声明过程和子程序。这将如何影响指代值和表达值？如果在子程序调
用中使用过程会怎样？反过来呢？
@eopl-index[#:range-mark 'end #:suffix @exer-ref-range["ex4.22" "ex4.27"] "Statements"]

}

@section[#:style section-title-style-numbered #:tag "s4.4"]{MUTABLE-PAIRS：可变序对语言}

在@exercise-ref{ex3.9} 中，我们给语言添加了列表，但它们是不可变的：不像 Scheme
中，有 @tt{set-car!} 和 @tt{set-cdr!} 处理它们。

现在，我们给 IMPLICIT-REFS 添加可变序对。序对是表达值，具有如下操作：

@envalign*{
@bold{@tt{make-pair}}  &: \mathit{Expval} \times \mathit{Expval} \to \mathit{MutPair} \\
@bold{@tt{left}} &: \mathit{MutPair} \to \mathit{Expval} \\
@bold{@tt{right}}   &: \mathit{MutPair} \to \mathit{Expval} \\
@bold{@tt{setleft}}  &: \mathit{MutPair} \times \mathit{Expval} \to \mathit{Unspecified} \\
@bold{@tt{setright}}  &: \mathit{MutPair} \times \mathit{Expval} \to \mathit{Unspecified}
}

序对包含两个位置，二者可以分别赋值。由此得出语言值的@term["domain equations"]{定
义域方程}：

@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} + \mathit{MutPair} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)} \\
\mathit{MutPair} &= \mathit{Ref(ExpVal)} \times \mathit{Ref(ExpVal)}
}

我们把这种语言叫做 MUTABLE-PAIRS。

@subsection[#:style section-title-style-numbered #:tag "s4.4.1"]{实现}

@eopl-index[#:range-mark 'start "MUTABLE-PAIRS"]
我们可以直接用前例中的 @tt{reference} 数据类型实现可变序对。
代码如@figure-ref{fig-4.9} 所示。

完成图中的代码，给语言添加这些就很直接了。如@figure-ref{fig-4.10} 所示，我们给表
达值数据类型新增一种变体 @tt{mutpair-val}，给 @tt{value-of} 新增 5 行代码。我们
让 @tt{setleft} 返回任意值 82，让 @tt{setright} 返回 83。
用@elemref["trace-instrument"]{前述}辅助组件得到的示例跟踪日志
如@figure-ref{fig-4.11} 所示。

@eopl-figure{
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

@eopl-caption["fig-4.9"]{可变序对的拙劣实现}
}

@eopl-figure[#:position "!ht"]{
@codeblock[#:indent racket-block-offset]{
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

@eopl-caption["fig-4.10"]{给解释器添加可变序对模块}
}

@subsection[#:style section-title-style-numbered #:tag "s4.4.2"]{可变序对的另一种表示}

把可变序对表示为两个引用没有利用与 @tt{MutPair} 相关的已知信息。序对中的两个位置
虽然能够各自赋值，但它们不是独立分配的。我们知道它们会一起分配：如果序对的左侧是
一个位置，那么右侧是下一个位置。所以我们还可以用左侧的引用表示序对。
代码如@figure-ref{fig-4.13} 所示，不需再做其他修改。

@eopl-figure{
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

@eopl-caption["fig-4.11"]{MUTABLE-PAIRS求值的跟踪日志}
}

@eopl-figure{
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

@eopl-caption["fig-4.12"]{MUTABLE-PAIRS求值的跟踪日志，续}
}

与之类似，堆中的任何聚合对象都可以用其第一个位置的指针表示。但是，如果不提供区域
的长度信息，指针本身无法指明一片内存区域（见@exercise-ref{ex4.30}）。缺乏长度信
息是经典安全问题的一大来源，比如写数组越界。

@eopl-figure[#:position "!ht"]{
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

@eopl-caption["fig-4.13"]{可变序对的另一种表示
                          @eopl-index["MUTABLE-PAIRS"]}
}

@eopl-index[#:range-mark 'end "MUTABLE-PAIRS"]

@exercise[#:level 2 #:tag "ex4.28"]{

写出五个可变序对操作的推理规则规范。

}

@exercise[#:level 2 #:tag "ex4.29"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.29" "ex4.30"] "Array"]
给语言添加数组。添加新操作符 @tt{newarray}、@tt{arrayref} 和 @tt{arrayset}，用它
们来创建、解引用和更新数组。这需要：

@envalign*{
\mathit{ArrVal} &= \mathit{(Ref(ExpVal))} \\
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} + \mathit{ArrVal} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)}
}

由于数组中的位置是连续的，用上述第二种表示。下面程序的结果是什么？

@eopl-code{
@verbatim|{
let a = newarray(2,-99)
    p = proc (x)
         let v = arrayref(x,1)
         in arrayset(x,1,-(v,-1))
in begin arrayset(a,1,0); (p a); (p a); arrayref(a,1) end
}|
}

这里 @tt{newarray(2,-99)} 要创建长度为 2 的数组，数组中的每个位置都包含 -99。
@tt{begin} 表达式定义同@exercise-ref{ex4.4}。令数组索引从 0 开始，所以长度为 2
的数组索引为 0 和 1。

}

@exercise[#:level 2 #:tag "ex4.30"]{

@eopl-index[#:range-mark 'end "Array"]
给@exercise-ref{ex4.29} 的语言添加过程 @tt{arraylength}，返回数组的长度。你的过
程运行时间应为常数。@tt{arrayref} 和 @tt{arrayset} 一定要查验索引，确保索引值在
数组长度之内。

}

@section[#:style section-title-style-numbered #:tag "s4.5"]{传参变体}

当过程主体执行时，其形参绑定到一个指代值。那个值从哪儿来？它一定是过程调用传入实
参的值。我们已见过两种传参方式：

@itemlist[

 @item{自然传参，指代值与实参的表达值相同（@pageref{pass-by-value}）。}

 @item{按值调用，指代值是一个引用，指向一个位置，该位置包含实参的表达值
 （@secref{s4.3}）。这是最常用的传参方式。
 @eopl-index[(eopl-index-entry "Call-by-value" "Callbyvalue")]}

]

本节探讨其他一些传参机制。

@subsection[#:style section-title-style-numbered #:tag "s4.5.1"]{按指调用}

@eopl-index[#:range-mark 'start (eopl-index-entry "Call-by-reference" "Callbyreference")]
考虑下面的表达式：

@nested{
@eopl-code{
@verbatim|{
let p = proc (x) set x = 4
in let a = 3
   in begin (p a); a end
}|
}

按值调用时，绑定到 @tt{x} 的指代值是一个引用，它包含的初始值与绑定到 @tt{a} 的引
用相同，但这些引用互不相干。所以赋值给 @tt{x} 不会影响引用 @tt{a} 的内容，整个表
达式的值是 3。

}

按值调用时，当过程给某个参数赋新值，调用者无法获悉。当然，如果传给调用者的参数像
@secref{s4.4}那样包含可变序对，那么调用者能看到 @tt{setleft} 和 @tt{setright} 的
效果，但看不到 @tt{set} 的效果。

虽然这样隔离调用者和被调者常合所愿，但有些时候，给过程传递一个位置，并让过程给这
个位置赋值也不无好处。要这样做，可以给过程传递一个引用，该引用指向调用者变量的位
置，而不是变量的内容。这种传参机制叫做@term["call-by-reference"]{按指调用}。如果
操作数正是变量引用，那就传递变量位置的引用。然后，过程的形参绑定到这个位置。如果
操作数是其他类型的表达式，那么形参绑定到一个新位置，该位置包含操作数的值，就像按
值调用一样。在上例中使用按指调用，把 4 赋给 @tt{x} 等效于把 4 赋给 @tt{a}，所以
整个表达式返回 4，而不是 3。

按指调用过程，且实参为变量时，传递的不是按值调用中变量所在位置的内容，而是那个变
量的@emph{位置}。例如，考虑：

@nested{
@eopl-code{
@verbatim|{
let p = proc (x) set x = 44
in let g = proc (y) (f y)
   in let z = 55
      in begin (g z); z end
}|
}

调用过程 @tt{g} 时，@tt{y} 绑定到 @tt{z} 的位置，而不是那个位置的内容。类似地，
调用 @tt{f} 时，@tt{x} 绑定到同一个位置。所以，@tt{x}、@tt{y} 和 @tt{z} 都绑定到
同一位置，@tt{set x = 44} 的效果是把那个位置的内容设为 44。因此，整个表达式的值
是 44。执行这个表达式的跟踪日志如@figure-ref{fig-4.14} 和 @countref{fig-4.15} 所
示。在本例中，@tt{x}、@tt{y} 和 @tt{z} 最终都绑定到位置 5。

}

按指调用的常见用法是返回多个值。过程以通常方式返回一个值，并给按指传递的参数赋其
他值。另一种例子，对换变量的值：

@nested{
@eopl-code{
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

采用按指调用，这会对换 @tt{a} 和 @tt{b} 的值，所以它返回 11。但如果用已有的按值
调用解释器执行这个程序，它会返回 -11，因为在对换过程的内部赋值对变量 @tt{a} 和
@tt{b} 毫无影响。

}

就像在按值调用中一样，在按指调用中，变量仍然指代表达值的引用：

@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} \\
\mathit{DenVal} &= \mathit{Ref(ExpVal)} + \mathit{ExpVal}
}

唯一需要改变的是新位置的分配。按值调用中，求每个操作数的值都要分配新位置；按指调
用中，只在求@emph{非变量}操作数的值时才分配新位置。

这很容易实现。函数 @tt{apply-procedure} 必需修改，因为不是每个过程调用都要分配新
位置。那份责任必须上移至 @tt{value-of} 中的 @tt{call-exp} ，因为它才具有做出判断
所需的信息。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{apply-procedure}} : @${\mathit{Proc} \times \mathit{ExpVal} \to \mathit{ExpVal}}}
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body
          (extend-env var val saved-env))))))
]
}

然后我们修改 @tt{value-of} 中的 @tt{call-exp}，引入新函数 @tt{value-of-operand}
来做必要的判断。

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(call-exp
 (rator rand)
 (let ((proc (expval->proc (value-of rator env)))
       (arg (value-of-operand rand env)))
   (apply-procedure proc arg)))
}
}

过程 @tt{value-of-operand} 检查操作数是否为变量。如果是，则返回那个变量指代的引
用，然后传给过程 @tt{apply-procedure}；否则，它求出操作数的值，在新位置放入那个
值，并返回指向该位置的引用。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{value-of-operand}} : @${\mathit{Exp} \times \mathit{Env} \to \mathit{Ref}}}
(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp (var) (apply-env env var))
      (else
        (newref (value-of exp env))))))
]
}

我们也可以按照同样的方式修改 @tt{let}，但我们不这样做，因此语言中仍然保留按值调
用的功能。

多个按指调用参数可以指向同一个位置，如下面的程序所示。

@nested{
@eopl-code{
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

它的值为 4，因为 @tt{x} 和 @tt{y} 指向同一个位置，即 @tt{b} 的绑定。这种现象叫
做@eopl-index{Aliases}@term["variable aliasing"]{变量别名}。这里的 @tt{x} 和
@tt{y} 是同一个位置的别名（名字）。通常，我们在给一个变量赋值时并不想改变另一个
变量的值，所以别名会导致程序难以理解。

}

@eopl-figure{
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

@eopl-caption["fig-4.14"]{CALL-BY-REFERENCE 的简单求值}
}

@eopl-figure[#:position "!ht"]{
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

@eopl-caption["fig-4.15"]{CALL-BY-REFERENCE 的简单求值，续}
}

@eopl-index[#:range-mark 'end (eopl-index-entry "Call-by-reference" "Callbyreference")]

@exercise[#:level 1 #:tag "ex4.31"]{

写出 CALL-BY-REFERENCE 的推理规则规范。

}

@exercise[#:level 1 #:tag "ex4.32"]{

扩展语言 CALL-BY-REFERENCE，支持多参数过程。

}

@exercise[#:level 2 #:tag "ex4.33"]{

扩展语言 CALL-BY-REFERENCE，也令其支持按值调用的过程。

}

@exercise[#:level 1 #:tag "ex4.34"]{

给语言添加按指调用的 @tt{let}，名为 @tt{letref}。写出规范并实现。

}

@exercise[#:level 2 #:tag "ex4.35"]{

在按值调用框架下，我们仍能享受按指调用的便利。扩展语言 IMPLICIT-REF，添加新表达
式：

@envalign*{
        \mathit{Expression} &::= @tt{ref @m{\mathit{Identifier}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{ref-exp (var)}}}

这与语言 EXPLICIT-REF 不同。因为引用只能从变量获得。这就使我们能用按值调用语言写
出类似 @tt{swap} 的程序。下面表达式的值是什么？

@eopl-code{
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

此处使用支持多声明的 @tt{let}（@exercise-ref{ex3.16}）。这种语言的表达值和指代值
是什么？

}

@exercise[#:level 1 #:tag "ex4.36"]{

@eopl-index[#:suffix @exer-ref-range["ex4.36"] "Array"]
大多数语言支持数组，在按指调用中，数组引用通常以类似变量引用的方式处理。如果操作
数是数组引用，那就不给被调过程传递它的内容，而是传递引用指向的位置。比如，需要调
用对换过程的常见情形是交换数组元素，传递数组引用就能用上对换过程。给按指调用语言
添加@exercise-ref{ex4.29} 中的数组操作符，扩展 @tt{value-of-operand}，处理这种情
况，使下例中的过程调用能够如愿执行：

@centered{@code{((swap (arrayref a i)) (arrayref a j))}}

要是下面这样呢？

@centered{@code{((swap (arrayref a (arrayref a i))) (arrayref a j))}}

}

@exercise[#:level 2 #:tag "ex4.37"]{

@eopl-index[#:range-mark 'start #:suffix @exer-ref-range["ex4.37"] (eopl-index-entry "Call-by-value-result" "Callbyvalueresult")]
@term["call-by-value-result"]{按值和结果调用} 是按指调用的一种变体。在按值和结果
调用中，实参必需是变量。传递参数时，形参绑定到新的引用，初值为实参的值，就像按值
调用一样。然后过程主体照常执行。但过程主体返回时，新引用处的值复制到实参指代的引
用中。因为这样可以改进内存分配，所以可能比按指调用更高效。实现按值和结果调用，写
出一个过程，使之在按指调用与按值和结果调用中产生不同的答案。
@eopl-index[#:range-mark 'end  #:suffix @exer-ref-range["ex4.37"] (eopl-index-entry "Call-by-value-result" "Callbyvalueresult")]

}

@subsection[#:style section-title-style-numbered #:tag "s4.5.2"]{懒求值：按名调用和按需调用}

@eopl-index["Eager evaluation"]
@eopl-index[#:range-mark 'start "Lazy evaluation"]
@eopl-index[#:range-mark 'start "Operands"]
迄今为止，我们讨论的所有参数传递机制都是@term["eager"]{即时}的：它们总是找出每个
操作数的值。现在我们来看另一种截然不同的传参机制，名叫@term["lazy
evaluation"]{懒求值}。在懒求值中，操作数的值直到过程主体需要时才会求取。如果过程
主体从未引用相关参数，就不需求操作数的值。

这可能使程序免于永不终止。例如，考虑：

@nested{
@eopl-code{
@verbatim|{
letrec infinite-loop (x) = infinite-loop(-(x,-1))
in let f = proc (z) 11
   in (f (infinite-loop 0))
}|
}

这里的 @tt{infinite-loop} 是一个过程，调用时永不终止。@tt{f} 是一个过程，调用时
不引用它的参数，总返回 11。我们考虑过的任何一种传参机制都无法使这个程序终止。但
在懒求值中，这个程序将返回 11，因为操作数 @tt{(infinite-loop 0)} 没有求值。

}

现在，我们使用懒求值修改我们的语言。在懒求值中，如无必要，我们不求操作数表达式的
值。因此，我们将过程的绑定变量与未求值的操作数关联起来。当过程主体需要绑定变量的
值时，我们先求对应操作数的值。有时，我们把不求操作数的值而传给过程
叫做@term["frozen"]{冻结}，把过程求操作数的值叫做@term["thawed"]{解冻}。
@eopl-index["Freeze"]

当然，我们还要加入过程求值时的环境。要这样，我们引入一种新的数据类型，
@term["thunk"]{值箱}。值箱包含一个表达式、一个环境。

@nested{
@eopl-code{
@racketblock[
(define-datatype thunk thunk?
  (a-thunk
   (exp1 expression?)
   (env environment?)))
]
}

当过程需要使用绑定变量的值时，会求相应值箱的值。

}

我们面对的情况稍稍复杂一些，因为我们需要同时容纳懒求值、计算效果和即时求值
（@tt{let} 需要）。因此，我们把指代值定为内存位置的引用，位置包含表达值或者值箱。

@envalign*{
\mathit{DenVal} &= \mathit{Ref(ExpVal + Thunk)} \\
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc}
}

我们的位置分配策略与按指调用类似：如果操作数是变量，那么我们传递指代的引用；否则，
我们给未求值的参数在新位置放一个值箱，传递该位置的引用。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{value-of-operand}} : @${\mathit{Exp} \times \mathit{Env} \to \mathit{Ref}}}
(define value-of-operand
  (lambda (exp env)
    (cases expression exp
      (var-exp (var) (apply-env env var))
      (else
        (newref (a-thunk exp env))))))
]
}

求 @tt{var-exp} 的值时，我们首先找到变量绑定的位置。如果该位置是一个表达值，那么
返回这个值，作为 @tt{var-exp} 的值。如果它包含一个值箱，那么我们求取并返回值箱的
值。这叫做@term["call by name"]{按名调用}。
@eopl-index[#:range-mark 'start (eopl-index-entry "Call-by-name" "Callbyname")]

@eopl-code{
@codeblock[#:indent racket-block-offset]{
(var-exp (var)
  (let ((ref1 (apply-env env var)))
    (let ((w (deref ref1)))
      (if (expval? val)
        val
        (value-of-thunk val)))))
}
}

过程 @tt{value-of-thunk} 定义如下：

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{value-of-thunk}} : @${\mathit{Thunk} \to \mathit{ExpVal}}}
(define value-of-thunk
  (lambda (th)
    (cases thunk th
      (a-thunk (exp1 saved-env)
        (value-of exp1 saved-env)))))
]
}

或者，一旦发现值箱的值，我们可以把表达值放到同一个位置，这样就不需要再次求值箱的
值。这种方式叫做@term["call by need"]{按需调用}。
@eopl-index[#:range-mark 'start (eopl-index-entry "Call-by-need" "Callbyneed")]

@nested{
@eopl-code{
@codeblock[#:indent racket-block-offset]{
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
}

这里用到了名为@term["memoization"]{助记法} 的通用策略。
@eopl-index["Memoization"]

}

各种形式的懒求值引人之处在于，即使没有效果，通过它也能以相当简单的方式思考程序。
把过程调用替换为过程的主体，把过程主体内对每个形参的引用替换为对应的操作数，就能
建模过程调用。这种求值策略是 lambda 演算的基础，在 lambda 演算中，它叫做
@term[@elem{@${\beta}-reduction}]{@${\beta}-推导}。
@eopl-index[(eopl-index-entry @elem{@${\beta}-reduction} "betareduction")]
@eopl-index["Lambda calculus"]

不幸的是，按名调用和按需调用使求值顺序难以确定，而这对理解有效果的程序至关重要。
但没有效果时，这不成问题。所以懒求值盛行于函数式编程语言（没有计算效果的那些），
在别处却杳无踪影。
@eopl-index[#:range-mark 'end (eopl-index-entry "Call-by-name" "Callbyname")]
@eopl-index[#:range-mark 'end (eopl-index-entry "Call-by-need" "Callbyneed")]
@eopl-index[#:range-mark 'end "Lazy evaluation"]
@eopl-index[#:range-mark 'end "Operands"]

@exercise[#:level 1 #:tag "ex4.38"]{

下面的例子展示了@exercise-ref{ex3.25} 在按需调用中的变体。@exercise-ref{ex3.25}
中的原始程序在按需调用中可行吗？如果下面的程序在按值调用中运行呢？为什么？

@eopl-code{
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

修改 @tt{value-of-operand}，避免为常量和过程生成值箱。

}

@exercise[#:level 2 #:tag "ex4.41"]{

写出按名调用和按需调用的推理规则规范。

}

@exercise[#:level 2 #:tag "ex4.42"]{

给按需调用解释器添加懒求值的 @tt{let}。

}
