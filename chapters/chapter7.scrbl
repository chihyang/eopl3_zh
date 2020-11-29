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

@title[#:style part-title-style-numbered #:tag "types"]{类型}

我们已理解如何用解释器建模程序的运行时行为。现在，我们用同样的技术不加运行
地@emph{分析}或@emph{预测}程序的行为。

我们已见识过一些了：我们的词法地址翻译器在分析阶段预测程序在运行时如何从环境中找
出各个变量。而且，翻译器本身看起来就像一个解释器，只是我们传递的不是环境，而是静
态环境。

我们的目标是分析程序，预测程序求值是否@term["safe"]{安全}，即，求值过程是否能避
免某些类型的错误，但安全的含义视语言而不同。如果我们能保证求值是安全的，我们就能
确保程序满足其合约。

本章，我们考虑类似@secref{expr} LETREC的语言。这些语言求值安全，当且仅当：

@itemlist[#:style 'ordered

 @item{每个待求值的变量 @${var} 都已绑定。}

 @item{每个待求值的差值表达式 @tt{(diff-exp @${exp_1} @${exp_2})} 中，@${exp_1}
 和 @${exp_2} 的值都是 @tt{num-val}。}

 @item{每个待求值的表达式 @tt{(zero?-exp @${exp_1})} 中，@${exp_1} 的值都是
 @tt{num-val}。}

 @item{每个待求值的条件表达式 @tt{(if-exp @${exp_1} @${exp_2} @${exp_3})} 中，
 @${exp_1} 的值都是 @tt{bool-val}。}

 @item{每个待求值的过程调用 @tt{(call-exp @${rator} @${rand})} 中，@${rator} 的
 值都是 @tt{proc-val}。}

]

这些条件确保每个操作符都作用于正确类型的操作数。因此，我们说违反这些条件
是@term["type error"]{类型错误}。

安全的求值仍可能因为其他原因而失败：除以零，取空列表的 @tt{car}，等等。我们不把
这些算作安全的定义，因为在预测安全性时，保证这些条件要比上面列出的难得多。同样地，
安全的求值可能永远运行。我们的安全定义不包含不终止，因为检查程序是否终止也很困难
（事实上，这一般是无法判定的）。有些语言的类型系统给出比上述更强的保证，但它们要
比我们这里考虑的复杂得多。

我们的目标是写出过程，查看程序文本，接受或者拒绝它。而且，我们希望我们的分析过程
保守一点：如果分析接受程序，那么我们确保求程序的值是安全的。如果分析不能确定求值
是否安全，它必须拒绝程序。我们称这样的分析是@term["sound"]{健壮的}。

拒绝所有程序的分析仍是健壮的，可我们还是想让我们的分析接受一大批程序。本章的分析
将接受足够多的程序，因此是有用的。

这里是一些示例程序，以及它们应被分析拒绝或接受：

@eopl-code{
@verbatim|{
if 3 then 88 else 99      |@smaller{拒绝：条件非布尔值}
proc (x) (3 x)            |@smaller{拒绝：rator非过程值}
proc (x) (x 3)            |@smaller{接受}
proc (f) proc (x) (f x)   |@smaller{接受}
let x = 4 in (x 3)        |@smaller{拒绝：rator非过程值}

(proc (x) (x 3)           |@smaller{拒绝：同前例}
 4)

let x = zero?(0)          |@smaller{拒绝：diff-exp 参数非整数}
in -(3, x)

(proc (x) -(3,x)          |@smaller{拒绝：同前例}
 zero?(0))

let f = 3                 |@smaller{拒绝：rator非过程值}
in proc (x) (f x)

(proc (f) proc (x) (f x)  |@smaller{拒绝：同前例}
 3)

letrec f(x) = (f -(x,-1)) |@smaller{接受，不终止，但是安全}
in (f 1)
}|
}

虽然最后一个例子求值不终止，但根据上述定义，求值仍是安全的，所以我们的分析可以接
受它。之所以接受它，是因为我们的分析器不够好，不足以判定这个程序不会终止。

@section[#:style section-title-style-numbered #:tag "s7.1"]{值及其类型}

由于安全条件只涉及 @tt{num-val}、@tt{bool-val} 和 @tt{proc-val}，有人可能以为记
录这三种类型就足够了。但那是不够的：如果我们只知道 @tt{f} 绑定到一个
@tt{proc-val}，我们根本无法确认 @tt{(f 1)} 的值。从这个角度来看，我们需要更细致
地记录与过程相关的信息。这些更细致的信息叫做语言的@term["type structure"]{类型结
构}。

我们的语言将有一种非常简单的类型结构。现在，考虑 LETREC 中的表达值。这些值只包含
单参数过程，但处理@exercise-ref{ex3.33} 中的多参数过程也很明了：只需做些额外工作，
没有任何新思想。

@big-bracket[#:title "类型语法"]{
@envalign*{\mathit{Type} &::= \mathit{int} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{int-type ()}} \\[5pt]
           \mathit{Type} &::= \mathit{bool} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{bool-type ()}} \\[5pt]
           \mathit{Type} &::= @tt{(@m{\mathit{Type}} -> @m{\mathit{Type}})} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{proc-type (arg-type result-type)}}}
}

要理解这个系统如何工作，让我们来看些例子。

@big-bracket[#:title "值及其类型的例子"]{
@nested[#:style 'noindent]{

@tt{3} 的值类型为 @tt{int}。

@tt{-(33,22)} 的值类型为 @tt{int}。

@tt{zero?(11)} 的值类型为@tt{bool}。

@tt{proc (x) -(x,11)} 的值类型为 @tt{int -> int}，因为给定一个整数时，它返回一个
整数。

@tt{proc (x) let y = -(x,11) in -(x,y)}@linebreak[]的值类型为 @tt{int -> int}，
因为给定一个整数时，它返回一个整数。

@tt{proc (x) if x then 11 else 22}@linebreak[]的值类型为 @tt{bool -> int}，因为
给定一个布尔值时，它返回一个整数。

@tt{proc (x) if x then 11 else zero?(11)} 在我们的类型系统中没有类型，因为给定一
个布尔值时，它既可能返回一个整数，也可能返回一个布尔值，而我们没有描述这种行为的
类型。

@tt{proc (x) proc (y) if y then x else 11}@linebreak[]的值类型为 @tt{(int ->
(bool -> int))}，因为给定一个布尔值时，它返回一个过程，该过程取一布尔值，返回一
整数。

@tt{proc (f) (f 3)} 的值类型为 @tt{((int -> @${t}) -> @${t})}，@${t} 是任意类型，
因为给定一个类型为 @tt{(int -> @${t})} 的过程，它返回类型为 @${t} 的值。

@tt{proc (f) proc (x) (f (f x))} 的值类型为 @tt{((@${t} -> @${t}) -> (@${t} ->
@${t}))}，@${t} 是任意类型，因为给定一个类型为 @tt{(@${t} -> @${t})} 的过程，它
返回另一过程，该过程取一类型为 @${t} 的参数，返回一类型为 @${t} 的值。

}}

我们用下面的定义解释这些例子。

@nested{

@definition[#:title #f #:tag "d7.1.1"]{
 性质@exact-elem{“}表达值v的类型为t@exact-elem{”}由对 t 进行归纳得到：

 @itemlist[

  @item{当且仅当表达值是一个 @tt{num-val}，其类型为 @tt{int}。}

  @item{当且仅当表达值是一个 @tt{bool-val}，其类型为 @tt{bool}。
  @eopl-index[(eopl-index-entry @elem{@tt{bool} type} "booltype")]}

  @item{当且仅当表达值是一个 @tt{proc-val}，且给定类型为 @${t_1} 的参数时，发生
  如下之一：

  @itemlist[#:style 'ordered

   @item{返回值类型为 @${t_2}}

   @item{不终止}

   @item{发生类型错误之外的错误}
  ]

  其类型为 @tt{(@${t_1 \to t_2})}。} ]

}

有时，我们不说@exact-elem{“}@${v}类型为@${t}@exact-elem{”}，而说
@exact-elem{“}@${v} 具有类型 @${t}@exact-elem{”}。

}

此定义归纳自 @${t}。但是它依赖于上面另行定义的类型错误。

在该系统中，值 @${v} 可以有多个类型。比如，值 @tt{proc (x) x} 类型为 @tt{(@${t
\to t})}，@${t} 是任意类型。有些值可能没有类型，比如 @tt{proc (x) if x then 11
else zero?(11)}。

@exercise[#:level 1 #:tag "ex7.1"]{

下面是一些含有闭包的表达式。想想每个表达式的值。每个值的类型是什么（可能不止一
个）？有些值的类型在我们的有类型语言中可能无法描述。

@itemlist[#:style 'ordered

@item{@tt{proc (x) -(x,3)}}

@item{@tt{proc (f) proc (x) -((f x), 1)}}

@item{@tt{proc (x) x}}

@item{@tt{proc (x) proc (y) (x y)}}

@item{@tt{proc (x) (x 3)}}

@item{@tt{proc (x) (x x)}}

@item{@tt{proc (x) if x then 88 else 99}}

@item{@tt{proc (x) proc (y) if x then y else 99}}

@item{
@verbatim|{
(proc (p) if p then 88 else 99
 33)
}|
}

@item{
@verbatim|{
(proc (p) if p then 88 else 99
 proc (z) z)
}|
}

@item{
@verbatim|{
proc (f)
 proc (g)
  proc (p)
   proc (x) if (p (f x)) then (g 1) else -((f x),1)
}|
}

@item{
@verbatim|{
proc (x)
 proc(p)
  proc (f)
   if (p x) then -(x,1) else (f p)
}|
}

@item{
@verbatim|{
proc (f)
 let d = proc (x)
          proc (z) ((f (x x)) z)
 in proc (n) ((f (d d)) n)
}|
}

]

}

@exercise[#:level 2 #:tag "ex7.2"]{

根据@definition-ref{d7.1.1}，有没有表达值恰好有两种类型？

}

@exercise[#:level 2 #:tag "ex7.3"]{

在语言 LETREC 中，能否判定表达值 @${val} 的类型为 @${t}？

}

@section[#:style section-title-style-numbered #:tag "s7.2"]{赋予表达值类型}

现在，我们只解决了表达值的类型。为了分析程序，我们要写出过程，预测表达式值的类型。

更准确地说，我们的目标是写出过程 @tt{type-of}。给定一个表达式（名为 @${exp}）和
一个将变量映射到某一类型的@term["type environment"]{类型环境}（名为@${tenv}），
它赋给 @${exp} 一个类型 @${t}，且 @${t} 具有性质：

@big-bracket[#:title @elem{@tt{type-of} 规范}]{
@nested[#:style 'noindent]{

不论何时求 @${exp} 的值，若环境中所有变量对应值的类型都由 @${tenv} 指定，则发生
如下之一：

@itemlist[

@item{结果类型为 @${t}，}

@item{求值不终止，或}

@item{求值因类型错误之外的原因失败。}
]
}
}

如果我们可以赋予表达式一个类型，我们说该表达式是@term["well-typed"]{正常类型} 的，
否则说它是@term["ill-typed"]{异常类型} 或@term[#:full #f "has no type"]{无类型}
的。

我们的分析基于以下原则：如果我们能预测表达式中所有子表达式的值类型，就能预测表达
式的值类型。

我们用这一想法写出 @tt{type-of} 遵循的一些规则。设 @${tenv} 为一@emph{类型环境}，
将各个变量映射到类型。那么我们有：

@big-bracket[#:title "简单判类规则"]{
@verbatim|{
(type-of (const-exp |@${num}) |@${tenv}) = int

(type-of (var-exp |@${num}) |@${tenv}) = |@${tenv}(|@${var})

|@${\infer{@tt{(type-of (zero?-exp @${exp_1}) @${tenv}) = bool}}
          {@tt{(type-of @${exp_1} @${tenv}) = int}}}

|@${\infer{@tt{(type-of (diff-exp @${exp_1} @${exp_2}) @${tenv}) = int}}
          {@tt{(type-of @${exp_1} @${tenv}) = int} &
           @tt{(type-of @${exp_2} @${tenv}) = int}}}

|@${\infer{@tt{(type-of (let-exp @${var} @${exp_1} @${body}) @${tenv}) = @${t_2}}}
          {@tt{(type-of @${exp_1} @${tenv}) = @${t_1}} &
           @tt{(type-of @${body} [@${var}=@${t_1}]@${tenv}) = @${t_2}}}}

|@${\infer{@tt{(type-of (if-exp @${exp_1} @${exp_2} @${exp_3}) @${tenv}) = @${t}}}
          {\begin{gathered}
           @tt{(type-of @${exp_1} @${tenv}) = bool} \\
           @tt{(type-of @${exp_2} @${tenv}) = @${t}} \\
           @tt{(type-of @${exp_2} @${tenv}) = @${t}}
           \end{gathered}}}

|@${\infer{@tt{(type-of (call-exp @${rator} @${rand}) @${tenv}) = @${t_2}}}
          {@tt{(type-of @${rator} @${tenv}) = @${t_1 \to t_2}} &
           @tt{(type-of @${rand} @${tenv}) = @${t_1}}}}
}|
}

@elemtag["suitable-env"]{若我们在适当的环境中求类型为 @${t} 的表达式 @${exp} 的
值}，我们不仅知道值的类型为 @${t}，而且知道与这个值有关的历史信息。因为求
@${exp} 的值保证是安全的，我们知道 @${exp} 的值一定是由符合类型 @${t} 的操作符产
生的。在@secref{modules}，我们更细致地思考数据抽象时，这种观点会很有帮助。

过程呢？如果 @tt{proc(@${var}) @${body}} 类型为 @${t_1 \to t_2}，那么应该用类型
为 @${t_1} 的参数调用它。求 @${body} 的值时，绑定到变量 @${var} 的值类型为
@${t_1}。

这意味着如下规则：

@$${\infer{@tt{(type-of (proc-exp @${var} @${body}) @${tenv}) = @${t_1 \to t_2}}}
          {@tt{(type-of @${body} [@${var}=@${t_1}]@${tenv}) = @${t_2}}}}

这条规则是健壮的：如果 @tt{type-of} 对 @${body} 做出了正确预测，那么它也能对
@tt{(proc-exp @${var} @${body})} 做出正确预测。

只有一个问题：如果我们要计算 @tt{proc} 表达式的类型，我们怎么找出绑定变量的类型
@${t_1}？它无处可寻。

要解决这个问题，有两种标准设计：

@itemlist[

 @item{@term["Type Checking"]{类型检查}：按这种方法，程序员需要指出缺失的绑定变
 量类型，类型检查器推断其他表达式的类型，检查它们是否一致。}

 @item{@term["Type Inference"]{类型推导}：按这种方法，类型检查器根据程序中变量的
 使用，尝试@term["infer"]{推断} 绑定变量的类型。如果语言设计得当，类型检查器可以
 推断出大多数甚至所有这样的类型。}

]

我们依次研究它们。

@exercise[#:level 1 #:tag "ex7.4"]{

用本节的规则，像@pageref{deriv-tree}那样，写出 @tt{proc (x) x} 和 @tt{proc (x)
(x y)} 的类型推导。运用规则，给每个表达式赋予至少两种类型。这些表达式的值类型相
同吗？

}

@section[#:style section-title-style-numbered #:tag "s7.3"]{CHECKED：带有类型检查的语言}

@eopl-index[#:range-mark 'start "CHECKED"]
除了要求程序员写出所有绑定变量的类型之外，我们的新语言和 LETREC 相同。对由
@tt{letrec} 绑定的变量，我们还要求程序员指定过程结果的类型。

这里是一些 CHECKED 程序例子。

@eopl-code{
@verbatim|{
proc (x : int) -(x,1)

letrec
 int double (x : int) = if zero?(x)
                        then 0
                        else -((double -(x,1)), -2)
in double

proc (f : (bool -> int)) proc (n : int) (f zero?(n))
}|
}

@tt{double} 结果的类型为 @tt{int}，但 @tt{double} 本身的类型为 @tt{(int -> int)}，
因为它是一个过程，取一整数，返回一整数。

要定义这种语言的语法，我们改变 @tt{proc} 和 @tt{letrec} 表达式的生成式。

@big-bracket[#:title "修改后的生成式，适用于 CHECKED"]{
@nested[#:style small]{
@envalign*{
\mathit{Expression} &::= @tt{proc (@m{\mathit{Identifier : Type}}) @m{\mathit{Expression}}} \\[-3pt]
  &\mathrel{\phantom{::=}} \fbox{@tt{proc-exp (var ty body)}} \\[5pt]
\mathit{Expression} &::= @tt{letrec} \\[-3pt]
  &\mathrel{\phantom{::=}} \phantom{x}@tt{@m{\mathit{Type}} @m{\mathit{Identifier}} (@m{\mathit{Identifier}} : @m{\mathit{Type}}) = @m{\mathit{Expression}}} \\[-3pt]
  &\mathrel{\phantom{::=}} @tt{in @m{\mathit{Expression}}} \\[-3pt]
  &\mathrel{\phantom{::=}} \fbox{\begin{math}\begin{alignedat}{-1}
                                  &@tt{letrec-exp} \\
                                  &\phantom{xx}@tt["("]{p-result-type p-name b-var b-var-type} \\
                                  &\phantom{xxx}@tt{p-body} \\
                                  &\phantom{xxx}@tt{letrec-body}@tt[")"]
                                 \end{alignedat}\end{math}}
}}}

对指定绑定变量类型的 @tt{proc} 表达式，规则变为：

@$${\infer{@tt{(type-of (proc-exp @${var} @${t_{var}} @${body}) @${tenv}) = @${t_{var} \to t_{res}}}}
          {@tt{(type-of @${body} [@${var}=@${t_{var}}]@${tenv}) = @${t_{res}}}}}

@tt{letrec} 呢？典型的 @tt{letrec} 如下：

@centered{
@verbatim|{
letrec
  |@${t_{res}} |@${p} (|@${var} : |@${t_{var}}) = |@${e_{proc\mbox{-}body}}
in |@${e_{letrec\mbox{-}body}}
}|
}

该表达式声明一个名为 @${p} 的过程，其形参是类型为 @${t_{var}} 的变量 @${var}，主
体为 @${e_{proc\mbox{-}body}}。因此，@${p} 的类型应为 @${t_{var} \to t_{res}}。

检查 @tt{letrec} 中的表达式 @${e_{proc\mbox{-}body}} 和
@${e_{letrec\mbox{-}body}} 时，类型环境中的所有变量都必须有正确的类型。我们可以
用定界规则判断当前作用域属于哪些变量，并由此判断变量对应的类型。

@${e_{letrec\mbox{-}body}} 在过程名 @${p} 的作用域内。如上所述，@${p} 的类型声明
为 @${t_{var} \to t_{res}}。因此，检查 @${e_{letrec\mbox{-}body}} 时的类型环境应
为：

@$${tenv_{letrec\mbox{-}body} = \text{[}p=(t_{var} \to t_{res})\text{]}tenv}

@${e_{proc\mbox{-}body}} 呢？@${e_{proc\mbox{-}body}} 在变量 @${p} 的作用域内，
@${p} 的类型为 @${t_{var} \to t_{res}}；@${e_{proc\mbox{-}body}} 也在变量
@${var} 的作用域内，@${var} 的类型为 @${t_{var}}。因此，@${e_{proc\mbox{-}body}}
的类型环境应为：

@$${tenv_{proc\mbox{-}body} = \text{[}var=t_{var}\text{]}tenv_{letrec\mbox{-}body}}

而且，在这个类型环境中，@${e_{proc\mbox{-}body}} 的结果类型应为 @${t_{res}}。

把这些写成一条规则，我们有：

@$${\infer{@tt{(type-of}\ @tt{(letrec-exp} @${t_{res}} p @tt{(}var : t_{var}@tt{)} @tt{=}\
           e_{proc\mbox{-}body} @tt{in} e_{letrec\mbox{-}body}@tt{)} tenv@tt{)} @tt{=}\ t}
          {\begin{gathered}
           @tt{(type-of @${e_{proc\mbox{-}body}} [@${var}=@${t_{var}}][@${p}=@${(t_{var} \to t_{res})}]@${tenv}) = @${t_{res}}} \\
           @tt{(type-of @${e_{letrec\mbox{-}body}} [@${p}=@${(t_{var} \to t_{res})}]@${tenv}) = @${t}}
           \end{gathered}}}

现在我们已经写出了所有规则，可以实现语言的类型检查器了。

@subsection[#:style section-title-style-numbered #:tag "s7.3.1"]{检查器}

我们需要比较类型是否相等。我们用过程 @tt{check-equal-type!} 做比较，它比较两个类
型，若二者不等则报错。@tt{check-equal-type!} 的第三个参数是一表达式，指明类型不
等的位置。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{check-equal-type!}} : @${\mathit{Type} \times \mathit{Type} \times \mathit{Exp} \to \mathit{Unspecified}}}
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (if (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp))))

@#,elem{@bold{@tt{report-unequal-types}} : @${\mathit{Type} \times \mathit{Type} \times \mathit{Exp} \to \mathit{Unspecified}}}
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error 'check-equal-type!
      "Types didn't match: ~s != ~a in~%~a"
      (type-to-external-form ty1)
      (type-to-external-form ty2)
      exp)))
]}

我们不使用 @tt{check-equal-type!} 调用的返回值，因此如同@secref{s4.2.2}中的
@tt{setref} 那样，@tt{check-equal-type!} 的执行只求效果。

过程 @tt{report-unequal-types} 用 @tt{type-to-external-form}，将类型转换为易读的
列表。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{type-to-external-form}} : @${\mathit{Type} \to \mathit{List}}}
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
        (list
          (type-to-external-form arg-type)
          '->
          (type-to-external-form result-type))))))
]}

现在，我们可以将规则转换为程序，就像处理@secref{expr}中的解释器那样。结果
如@figure-ref{fig-7.1}--@countref{fig-7.3} 所示。

@eopl-figure{
@racketblock[
@#,elem{@${\mathit{Tenv} = \mathit{Var} \to \mathit{Type}}}

@#,elem{@bold{@tt{type-of-program}} : @${\mathit{Program} \to \mathit{Type}}}
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) (type-of exp1 (init-tenv))))))

@#,elem{@bold{@tt{type-of}} : @${\mathit{Exp} \times \mathit{Tenv} \to \mathit{Type}}}
(define type-of
  (lambda (exp tenv)
    (cases expression exp
      @#,elem{@${\fbox{@tt{(type-of @${num} @${tenv}) = int}}}}
      (const-exp (num) (int-type))
      @#,elem{@${\fbox{@tt{(type-of @${var} @${tenv}) = @${tenv}(@${var})}}}}
      (var-exp (var) (apply-tenv tenv var))
      @#,elem{@${\fbox{\infer{@tt{(type-of (diff-exp @${e_1} @${e_2}) @${tenv}) = int}}{@tt{(type-of @${e_1} @${tenv}) = int} & @tt{(type-of @${e_2} @${tenv}) = int}}}}}
      (diff-exp (exp1 exp2)
        (let ((ty1 (type-of exp1 tenv))
              (ty2 (type-of exp2 tenv)))
          (check-equal-type! ty1 (int-type) exp1)
          (check-equal-type! ty2 (int-type) exp2)
          (int-type)))
      @#,elem{@${\fbox{\infer{@tt{(type-of (zero?-exp @${e_1}) @${tenv}) = bool}}{@tt{(type-of @${e_1} @${tenv}) = int}}}}}
      (zero?-exp (exp1)
        (let ((ty1 (type-of exp1 tenv)))
          (check-equal-type! ty1 (int-type) exp1)
          (bool-type)))
      @#,elem{@${\fbox{\infer{@tt{(type-of (if-exp @${e_1} @${e_2} @${e_3}) @${tenv}) = @${t}}}{\begin{gathered}@tt{(type-of @${e_1} @${tenv}) = bool} \\ @tt{(type-of @${e_2} @${tenv}) = @${t}} \\ @tt{(type-of @${e_3} @${tenv}) = @${t}}\end{gathered}}}}}
      (if-exp (exp1 exp2 exp3)
        (let ((ty1 (type-of exp1 tenv))
              (ty2 (type-of exp2 tenv))
              (ty3 (type-of exp3 tenv)))
          (check-equal-type! ty1 (bool-type) exp1)
          (check-equal-type! ty2 ty3 exp)
          ty2))
@#,exact-elem{\begin{comment}}
...)))
@#,exact-elem{\end{comment}
\smallskip}
]

@eopl-caption["fig-7.1"]{CHECKED 的 @tt{type-of}
                         @eopl-index["CHECKED"]}
}

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,elem{@${\fbox{\infer{@tt{(type-of (let-exp @${var} @${e_1} @${body}) @${tenv}) = @${t_2}}}{@tt{(type-of @${body} [@${var}=@${t_1}]@${tenv}) = @${t_2}} & @tt{(type-of @${e_1} @${tenv}) = @${t_1}}}}}}
(let-exp (var exp1 body)
  (let ((exp1-type (type-of exp1 tenv)))
    (type-of body
      (extend-tenv var exp1-type tenv))))
@#,elem{@${\fbox{\infer{@tt{(type-of (proc-exp @${var} @${t_{var}} @${body}) @${tenv}) = (@${t_{var} \to t_{res}})}}{@tt{(type-of @${body} [@${var}=@${t_{var}}]@${tenv}) = @${t_res}}}}}}
(proc-exp (var var-type body)
  (let ((result-type
          (type-of body
            (extend-tenv var var-type tenv))))
    (proc-type var-type result-type)))
@#,elem{@${\fbox{\infer{@tt{(type-of (call-exp @${rator} @${rand}) @${tenv}) = @${t_2}}}{@tt{(type-of @${rator} @${tenv}) = (@${t_1 \to t_2})} & @tt{(type-of @${rand} @${tenv}) = @${t_1}}}}}}
(call-exp (rator rand)
  (let ((rator-type (type-of rator tenv))
        (rand-type (type-of rand tenv)))
    (cases type rator-type
      (proc-type (arg-type result-type)
        (begin
          (check-equal-type! arg-type rand-type rand)
          result-type))
      (else
        (report-rator-not-a-proc-type
          rator-type rator)))))
]

@eopl-caption["fig-7.2"]{CHECKED 的 @tt{type-of}，续
              @eopl-index["CHECKED"]}

}

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,exact-elem{\smallskip
\begin{comment}}
(((
@#,exact-elem{\end{comment}}
@#,elem{@${\fbox{\infer{@tt{(type-of}\ @tt{letrec}\ t_{res}\ p\ @tt{(}var @tt{:} t_{var}@tt{)}\ @tt{=}\ e_{proc\mbox{-}body}\ @tt{in}\ e_{letrec\mbox{-}body}\ tenv@tt{)}\ @tt{=}\ t}{\begin{gathered}@tt{(type-of @${e_{proc\mbox{-}body}} [@${var}=@${t_{var}}][@${p} =(@${t_{var} \to t_{res}})]@${tenv}) = @${t_{res}}} \\ @tt{(type-of @${e_{letrec\mbox{-}body}} [@${p} = (@${t_{var} \to t_{res}})]@${tenv}) = @${t}}\end{gathered}}}}}
(letrec-exp (p-result-type p-name b-var b-var-type
              p-body letrec-body)
  (let ((tenv-for-letrec-body
          (extend-tenv p-name
            (proc-type b-var-type p-result-type)
            tenv)))
    (let ((p-body-type
            (type-of p-body
              (extend-tenv b-var b-var-type
                tenv-for-letrec-body))))
      (check-equal-type!
        p-body-type p-result-type p-body)
      (type-of letrec-body tenv-for-letrec-body)))))))
]

@eopl-caption["fig-7.3"]{CHECKED 的 @tt{type-of}，续
              @eopl-index["CHECKED"]}
}

@eopl-index[#:range-mark 'end "CHECKED"]
@exercise[#:level 2 #:tag "ex7.5"]{

扩展检查器，处理多声明 @tt{let}、多参数过程、以及多声明 @tt{letrec}。你需要添加
形如 @tt{@${t_1} * @${t_2} * @${\dots} * @${t_n} -> @${t}} 的类型来处理多参数过
程。

}

@exercise[#:level 1 #:tag "ex7.6"]{

扩展检查器，处理赋值（@secref{s4.3}）。

}

@exercise[#:level 1 #:tag "ex7.7"]{

@eopl-index[#:suffix @exer-ref-range["ex7.7"] "Conditionals"]
修改检查 @tt{if-exp} 的代码，若条件不是布尔值，则不检查其他表达式。给出一个表达
式，使新旧两个版本的检查器表现出不同的行为。

}

@exercise[#:level 2 #:tag "ex7.8"]{

给语言添加类型 @tt{pairof}。比如，当且仅当一个值是序对，且所含值类型为 @${t_1}
和 @${t_2} 时，其类型为 @tt{pairof @${t_1} * @${t_2}}。给语言添加下列生成式：

@envalign*{\mathit{Type} &::= @tt{pairof @m{\mathit{Type}} * @m{\mathit{Type}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{pair-type (ty1 ty2)}} \\[5pt]
     \mathit{Expression} &::= @tt{pair (@m{\mathit{Expression}} , @m{\mathit{Expression}})} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{pair-exp (exp1 exp2)}} \\[5pt]
     \mathit{Expression} &::= @tt{unpair @m{\mathit{Identifier}} @m{\mathit{Identifier}} = @m{\mathit{Expression}}} \\[-3pt]
       &\mathrel{\phantom{::=}} @tt{in @m{\mathit{Expression}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{unpair-exp (var1 var2 exp body)}}}

@tt{pair} 表达式生成一个序对，@tt{unpair} 表达式（同@exercise-ref{ex3.18}）将两
个变量绑定到表达式的两部分。这些变量的作用域是 @tt{body}。@tt{pair} 和
@tt{unpair} 的判类规则为：

@$${\infer{@tt{(type-of (pair-exp @${e_1} @${e_2}) @${tenv}) = pairof @${t_1} * @${t_2}}}
          {\begin{gathered}
           @tt{(type-of @${e_1} @${tenv}) = @${t_1}} \\
           @tt{(type-of @${e_2} @${tenv}) = @${t_2}} \\
           \end{gathered}}}

@$${\infer{@tt{(type-of (unpair-exp @${var_1} @${var_2} @${e_1} @${e_{body}}) @${tenv}) = @${t_{body}}}}
          {\begin{gathered}
           @tt{(type-of @${e_{pair}} @${tenv}) = (pairof @${t_1} @${t_2})} \\
           @tt{(type-of @${e_{body}} [@${var_1}=@${t_1}][@${var_2}=@${t_2}]@${tenv}) = @${t_{body}}} \\
           \end{gathered}}}

扩展 CHECKED，实现这些规则。在 @tt{type-to-external-form} 中，用列表 @tt{(pairof
@${t_1} @${t_2})} 表示序对。

}

@exercise[#:level 2 #:tag "ex7.9"]{

给语言添加类型 @tt{listof}，其操作与@exercise-ref{ex3.9} 类似。当且仅当值是列表，
且所有元素类型均为 @${t} 时，值类型为 @tt{listof @${t}}。用下列生成式扩展语言：

@envalign*{\mathit{Type} &::= @tt{listof @m{\mathit{Type}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{list-type (ty)}} \\[5pt]
     \mathit{Expression} &::= @tt{list (@m{\mathit{Expression}} @m{\{,\mathit{Expression}}\}^{*})} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{list-exp (exp1 exps)}} \\[5pt]
     \mathit{Expression} &::= @tt{cons (@m{\mathit{Expression}} , @m{\mathit{Expression}})} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{cons-exp (exp1 exp2)}} \\[5pt]
     \mathit{Expression} &::= @tt{null? (@m{\mathit{Expression}})} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{null-exp (exp1)}} \\[5pt]
     \mathit{Expression} &::= @tt{emptylist_@m{\mathit{Type}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{emptylist-exp (ty)}} \\[5pt]
}

以及四条与类型相关的规则：

@$${\infer{@tt{(type-of (list-exp @${e_1} (@${e_2} ... @${e_n})) @${tenv}) = listof @${t}}}
          {\begin{gathered}
           @tt{(type-of @${e_1} @${tenv}) = @${t}} \\
           @tt{(type-of @${e_2} @${tenv}) = @${t}} \\
           \vdots \\
           @tt{(type-of @${e_n} @${tenv}) = @${t}}
           \end{gathered}}}

@$${\infer{@tt{(type-of cons(@${e_1}, @${e_2}) @${tenv}) = listof @${t}}}
          {\begin{gathered}
           @tt{(type-of @${e_1} @${tenv}) = @${t}} \\
           @tt{(type-of @${e_2} @${tenv}) = listof @${t}}
           \end{gathered}}}

@$${\infer{@tt{(type-of null?(@${e_1}) @${tenv}) = bool}}
          {@tt{(type-of @${e_1} @${tenv}) = listof @${t}}}}

@$${@tt{(type-of emptylist[@${t}] @${tenv}) = listof @${t}}}

虽然 @tt{cons} 和 @tt{pair} 类似，它们的判类规则却完全不同。

为 @tt{car} 和 @tt{cdr} 写出类似的规则，扩展检查器，处理这些和上述表达式。
用@exercise-ref{ex7.8} 中的小技巧避免与 @tt{proc-type-exp} 的冲突。这些规则应确
保 @tt{car} 和 @tt{cdr} 应用于列表，但它们无法保证列表非空。为什么让规则确保列表
非空不合理？为什么 @tt{emptylist} 中的类型参数是必需的？

}

@exercise[#:level 2 #:tag "ex7.10"]{

扩展检查器，处理 EXPLICIT-REFS。你需要这样做：

@itemlist[

 @item{给类型系统添加类型 @tt{refto @${t}}，其中，@${t} 是任意类型。这个类型表示
 引用，指向的位置包含类型为 @${t} 的值。那么，若 @${e} 类型为 @${t}，则
 @tt{(newref @${e})} 类型为 @tt{refto @${t}}。}

 @item{给类型系统添加类型 @tt{void}。@tt{seref} 的返回值为此类型。对类型为
 @tt{void} 的值，不能进行任何操作，所以 @tt{setref} 返回什么值都不要紧。这是把类
 型作为信息隐藏机制的例子。}

 @item{写出 @tt{newref}、@tt{deref} 和 @tt{setref} 的判类规则。}

 @item{在检查器中实现这些规则。}

]

}

@exercise[#:level 2 #:tag "ex7.11"]{

扩展检查器，处理 MUTABLE-PAIRS。

}

@section[#:style section-title-style-numbered #:tag "s7.4"]{INFERRED：带有类型推导的语言}

在程序中写出类型虽然有助于设计和文档，但很耗时。另一种设计是让编译器根据变量的使
用以及程序员可能给出的信息，推断出所有变量的类型。令人惊讶的是，对设计严谨的语言，
编译器@emph{总}能推断出变量的类型。这种策略叫做@emph{类型推导}。它适用于 LETREC
这样的语言，也适用于相当大的语言。

我们从语言 CHECKED 入手研究类型推导的实例。然后，我们修改语言，令所有类型表达式
成为可选项。我们用标记 @tt{?} 替代缺失的类型表达式。因此，典型的程序看起来像是：

@nested{
@eopl-code{
@verbatim|{
letrec
 ? foo (x : ?) = if zero?(x)
                 then 1
                 else -(x, (foo -(x,1)))
in foo
}|
}

每个问号（当然，除了 @tt{zero?} 结尾那个）指出所在之处有一个待推导的类型。

}

由于类型表达式是可选的，我们可以用类型替代某些 @tt{?}，例如：

@eopl-code{
@verbatim|{
letrec
 ? even (x : int) = if zero?(x) then 1 else (odd -(x,1))
 bool odd (x : ?) = if zero?(x) then 0 else (even -(x,1))
in (odd 13)
}|
}

要定义这种语法，我们新添一个非终止符，@${\mathit{Optional\mbox{-}type}}，并修改
@tt{proc} 和 @tt{letrec} 的生成式，令其用可选类型替代类型。

@nested[#:style small]{
@envalign*{
\mathit{Optinal\mbox{-}Type} &::= @tt{?} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{no-type ()}} \\[5pt]
\mathit{Optinal\mbox{-}Type} &::= \mathit{Type} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{a-type (ty)}} \\[5pt]
        \mathit{Expression} &::= @tt{proc (@m{\mathit{Identifier : Optinal\mbox{-}Type}}) @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{proc-exp (var otype body)}} \\[5pt]
        \mathit{Expression} &::= @tt{letrec} \\[-3pt]
          &\mathrel{\phantom{::=}} \phantom{x}@tt{@m{\mathit{Optinal\mbox{-}Type}} @m{\mathit{Identifier}} (@m{\mathit{Identifier}} : @m{\mathit{Optinal\mbox{-}Type}}) = @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} @tt{in @m{\mathit{Expression}}} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{\begin{math}\begin{alignedat}{-1}
                                          &@tt{letrec-exp} \\
                                          &\phantom{xx}@tt["("]{p-result-otype p-name} \\
                                          &\phantom{xxx}@tt{b-var b-var-otype p-body} \\
                                          &\phantom{xxx}@tt{letrec-body}@tt[")"]
                                         \end{alignedat}\end{math}}
}}

排除的类型就是需要我们找出的类型。要找出它们，我们遍历抽象语法树，生成类型之间的
方程，方程中可能含有这些未知类型。然后，我们求解含有未知类型的方程。

要理解这一流程，我们需要给未知类型起名字。对每个表达式 @${e} 或绑定变量 @${var}，
设 @${t_e} 或 @${t_{var}} 表示表达式或绑定变量的类型。

对表达式抽象语法树中的每个节点，类型规则决定了类型之间必须成立的某些方程。对我们
的 PROC 语言，这些方程是：

@envalign*{
@tt{(diff-exp @m{e_1} @m{e_2})} &: t_{e_1} = @tt{int} \\
              &\mathrel{\phantom{:}} t_{e_2} = @tt{int} \\
              &\mathrel{\phantom{:}} t_{@tt{(diff-exp @m{e_1} @m{e_2})}} = @tt{int} \\[1em]
@tt{(zero?-exp @m{e_1})} &: t_{e_1} = @tt{int} \\
       &\mathrel{\phantom{:}} t_{@tt{(zero?-exp @m{e_1})}} = @tt{bool} \\[1em]
@tt{(if-exp @m{e_1} @m{e_2} @m{e_3})} &: t_{e_1} = @tt{bool} \\
                    &\mathrel{\phantom{:}} t_{e_2} = t_{@tt{(if-exp @m{e_1} @m{e_2} @m{e_3})}} \\
                    &\mathrel{\phantom{:}} t_{e_3} = t_{@tt{(if-exp @m{e_1} @m{e_2} @m{e_3})}} \\[1em]
@tt{(proc-exp @m{var} @m{body})} &: t_{@tt{(proc-exp @m{var} @m{body})}} = (t_{var} \to t_{body}) \\[1em]
@tt{(call-exp @m{rator} @m{rand})} &: t_{rator} = (t_{rand} \to t_{@tt{(call-exp @m{rator} @m{rand})}})
}

@itemlist[

 @item{第一条规则是说，@tt{diff-exp} 的参数和结果类型均为 @tt{int}。}

 @item{第二条规则是说，@tt{zero?-exp} 的参数为 @tt{int}，结果为 @tt{bool}。}

 @item{第三条规则是说，@tt{if} 表达式中的条件类型必须为 @tt{bool}，两个分支的类
 型必须与整个 @tt{if} 表达式的类型相同。}

 @item{第四条规则是说，@tt{proc} 表达式的类型是一过程，其参数类型为绑定变量的类
 型，其结果类型为主体的类型。}

 @item{第五条规则是说，在过程调用中，操作符类型必须是一过程，其参数类型必须与操
 作数相同，其结果类型与整个调用表达式的类型相同。}

]

要推导表达式的类型，我们为所有子表达式和绑定变量分配一个类型变量，给出所有子表达
式的约束条件，然后求解得出的方程。要理解这一流程，我们来推导几个示例表达式的类型。

我们从表达式 @tt{proc(f) proc(x) -((f 3),(f x))} 开始。我们首先做一张表，涵盖这
个表达式中的所有绑定变量、@tt{proc} 表达式、@tt{if} 表达式和过程调用，并给它们分
别分配一个变量。

@tabular[#:style 'inset #:sep @hspace[4] #:row-properties '(bottom-border ())
         (list (list @bold{表达式}                         @bold{类型变量})
               (list @tt{f}                                @${t_f})
               (list @tt{x}                                @${t_x})
               (list @tt{proc(f)proc(x)-((f 3),(f x))}     @${t_0})
               (list @tt{proc(x)-((f 3),(f x))}            @${t_1})
               (list @tt{-((f 3),(f x))}                   @${t_2})
               (list @tt{(f 3)}                            @${t_3})
               (list @tt{(f x)}                            @${t_4}))]

现在，对每个复杂表达式，都可以根据上述规则写出一个类型方程。

@tabular[#:style 'inset #:sep @hspace[4] #:row-properties '(bottom-border ())
         (list (list @bold{表达式}                         @bold{方程})
               (list @tt{proc(f)proc(x)-((f 3),(f x))}     @${\text{1.\quad}t_0 = t_f \to t_1})
               (list @tt{proc(x)-((f 3),(f x))}            @${\text{2.\quad}t_1 = t_x \to t_2})
               (list @tt{-((f 3),(f x))}                   @${\text{3.\quad}t_3 = @tt{int}})
               (list ""                                    @${\text{4.\quad}t_4 = @tt{int}})
               (list ""                                    @${\text{5.\quad}t_2 = @tt{int}})
               (list @tt{(f 3)}                            @${\text{6.\quad}t_f = @tt{int} \to t_3})
               (list @tt{(f x)}                            @${\text{7.\quad}t_f = t_x \to t_4}))]

@itemlist[

 @item{方程 1 是说，整个表达式生成一个过程，其参数类型为 @${t_f}，结果类型与
 @tt{proc(x)-((f 3),(f x))} 相同。}

 @item{方程 2 是说，@tt{proc(x)-((f 3),(f x))} 产生一过程，其参数类型为 @${t_x}，
 结果类型与 @tt{-((f 3),(f x))} 相同。}

 @item{方程 3-5 是说，减法操作 @tt{-((f 3),(f x))} 的参数和结果都是整数。}

 @item{方程 6 是说，@tt{f} 期望的参数类型为 @tt{int}，返回值类型与 @tt{(f 3)} 相
 同。}

 @item{类似地，方程 7 是说，@tt{f} 期望的参数类型与 @tt{x} 相同，返回值类型与
 @tt{(f x)} 相同。}

]

只要满足如下方程，@${t_f}、@${t_x}、@${t_0}、@${t_1}、@${t_2}、@${t_3} 和
@${t_4} 的解可以是任意值：

@eopl-code{
@verbatim|{
|@${t_0 = t_f \to t_1}
|@${t_1 = t_x \to t_2}
|@${t_3 = @tt{int}}
|@${t_4 = @tt{int}}
|@${t_2 = @tt{int}}
|@${t_f = @tt{int} \to t_3}
|@${t_f = t_x \to t_4}
}|
}

我们的目标是找出变量的值，使所有方程成立。我们可以把这样的解表示为一组方程，方程
的左边都是变量。我们称这组方程为一组@term["substitution"]{代换式}，称代换式方程
左边的变量@term["bound"]{绑定} 于代换式。
@eopl-index["Binding" (eopl-index-entry "of type variables" "typevariables")]

我们可以按部就班地求解这些方程。这一过程叫做@term["unification"]{合一}。

我们把计算分为两种状态，一种是待求解的方程，一种是已发现的代换式。最开始，所有方
程都待求解，没有一个代换式。

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_0 = t_f \to t_1})
           (list @${t_1 = t_x \to t_2})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to t_3})
           (list @${t_f = t_x \to t_4}))]
   @tabular[#:row-properties '(bottom-border)
     (list (list @bold{代换式}))]))]

我们依次考虑每个方程。如果方程左边是一个变量，我们将其移到代换式组中。

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_1 = t_x \to t_2})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to t_3})
           (list @${t_f = t_x \to t_4}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to t_1}))]))]

但是，这样做可能会改变代换式组。例如，下一个方程给出了 @${t_1} 的值。代换式
@${t_0} 右边的值包含 @${t_1}，我们要在其中使用这一信息。所以，我们把代换式右边出
现的每个 @${t_1} 换掉。那么，我们有：

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to t_3})
           (list @${t_f = t_x \to t_4}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to (t_x \to t_2)})
           (list @${t_1 = t_x \to t_2}))]))]

如果方程右边是一变量，我们调换两侧，然后仍照上面操作。我们可以按照这种方式，继续
处理下面的的三个方程。

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to t_3})
           (list @${t_f = t_x \to t_4}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to (t_x \to t_2)})
           (list @${t_1 = t_x \to t_2})
           (list @${t_3 = @tt{int}}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to t_3})
           (list @${t_f = t_x \to t_4}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to (t_x \to t_2)})
           (list @${t_1 = t_x \to t_2})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_f = @tt{int} \to t_3})
           (list @${t_f = t_x \to t_4}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to (t_x \to @tt{int})})
           (list @${t_1 = t_x \to @tt{int}})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}}))]))]

现在，下一个要处理的方程含有 @${t_3}，已经在代换式组中绑定到 @tt{int}。所以，我
们用 @tt{int} 替换方程中的 @${t_3}。方程中的其他类型变量也这样处理。我们称之为对
方程@term["apply"]{应用} 代换式。

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_f = @tt{int} \to @tt{int}})
           (list @${t_f = t_x \to t_4}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to (t_x \to @tt{int})})
           (list @${t_1 = t_x \to @tt{int}})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}}))]))]

我们把得到的方程移入代换式组中，并更新需要更新的代换式。

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_f = t_x \to t_4}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = (@tt{int} \to @tt{int}) \to (t_x \to @tt{int})})
           (list @${t_1 = t_x \to @tt{int}})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to @tt{int}}))]))]

下一个方程，@${t_f = t_x \to t_4}，包含 @${t_f} 和 @${t_4}，均已绑定于代换式，所
以我们对该方程应用代换式，得：

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${@tt{int} \to @tt{int} = t_x \to @tt{int}}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = (@tt{int} \to @tt{int}) \to (t_x \to @tt{int})})
           (list @${t_1 = t_x \to @tt{int}})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to @tt{int}}))]))]

如果方程两边都不是变量，我们可以将其化简，得到两个方程：

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${@tt{int} = t_x})
           (list @${@tt{int} = @tt{int}}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = (@tt{int} \to @tt{int}) \to (t_x \to @tt{int})})
           (list @${t_1 = t_x \to @tt{int}})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to @tt{int}}))]))]

我们还是照常处理：像之前那样，对调第一个方程的两侧，加入代换式组，更新代换式组。

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${@tt{int} = @tt{int}}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = (@tt{int} \to @tt{int}) \to (@tt{int} \to @tt{int})})
           (list @${t_1 = @tt{int} \to @tt{int}})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to @tt{int}})
           (list @${t_x = @tt{int}}))]))]

最后一个方程 @${@tt{int} = @tt{int}} 总是成立，所以可以丢弃。

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border)
     (list (list @bold{方程}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = (@tt{int} \to @tt{int}) \to (@tt{int} \to @tt{int})})
           (list @${t_1 = @tt{int} \to @tt{int}})
           (list @${t_3 = @tt{int}})
           (list @${t_4 = @tt{int}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = @tt{int} \to @tt{int}})
           (list @${t_x = @tt{int}}))]))]

没有方程了，所以我们已完成。从这个计算，我们得出结论：原表达式 @tt{proc (f) proc
(x) -((f 3),(f x))} 的类型应为：

@nested[#:style 'inset]{
@${((@tt{int} \to @tt{int}) \to (@tt{int} \to @tt{int}))}
}

这是合理的：@tt{f} 的第一个参数必须是一个 @tt{int}，因为它接受 @tt{3} 做参数。它
必须生成一个 @tt{int}，因为它的值用作减法操作的参数。@tt{x} 也必须是一个
@tt{int}，因为它也用作 @tt{f} 的参数。

我们再看另一个例子：@tt{proc (f) (f 11)}。我们仍从分配类型变量开始。

@tabular[#:style 'inset #:sep @hspace[4] #:row-properties '(bottom-border ())
         (list (list @bold{表达式}         @bold{类型变量})
               (list @tt{f}                @${t_f})
               (list @tt{proc (f) (f 11)}  @${t_0})
               (list @tt{(f 11)}           @${t_1}))]

接下来我们写出方程：

@tabular[#:style 'inset #:sep @hspace[4] #:row-properties '(bottom-border ())
         (list (list @bold{表达式}          @bold{方程})
               (list @tt{proc(f)(f 11)}     @${t_0 = t_f \to t_1})
               (list @tt{(f 11)}            @${t_f = @tt{int} \to t_1}))]

然后求解：

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_0 = t_f \to t_1})
           (list @${t_f = @tt{int} \to t_1}))]
   @tabular[#:row-properties '(bottom-border)
     (list (list @bold{代换式}))]))]

@tabular[#:style 'inset #:sep @hspace[8]
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_f = @tt{int} \to t_1}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to t_1}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border)
     (list (list @bold{方程}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = (@tt{int} \to t_1) \to t_1})
           (list @${t_f = @tt{int} \to t_1}))]))]

这意味着可以给 @tt{proc (f) (f 11)} 赋予类型 @${(@tt{int} \to t_1) \to t_1}，其
中 @${t_1} 是任何类型。这也是合理的：我们可以推出 @tt{f} 必须取一 @tt{int} 参数，
但对 @tt{f} 结果的类型一无所知。而且，对任何 @${t_1}，这个代码都切实可行，只要
@tt{f} 取一 @tt{int} 参数，返回一类型为 @${t_1} 的值。我们称@${t_1} 对它
是@term["polymorphic"]{多态} 的。

再来看一个例子。考虑 @tt{if x then -(x,1) else 0}。我们还是给每个不是常数的子表
达式分配一个类型变量。

@tabular[#:style 'inset #:sep @hspace[4] #:row-properties '(bottom-border ())
         (list (list @bold{表达式}                 @bold{类型变量})
               (list @tt{x}                        @${t_x})
               (list @tt{if x then -(x,1) else 0}  @${t_0})
               (list @tt{-(x,1)}                   @${t_1}))]

然后给出方程：

@tabular[#:style 'inset #:sep @hspace[4] #:row-properties '(bottom-border ())
         (list (list @bold{表达式}                @bold{方程})
               (list @tt{if x then -(x,1) else 0} @${t_x = @tt{bool}})
               (list ""                           @${t_1 = t_0})
               (list ""                           @${@tt{int} = t_0})
               (list @tt{-(x,1)}                  @${t_x = @tt{int}})
               (list ""                           @${t_1 = @tt{int}}))]

像之前那样处理这些方程，我们有：

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_x = @tt{bool}})
           (list @${t_1 = t_0})
           (list @${@tt{int} = t_0})
           (list @${t_x = @tt{int}})
           (list @${t_1 = @tt{int}}))]
   @tabular[#:row-properties '(bottom-border)
     (list (list @bold{代换式}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_1 = t_0})
           (list @${@tt{int} = t_0})
           (list @${t_x = @tt{int}})
           (list @${t_1 = @tt{int}}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_x = @tt{bool}}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${@tt{int} = t_0})
           (list @${t_x = @tt{int}})
           (list @${t_1 = @tt{int}}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_x = @tt{bool}})
           (list @${t_1 = t_0}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_0 = @tt{int}})
           (list @${t_x = @tt{int}})
           (list @${t_1 = @tt{int}}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_x = @tt{bool}})
           (list @${t_1 = t_0}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_x = @tt{int}})
           (list @${t_1 = @tt{int}}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_x = @tt{bool}})
           (list @${t_1 = t_0})
           (list @${t_0 = @tt{int}}))]))]

由于 @${t_x} 已经绑定于代换式组，我们对下一方程应用代换，得：

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${@tt{bool} = @tt{int}})
           (list @${t_1 = @tt{int}}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_x = @tt{bool}})
           (list @${t_1 = t_0})
           (list @${t_0 = @tt{int}}))]))]

怎么回事？从这些方程，我们推出 @tt{bool = int}。所以在这些方程中的解中，均有
@tt{bool = int}。但 @tt{bool} 和 @tt{int} 不可能相等。因此，这些方程无解，也就无
法赋予这个表达式类型。这是合理的，因为表达式 @tt{if x then -(x,1) else 0} 中，
@tt{x} 同时用作布尔值和整数值，而在我们的类型系统中，这是不允许的。

再来看一个例子。考虑 @tt{proc (f) zero?((f f))}。我们仍像之前那样处理。

@tabular[#:style 'inset #:sep @hspace[4] #:row-properties '(bottom-border ())
         (list (list @bold{表达式}                 @bold{类型变量})
               (list @tt{proc (f) zero?((f f))}    @${t_0})
               (list @tt{f}                        @${t_f})
               (list @tt{zero?((f f))}             @${t_1})
               (list @tt{(f f)}                    @${t_2}))]

@tabular[#:style 'inset #:sep @hspace[4] #:row-properties '(bottom-border ())
         (list (list @bold{表达式}                @bold{方程})
               (list @tt{proc (f) zero?((f f))}   @${t_0 = t_f \to t_1})
               (list @tt{zero?((f f))}            @${t_1 = @tt{bool}})
               (list ""                           @${t_2 = @tt{int}})
               (list @tt{(f f)}                   @${t_f = t_f \to t_2}))]

然后，我们仍像之前那样求解：

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_0 = t_f \to t_1})
           (list @${t_1 = @tt{bool}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = t_f \to t_2}))]
   @tabular[#:row-properties '(bottom-border)
     (list (list @bold{代换式}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_1 = @tt{bool}})
           (list @${t_2 = @tt{int}})
           (list @${t_f = t_f \to t_2}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to t_1}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_2 = @tt{int}})
           (list @${t_f = t_f \to t_2}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to @tt{bool}})
           (list @${t_1 = @tt{bool}}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_f = t_f \to t_2}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to @tt{bool}})
           (list @${t_1 = @tt{bool}})
           (list @${t_2 = @tt{int}}))]))]

@tabular[#:style 'inset #:sep @hspace[8] #:column-properties '(baseline baseline)
(list
  (list
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{方程})
           (list @${t_f = t_f \to @tt{int}}))]
   @tabular[#:row-properties '(bottom-border ())
     (list (list @bold{代换式})
           (list @${t_0 = t_f \to @tt{bool}})
           (list @${t_1 = @tt{bool}})
           (list @${t_2 = @tt{int}}))]))]

问题来了。我们推导出 @${t_f = t_f \to @tt{int}}。但没有一种类型具有这种性质，因
为这个方程的右边总是比左边大：如果 @${t_f} 的语法树包含 @${k} 个节点，那么方程右
边总是包含 @${k+2} 个节点。

所以，如果我们推导的方程形如 @${tv = t}，且类型变量 @${tv} 出现在类型 @${t} 中，
我们只能得出结论：原方程无解。这个附加条件叫做@term["occurrence check"]{验存}。

这个条件也意味着我们生成的代换式应满足如下不变式：

@big-bracket[#:title "无存不变式"]{
代换式中绑定的变量不应出现在任何代换式的右边。
}

我们解方程的代码极度依赖这个不变式。

@exercise[#:level 1 #:tag "ex7.12"]{

用本节的方法，推导@exercise-ref{ex7.1} 中每个表达式的类型，或者判定表达式没有类
型。就像本节的其他练习那样，假设每个绑定变量都有对应的 @tt{?}。

}


@exercise[#:level 1 #:tag "ex7.13"]{

写出 @tt{let} 表达式的类型推导规则。用你的规则，推导下列各表达式的类型，或者判定
表达式无类型。

@itemlist[#:style 'ordered

 @item{@tt{let x = 4 in (x 3)}}

 @item{@tt{let f = proc (z) z in proc (x) -((f x), 1)}}

 @item{@tt{let p = zero?(1) in if p then 88 else 99}}

 @item{@tt{let p = proc (z) z in if p then 88 else 99}}
 ]

}

@exercise[#:level 1 #:tag "ex7.14"]{

下面的表达式有何问题？

@eopl-code{
@verbatim|{
letrec
 ? even(odd : ?) =
    proc (x : ?)
     if zero?(x) then 1 else (odd -(x,1))
in letrec
    ? odd(x : bool) =
       if zero?(x) then 0 else ((even odd) -(x,1))
    in (odd 13)
}|
}

}

@exercise[#:level 2 #:tag "ex7.15"]{

写出 @tt{letrec} 表达式的类型推导规则。你的规则应能处理多声明的 @tt{letrec}。用
你的规则推导下列每个表达式的类型，或者判定表达式无类型。

@itemlist[#:style 'ordered

 @item{
  @verbatim{letrec ? f (x : ?)
                    = if zero?(x) then 0 else -((f -(x,1)), -2)
            in f}}

  @verbatim{letrec ? even (x : ?)
                      = if zero?(x) then 1 else (odd -(x,1))
                   ? odd (x : ?)
                      = if zero?(x) then 0 else (even -(x,1))
            in (odd 13)}

  @verbatim{letrec ? even (odd : ?)
                      = proc (x) if zero?(x)
                                 then 1
                                 else (odd -(x,1))
            in letrec ? odd (x : ?) =
                         if zero?(x)
                         then 0
                         else ((even odd) -(x,1))
               in (odd 13)}
 ]

}

@exercise[#:level 3 #:tag "ex7.16"]{

修改 INFERRED 的语法，排除缺失类型，不再用 @tt{?} 做标记。

}

@subsection[#:style section-title-style-numbered #:tag "s7.4.1"]{代换式}

我们按自底向上的方式实现。我们首先来考虑代换式。

我们将类型变量表示为数据类型 @tt{type} 的新变体。这里用到的技术和@secref{s3.7}中
处理词法地址的相同。我们给语法添加生成式：

@nested[#:style small]{
@envalign*{\mathit{Type} &::= @tt{%tvar-type} \ \mathit{Number} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{tvar-type (serial-number)}}}}

我们把这些扩展后的类型称为@term["type expression"]{类型表达式}。类型表达式的基本
操作是用类型代换类型变量，由 @tt{apply-one-subst}定义。@tt{(apply-one-subst
@${t_0} @${tv} @${t_1})} 将 @${t_0} 中出现的每个@${tv} 代换为 @${t_1}，返回代换
后的表达式。有时，这写作 @tt{@${t_0}[@${tv=t_1}]}。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{apply-one-subst}} : @${\mathit{Type} \times \mathit{Tvar} \times \mathit{Type} \to \mathit{Type}}}
(define apply-one-subst
  (lambda (ty0 tvar ty1)
    (cases type ty0
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (arg-type result-type)
        (proc-type
          (apply-one-subst arg-type tvar ty1)
          (apply-one-subst result-type tvar ty1)))
      (tvar-type (sn)
        (if (equal? ty0 tvar) ty1 ty0)))))
]}

这个过程用来代换单个类型变量。它不能像上节中描述的那样处理所有代换。

代换式组是一个方程列表，方程两边分别为类型变量和类型。该列表也可视为类型变量到类
型的函数。当且仅当类型变量出现于代换式组中某个方程的左侧时，我们说该变量@emph{绑
定}于代换式。
@eopl-index["Binding" (eopl-index-entry "of type variables" "typevariables")]

我们用序对 @tt{(类型变量 . 类型)} 的列表表示代换式组。代换式组的必要观测器是
@tt{apply-subst-to-type}。它遍历类型 @${t}，把每个类型变量替换为代换式组
@${\sigma} 中的绑定。如果一个变量未绑定于代换式，那么保持不变。我们用
@${t\sigma} 表示得到的类型。

这一实现用 Scheme 过程 @tt{assoc} 在代换式组中查找类型变量。若给定类型是列表中某
个序对的首项，@tt{assoc} 返回对应的（类型变量，类型）序对，否则返回 @tt{#f}。我
们将它写出来：

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{apply-subst-to-type}} : @${\mathit{Type} \times \mathit{Subst} \to \mathit{Type}}}
(define apply-subst-to-type
  (lambda (ty subst)
    (cases type ty
      (int-type () (int-type))
      (bool-type () (bool-type))
      (proc-type (t1 t2)
        (proc-type
          (apply-subst-to-type t1 subst)
          (apply-subst-to-type t2 subst)))
      (tvar-type (sn)
        (let ((tmp (assoc ty subst)))
          (if tmp
            (cdr tmp)
            ty))))))
]}

代换式组的构造器有 @tt{empty-subst} 和 @tt{extend-subst}。@tt{(empty-subst)} 生
成空代换式组的表示。@tt{(extend-subst @${\sigma} @${tv} @${t})} 取一代换式组
@${\sigma}，像上节那样给它添加方程 @${tv = t}。这个操作分两步：首先把代换式组中
所有方程右边的 @${tv} 替换为 @${t}，然后把方程 @${tv = t} 添加到列表中。用公式表
示为：

@$${
\begin{pmatrix}
 tv_1 = t_1 \\
 \vdots \\
 tv_n = t_n
\end{pmatrix}[tv = t] =
\begin{pmatrix}
 tv = t \\
 tv_1 = t_1[tv = t] \\
 \vdots \\
 tv_n = t_n[tv = t]
\end{pmatrix}}

该定义具有如下性质：对任意类型 @${t}，

@$${(t\sigma)[tv = t'] = t(\sigma[tv = t'])}

@tt{extend-subst} 的实现依照上式。它把 @${\sigma_0} 所有绑定中的 @${t_0} 代换为
@${tv_0}。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{empty-subst}} : @${\mathit{()} \to \mathit{Subst}}}
(define empty-subst (lambda () '()))

@#,elem{@bold{@tt{extend-subst}} : @${\mathit{Subst} \times \mathit{Tvar} \times \mathit{Type} \to \mathit{Subst}}}
@#,elem{@bold{用法} : @tt{tvar 尚未绑定于 subst。}}
(define extend-subst
  (lambda (subst tvar ty)
    (cons
      (cons tvar ty)
      (map
        (lambda (p)
          (let ((oldlhs (car p))
                (oldrhs (cdr p)))
            (cons
              oldlhs
              (apply-one-subst oldrhs tvar ty))))
        subst))))
]}

这一实现保持无存不变式，但既不依赖它，也不强制它。那是下一节中合一器的工作。


@exercise[#:level 2 #:tag "ex7.17"]{

在我们的实现中，当 @${\sigma} 很大时，@tt{extend-subst} 要做大量工作。实现另一种
表示，则 @tt{extend-subst} 变成：

@eopl-code{
@racketblock[
(define extend-subst
  (lambda (subst tvar ty)
    (cons (cons tvar ty) subst)))
]
}

其余工作移至 @tt{apply-subst-to-type}，而性质 @${t(\sigma[tv = t']) =
(t\sigma)[tv = t']} 仍然满足。这样定义 @tt{extend-subst} 还需要无存不变式吗？

}

@exercise[#:level 2 #:tag "ex7.18"]{

修改前一道练习中的实现，则对任意类型变量，@tt{apply-subst-to-type} 最多只需计算
一次代换。

}

@subsection[#:style section-title-style-numbered #:tag "s7.4.2"]{合一器}

合一器的主要过程是 @tt{unifier}。合一器执行上述推导流程中的这一的步骤：取两个类
型 @tt{t_1} 和 @tt{t_2}，满足无存不变式的代换式组 @${\sigma}，以及表达式
@tt{exp}，将 @${t_1 = t_2} 添加到 @${\sigma}，返回得到的代换式组。这是合并
@${t_1\sigma} 和 @${t_2\sigma} 后所得的最小 @${\sigma} 扩展。这组代换式仍满足无
存不变式。若添加 @${t_1 = t_2} 导致矛盾，或者违反了无存不变式，那么合一器报错，
指出错误所在的表达式 @tt{exp}。这通常是得出方程 @${t_1 = t_2} 的表达式。

这个算法用 @tt{cases} 来写十分不便，所以我们改用类型的谓词和提取器。算法
如@figure-ref{fig-7.4} 所示，其流程如下：

@itemlist[

 @item{首先，像上面那样，我们对类型 @${t_1} 和 @${t_2} 分别应用代换式。}

 @item{如果结果类型相同，我们立即返回。这一步对应上面的删除简单方程。}

 @item{如果 @tt{ty1} 为未知类型，那么无存不变式告诉我们，它未绑定于代换式。由于
 它未绑定，我们尝试把 @${t_1 = t_2} 添加到代换式组。但我们要验存，以保证无存不变
 式成立。当且仅当类型变量 @${tv} 不在 @${t} 中时，调用 @tt{(no-occurrence?
 @${tv} @${t})} 返回 @tt{#t}（@figure-ref{fig-7.5}）。}

 @item{如果 @${t_2} 为未知类型，则对调 @${t_1} 和 @${t_2}，也照这样处理。}

 @item{如果 @${t_1} 和 @${t_2} 都不是类型变量，那么我们再做进一步分析。

 如果它们都是 @tt{proc} 类型，那么我们化简方程，在两个参数类型之间建立方程，得到
 一组代换式，然后用这组代换式在结果类型之间建立方程。

 否则，@${t_1} 和 @${t_2} 中一个是 @tt{int}，另一个是 @tt{bool}，或一个是
 @tt{proc}，另一个是 @tt{int} 或 @tt{bool}。不管是哪种情况，方程都无解，引发报错。}

]

从另一种角度来思考这些有助于理解。代换式组是一个@emph{存储器}，未知类型是指向存
储器中某位置的@emph{引用}。@tt{unifier} 把 @tt{ty1 = ty2} 添加到存储器中，得到一
个新的存储器。

最后，我们必须验存。直接递归处理类型即可，如@figure-ref{fig-7.5} 所示。

@exercise[#:level 1 #:tag "ex7.19"]{

我们说：@exact-elem{“}如果@tt{ty1}为未知类型，那么无存不变式告诉我们，它未绑定
于代换式。@exact-elem{”}详细解释为什么如此。

}

@exercise[#:level 2 #:tag "ex7.20"]{

修改合一器，不是对合一器的实参，而是只对类型变量调用 @tt{apply- subst-to-type}。

}

@exercise[#:level 2 #:tag "ex7.21"]{

我们说代换式组就像存储器。用@exercise-ref{ex7.17} 中的代换式组表示实现合一器，用
全局 Scheme 变量记录代换式组，就像@figure-ref{fig-4.1} 和 @countref{fig-4.2} 那
样。

}

@exercise[#:level 2 #:tag "ex7.22"]{

优化前一道练习的实现，在常数时间内获取类型变量的绑定。

}

@eopl-figure[#:position "!t"]{
@racketblock[
@#,elem{@bold{@tt{unifier}} : @${\mathit{Type} \times \mathit{Type} \times \mathit{Subst} \times \mathit{Exp} \to \mathit{Subst}}}
(define unifier
  (lambda (ty1 ty2 subst exp)
    (let ((ty1 (apply-subst-to-type ty1 subst))
           (ty2 (apply-subst-to-type ty2 subst)))
      (cond
        ((equal? ty1 ty2) subst)
        ((tvar-type? ty1)
          (if (no-occurrence? ty1 ty2)
            (extend-subst subst ty1 ty2)
            (report-no-occurrence-violation ty1 ty2 exp)))
        ((tvar-type? ty2)
          (if (no-occurrence? ty2 ty1)
            (extend-subst subst ty2 ty1)
            (report-no-occurrence-violation ty2 ty1 exp)))
        ((and (proc-type? ty1) (proc-type? ty2))
          (let ((subst (unifier
                         (proc-type->arg-type ty1)
                         (proc-type->arg-type ty2)
                         subst exp)))
            (let ((subst (unifier
                           (proc-type->result-type ty1)
                           (proc-type->result-type ty2)
                           subst exp)))
              subst)))
        (else (report-unification-failure ty1 ty2 exp))))))
]

@eopl-caption["fig-7.4"]{合一器}
}

@eopl-figure{
@racketblock[
@#,elem{@bold{@tt{no-occurrence?}} : @${\mathit{Tvar} \times \mathit{Type} \to \mathit{Bool}}}
(define no-occurrence?
  (lambda (tvar ty)
    (cases type ty
      (int-type () #t)
      (bool-type () #t)
      (proc-type (arg-type result-type)
        (and
          (no-occurrence? tvar arg-type)
          (no-occurrence? tvar result-type)))
      (tvar-type (serial-number) (not (equal? tvar ty))))))
]

@eopl-caption["fig-7.5"]{验存}
}

@subsection[#:style section-title-style-numbered #:tag "s7.4.3"]{找出表达式的类型}

我们用 @tt{otype->type} 为每个 @tt{?} 定义一个新类型变量，把可选类型转换为未知类
型。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{otype->type}} : @${\mathit{OptionalType} \to \mathit{Type}}}
(define otype->type
  (lambda (otype)
    (cases optional-type otype
      (no-type () (fresh-tvar-type))
      (a-type (ty) ty))))

@#,elem{@bold{@tt{fresh-tvar-type}} : @${\mathit{()} \to \mathit{Type}}}
(define fresh-tvar-type
  (let ((sn 0))
    (lambda ()
      (set! sn (+ sn 1))
      (tvar-type sn))))
]}

把类型转换为外在表示时，我们用包含序号的符号表示类型变量。

@eopl-code{
@racketblock[
@#,elem{@bold{@tt{type-to-external-form}} : @${\mathit{Type} \to \mathit{List}}}
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () 'int)
      (bool-type () 'bool)
      (proc-type (arg-type result-type)
        (list
          (type-to-external-form arg-type)
          '->
          (type-to-external-form result-type)))
      (tvar-type (serial-number)
        (string->symbol
          (string-append
            "ty"
            (number->string serial-number)))))))
]}

现在我们可以写 @tt{type-of} 了。它取一表达式，一个将程序变量映射到类型表达式的类
型环境，和一个满足无存不变式的代换式组，返回一个类型和满足无存不变式的新代换式组。

类型环境将各类型表达式与程序变量对应起来。代换式组解释了类型表达式中每个类型变量
的含义。我们把代换式组比作@emph{存储器}，把类型变量比作存储器@emph{引用}。因此，
@tt{type-of} 返回两个值：一个类型表达式，和一个解释表达式中类型变量的代换式组。
像@exercise-ref{ex4.12} 那样，我们在实现时新定义一种包含两个值的数据类型，用作返
回值。

@tt{type-of} 的定义如@figure-ref{fig-7.6}--@countref{fig-7.8} 所示。对每个表达式，
我们递归处理子表达式，一路传递代换式组参数中现有的解。然后，我们根据规范，为当前
表达式建立方程，调用 @tt{unifier}，在代换式组中记录这些。

因为多态的缘故，测试推导器比测试之前的解释器稍微麻烦。例如，如果给推导器输入
@tt{proc (x) x}，它给出的外在表示可能是 @tt{(tvar1 -> tvar1)}、@tt{(tvar2 ->
tvar2)} 或 @tt{(tvar3 -> tvar3)}，等等。每次调用推导器结果都可能不同，所以我们写
测试项时不能直接使用它们，否则就无法比较推导出的类型和正确类型。我们需要接受上述
所有可能，但拒绝 @tt{(tvar3 -> tvar4)} 或是 @tt{(int -> tvar17)}。

要比较两种类型的外在表示，我们统一未知类型的名字，遍历每个外在表示，给类型变量重
新编号，使之从 @tt{ty1} 开始。然后，我们就能用 @tt{equal?} 比较重新编号的类型
（@figure-ref{fig-7.10}--@countref{fig-7.11}）。

要逐个命名所有未知变量，我们用 @tt{canonical-subst} 生成代换式组。我们用
@tt{table}做累加器，即可直接递归。@tt{table} 的长度告诉我们已找出多少个不同的未
知类型，我们可以用其长度给@exact-elem{“}下一个@exact-elem{”}@tt{ty}符号编号。
这和我们在@figure-ref{fig-4.1} 中使用的 @tt{length} 类似。

@eopl-figure{
@racketblock[
@#,elem{@${\mathit{Answer} = \mathit{Type} \to \mathit{Subst}}}

(define-datatype answer answer?
  (an-answer
    (ty type?)
    (subst substitution?)))

@#,elem{@bold{@tt{type-of-program}} : @${\mathit{Program} \to \mathit{Type}}}
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
        (cases answer (type-of exp1
                        (init-tenv) (empty-subst))
          (an-answer (ty subst)
            (apply-subst-to-type ty subst)))))))

@#,elem{@bold{@tt{type-of}} : @${\mathit{Exp} \times \mathit{Tenv} \times \mathit{Subst} \to \mathit{Answer}}}
(define type-of
  (lambda (exp tenv subst)
    (cases expression exp
      (const-exp (num) (an-answer (int-type) subst))

      @#,elem{@${\fbox{\begin{math}\begin{alignedat}{-1}@tt{(zero?-exp @${e_1})} &: @${t_{e_1}} = @tt{int} \\ &\mathrel{\phantom{:}} @${t_{@tt{(zero?-exp @${e_1})}}} = @tt{bool}\end{alignedat}\end{math}}}}
      (zero?-exp (exp1)
        (cases answer (type-of exp1 tenv subst)
          (an-answer (ty1 subst1)
            (let ((subst2
                    (unifier ty1 (int-type) subst1 exp)))
              (an-answer (bool-type) subst2)))))
@#,exact-elem{\begin{comment}}
...)))
@#,exact-elem{\end{comment}
\smallskip}
]

@eopl-caption["fig-7.6"]{INFERRED 的 @tt{type-of}，第 1 部分}
}

@eopl-figure{
@racketblock[
@#,elem{@${\fbox{\begin{math}\begin{alignedat}{-1}@tt{(diff-exp @${e_1} @${e_2})} &: @${t_{e_1}} = @tt{int} \\ &\mathrel{\phantom{:}} @${t_{e_2}} = @tt{int} \\ &\mathrel{\phantom{:}} @${t_{@tt{(diff-exp @${e_1} @${e_2})}}} = @tt{int}\end{alignedat}\end{math}}}}
(diff-exp (exp1 exp2)
  (cases answer (type-of exp1 tenv subst)
    (an-answer (ty1 subst1)
      (let ((subst1
              (unifier ty1 (int-type) subst1 exp1)))
        (cases answer (type-of exp2 tenv subst1)
          (an-answer (ty2 subst2)
            (let ((subst2
                    (unifier ty2 (int-type)
                      subst2 exp2)))
              (an-answer (int-type) subst2))))))))

@#,elem{@${\fbox{\begin{math}\begin{alignedat}{-1}@tt{(if-exp @${e_1} @${e_2} @${e_3})} &: @${t_{e_1}} = @tt{bool} \\ &\mathrel{\phantom{:}} @${t_{e_2}} = @${t_{@tt{(if-exp @${e_1} @${e_2} @${e_3})}}} \\ &\mathrel{\phantom{:}} @${t_{e_3}} = @${t_{@tt{(if-exp @${e_1} @${e_2} @${e_3})}}}\end{alignedat}\end{math}}}}
(if-exp (exp1 exp2 exp3)
  (cases answer (type-of exp1 tenv subst)
    (an-answer (ty1 subst)
      (let ((subst
              (unifier ty1 (bool-type) subst exp1)))
        (cases answer (type-of exp2 tenv subst)
          (an-answer (ty2 subst)
            (cases answer (type-of exp3 tenv subst)
              (an-answer (ty3 subst)
                (let ((subst
                        (unifier ty2 ty3 subst exp)))
                  (an-answer ty2 subst))))))))))
(var-exp (var)
  (an-answer (apply-tenv tenv var) subst))
(let-exp (var exp1 body)
  (cases answer (type-of exp1 tenv subst)
    (an-answer (exp1-type subst)
      (type-of body
        (extend-tenv var exp1-type tenv)
        subst))))
]

@eopl-caption["fig-7.7"]{INFERRED 的 @tt{type-of}，第 2 部分}
}

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,elem{@${\fbox{@tt{(proc-exp @${var} @${body})} : @${t_{@tt{(proc-exp @${var} @${body})}}} = @tt{(@${tvar} @${\to} @${t_{body}})}}}}
(proc-exp (var otype body)
  (let ((var-type (otype->type otype)))
    (cases answer (type-of body
                    (extend-tenv var var-type tenv)
                    subst)
      (an-answer (body-type subst)
        (an-answer
          (proc-type var-type body-type)
          subst)))))

@#,elem{@${\fbox{@tt{(call-exp @${rator} @${rand})} : @${t_{rator}} = @tt{(@${t_{rand}} @${\to} @${t_{@tt{(call-exp @${rator} @${rand})}}})}}}}
(call-exp (rator rand)
  (let ((result-type (fresh-tvar-type)))
    (cases answer (type-of rator tenv subst)
      (an-answer (rator-type subst)
        (cases answer (type-of rand tenv subst)
          (an-answer (rand-type subst)
            (let ((subst
                    (unifier
                      rator-type
                      (proc-type
                        rand-type result-type)
                      subst
                      exp)))
              (an-answer result-type subst))))))))
]

@eopl-caption["fig-7.8"]{INFERRED 的 @tt{type-of}，第 3 部分}}

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,elem{@${\fbox{\begin{math}\begin{alignedat}{-1}&@tt{letrec @${t_{proc\mbox{-}result}} @${p} (@${var} : @${t_{var}}) = @${e_{proc\mbox{-}body}} in @${e_{letrec\mbox{-}body}}} : \\ &\phantom{xx}t_{p} = t_{var} \to t_{e_{proc\mbox{-}body}} \\ &\phantom{xx}t_{e_{letrec\mbox{-}body}} = t_{@tt{letrec @${t_{proc\mbox{-}result}} @${p} (@${var} : @${t_{var}}) = @${e_{proc\mbox{-}body}} in @${e_{letrec\mbox{-}body}}}}\end{alignedat}\end{math}}}}
@#,exact-elem{\smallskip
\begin{comment}}
(((...
@#,exact-elem{\end{comment}}
    (letrec-exp (p-result-otype p-name b-var b-var-otype
                  p-body letrec-body)
      (let ((p-result-type (otype->type p-result-otype))
             (p-var-type (otype->type b-var-otype)))
        (let ((tenv-for-letrec-body
                (extend-tenv p-name
                  (proc-type p-var-type p-result-type)
                  tenv)))
          (cases answer (type-of p-body
                          (extend-tenv b-var p-var-type
                            tenv-for-letrec-body)
                          subst)
            (an-answer (p-body-type subst)
              (let ((subst
                      (unifier p-body-type p-result-type
                        subst p-body)))
                (type-of letrec-body
                  tenv-for-letrec-body
                  subst))))))))))
]

@eopl-caption["fig-7.9"]{INFERRED 的 @tt{type-of}，第 4 部分}
}

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,elem{@${\mathit{TvarTypeSym} = @emph{含有数字的符号}}}

@#,elem{@${\mathit{A\mbox{-}list} = \mathit{Listof(Pair(TvarTypeSym,TvarTypeSym))}}}

@#,elem{@bold{@tt{equal-up-to-gensyms?}} : @${\mathit{S\mbox{-}exp} \times \mathit{S\mbox{-}exp} \to \mathit{Bool}}}
(define equal-up-to-gensyms?
  (lambda (sexp1 sexp2)
    (equal?
      (apply-subst-to-sexp (canonical-subst sexp1) sexp1)
      (apply-subst-to-sexp (canonical-subst sexp2) sexp2))))

@#,elem{@bold{@tt{canonical-subst}} : @${\mathit{S\mbox{-}exp} \to \mathit{A\mbox{-}list}}}
(define canonical-subst
  (lambda (sexp)
    loop : S-exp × A-list → A-list
    (let loop ((sexp sexp) (table '()))
      (cond
        ((null? sexp) table)
        ((tvar-type-sym? sexp)
          (cond
            ((assq sexp table) table)
            (else
              (cons
                (cons sexp (ctr->ty (length table)))
                table))))
        ((pair? sexp)
          (loop (cdr sexp)
            (loop (car sexp) table)))
        (else table)))))
]

@eopl-caption["fig-7.10"]{@tt{equal-up-to-gensyms?}，第 1 部分}
}

@eopl-figure[#:position "!ht"]{
@racketblock[
@#,elem{@bold{@tt{tvar-type-sym?}} : @${\mathit{Sym} \to \mathit{Bool}}}
(define tvar-type-sym?
  (lambda (sym)
    (and (symbol? sym)
      (char-numeric? (car (reverse (symbol->list sym)))))))

@#,elem{@bold{@tt{symbol->list}} : @${\mathit{Sym} \to \mathit{List}}}
(define symbol->list
  (lambda (x)
    (string->list (symbol->string x))))

@#,elem{@bold{@tt{apply-subst-to-sexp}} : @${\mathit{A\mbox{-}list} \times \mathit{S\mbox{-}exp} \to \mathit{S\mbox{-}exp}}}
(define apply-subst-to-sexp
  (lambda (subst sexp)
    (cond
      ((null? sexp) sexp)
      ((tvar-type-sym? sexp)
        (cdr (assq sexp subst)))
      ((pair? sexp)
        (cons
          (apply-subst-to-sexp subst (car sexp))
          (apply-subst-to-sexp subst (cdr sexp))))
      (else sexp))))

@#,elem{@bold{@tt{ctr->ty}} : @${\mathit{N} \to \mathit{Sym}}}
(define ctr->ty
  (lambda (n)
    (string->symbol
      (string-append "tvar" (number->string n)))))
]

@eopl-caption["fig-7.11"]{@tt{equal-up-to-gensyms?}，第 2 部分}
}

@exercise[#:level 2 #:tag "ex7.23"]{

扩展推导器，像@exercise-ref{ex7.8} 那样处理序对类型。

}

@exercise[#:level 2 #:tag "ex7.24"]{

扩展推导器，处理多声明 @tt{let}、多参数过程和多声明 @tt{letrec}。

}

@exercise[#:level 2 #:tag "ex7.25"]{

扩展推导器，像@exercise-ref{ex7.9} 那样处理列表类型。修改语言，用生成式

@$${\mathit{Expression} ::= @tt{emptylist}}

代替

@$${\mathit{Expression} ::= @tt{emptylist_@${\mathit{Type}}}}

提示：考虑用类型变量代替缺失的 @tt{_@${t}}。
}

@exercise[#:level 2 #:tag "ex7.26"]{

扩展推导器，像@exercise-ref{ex7.10} 那样处理 EXPLICIT-REFS。

}

@exercise[#:level 2 #:tag "ex7.27"]{

重写推导器，将其分为两步。第一步生成一系列方程，第二步重复调用 @tt{unify} 求解它
们。

}

@exercise[#:level 2 #:tag "ex7.28"]{

我们的推导器虽很有用，却不够强大，不允许程序员定义多态过程，像定义多态原语
@tt{pair} 或 @tt{cons} 那样，适用于多种类型。例如，即使执行是安全的，我们的推导
器也会拒绝程序

@eopl-code{
@verbatim|{
let f = proc (x : ?) x
in if (f zero?(0))
   then (f 11)
   else (f 22)
}|
}

因为 @tt{f} 既是 @tt{(bool -> bool)} 也是 @tt{(int -> int)}。由于本节的推导器至
多只能找出 @tt{f} 的一种类型，它将拒绝这段程序。

更实际的例子是这样的程序

@eopl-code{
@verbatim|{
letrec
 ? map (f : ?) =
    letrec
     ? foo (x : ?) = if null?(x)
                     then emptylist
                     else cons((f car(x)),
                               (foo cdr(x)))
    in foo
in letrec
    ? even (y : ?) = if zero?(y)
                     then zero?(0)
                     else if zero?(-(y,1))
                          then zero?(1)
                          else (even -(y,2))
   in pair(((map proc(x : int) -(x,1))
           cons(3,cons(5,emptylist))),
           ((map even)
            cons(3,cons(5,emptylist))))
}|
}

这个表达式用了两次 @tt{map}，一次产生 @tt{int} 列表，一次产生 @tt{bool} 列表。因
此，两次使用它需要两个不同的类型。由于本节的推导器至多只能找出 @tt{map} 的一种类
型，它检测到 @tt{int} 和 @tt{bool} 冲突，拒绝程序。

避免这个问题的一种方法是只允许 @tt{let} 引入多态，然后在类型检查时区分
@tt{(let-exp @${var} @${e_1} @${e_2})} 和 @tt{(call-exp (proc-exp @${var}
@${e_2}) @${e_1})}。

给推导器添加多态绑定，处理表达式 @tt{(let-exp @${var} @${e_1} @${e_2})} 时，把
@${e_2} 中自由出现的每个 @${var} 代换为 @${e_1}。那么，在推导器看来，@tt{let} 主
体中有多个不同的 @${e_1} 副本，它们可以有不同的类型，上述程序就能通过。

}

@exercise[#:level 3 #:tag "ex7.29"]{

@eopl-index[#:suffix @exer-ref-range["ex7.29"] "Algorithm W"]
前一道练习指出的类型推导算法会多次分析@${e_1}，每次对应 @${e_2} 中出现的一个
@${e_1}。实现 Milner 的 W 算法，只需分析 @${e_1} 一次。

}

@exercise[#:level 3 #:tag "ex7.30"]{

多态和副作用之间的相互作用很微妙。考虑以下文开头的一段程序

@eopl-code{
@verbatim|{
let p = newref(proc (x : ?) x)
in ...
}|
}

@itemlist[#:style 'ordered

 @item{完成这段程序，使之通过推导器的检查，但根据本章开头的定义，求值不安全。}

 @item{限制 @tt{let} 声明的右边，不允许出现作用于存储器的效果，从而避免这一问题。
 这叫做@term["value restriction"]{值约束}。}

]

}
