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

@title[#:style 'numbered #:tag "types"]{类型}

我们已明白如何用解释器建模程序的运行时行为。现在，我们用同样的技术不加运行地
@emph{分析}或@emph{预测}程序的行为。

我们已见识过一些了：我们的词法地址翻译器在分析阶段预测运行时如何从环境中找出每个
变量。而且，翻译器本身看起来就像一个解释器，只是我们传递的不是环境，而是静态环境。

我们的目标是分析程序，预测程序求值是否@emph{安全} (@emph{safe})，即，求值过程是
否能避免某些类型的错误。但安全究竟何意因语言而不同。如果我们能保证求值是安全的，
我们就能确保程序满足其合约。

本章，我们考虑类似@secref{expr}中LETREC的语言。这些语言求值安全，当且仅当：

@itemlist[#:style 'ordered

 @item{每个待求值的变量@${var}都已绑定。}

 @item{每个待求值的差值表达式@tt{(diff-exp @${exp_1} @${exp_2})}中，@${exp_1}和
 @${exp_2}的值都是@tt{nul-val}。}

 @item{每个待求值的表达式@tt{(zero?-exp @${exp_1})}中，@${exp_1}的值都是
 @tt{num-val}。}

 @item{每个待求值的条件表达式@tt{(if-exp @${exp_1} @${exp_2} @${exp_3})}中，
 @${exp_1}的值都是@tt{bool-val}。}

 @item{每个待求值的过程调用@tt{(call-exp @${rator} @${rand})}中，@${rator}的值都
 是@tt{proc-val}。}

]

这些条件确保每个操作符都作用于正确类型的操作数。因此，我们说违反这些条件是
@emph{类型错误} (@emph{type error})。

安全的求值仍可能因为其他原因而失败：除以零，取空列表的@tt{car}，等等。我们不把这
些算作安全的定义，因为在预测安全性时，保证这些条件要比上面列出的难得多。同样地，
安全的求值可能永远运行。我们不把无法终止算作安全的定义，因为检查程序是否终止也很
困难（事实上，这一般是无法判定的）。有些语言的类型系统给出比上述更强的保证，但要
比我们这里考虑的复杂得多。

我们的目标是写出过程，查看程序文本，接受或者拒绝它。而且，我们希望我们的分析过程
保守一点：如果分析接受程序，可以确保求值程序是安全的。如果分析不能确定求值是安全
的，它必须拒绝程序。我们称这样的分析是@emph{健壮的} (@emph{sound})。

拒绝所有程序的分析仍是健壮的，可我们还是想让我们的分析接受一大批程序。本章的分析
将接受足够多的程序，因此是有用的。

这里是一些示例程序，以及它们应被分析拒绝或接受：

@nested[#:style 'code-inset]{
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

虽然最后一个待求值的例子不终止，但根据上述定义，求值仍是安全的，所以我们的分析可
以接受它。原来，由于我们的分析器不够好，无法判定本程序不会终止，因此会接受它。

@section[#:tag "s7.1"]{值及其类型}

由于安全条件只涉及@tt{num-val}，@tt{bool-val}和@tt{proc-val}，有人可能以为记录这
三种类型就足够了。但那是不够的：如果我们只知道@tt{f}绑定到一个@tt{proc-val}，我
们根本无法确认@tt{(f 1)}的值。从这个角度来看，我们需要更细致地记录与过程相关的信
息。这些更细致的信息叫做语言的@emph{类型结构} (@emph{type structure})。

我们的语言将有一种非常简单的类型结构。现在，考虑LETREC中的表达值。这些值只包含单
参数过程，但处理练习3.33中的多参数过程也很直接：只需做些额外工作，没有任何新思想。

@bold{类型语法}
@; TODO: big bracket
@envalign*{\mathit{Type} &::= \mathit{int} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{int-type ()}} \\[5pt]
           \mathit{Type} &::= \mathit{bool} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{bool-type ()}} \\[5pt]
           \mathit{Type} &::= @tt{(@m{\mathit{Type}} -> @m{\mathit{Type}})} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{proc-type (arg-type result-type)}}}
@; TODO: big bracket

要理解这个系统如何工作，让我们来看些例子。

@bold{值及其类型的例子}
@; TODO: big bracket
@nested[#:style 'noindent]{

值@tt{3}的类型为@tt{int}。

值@tt{-(33,22)}的类型为@tt{int}。

值@tt{zero?(11)}的类型为@tt{bool}。

值@tt{proc (x) -(x,11)}的类型为@tt{int -> int}，因为给一个整数时，它返回一个整数。

值@tt{proc (x) let y = -(x,11) in -(x,y)}@linebreak[]的类型为@tt{int -> int}，因
为给一个整数时，它返回一个整数。

值@tt{proc (x) if x then 11 else 22}@linebreak[]的类型为@tt{bool -> int}，因为给
一个布尔值时，它返回一个整数。

值@tt{proc (x) if x then 11 else zero?(11)}在我们的类型系统中没有类型，因为给一
个布尔值时，它既可能返回一个整数，也可能返回一个布尔值，而我们没有描述这种行为的
类型。

值@tt{proc (x) proc (y) if y then x else 11}@linebreak[]的类型为@tt{(int ->
(bool -> int))}，因为给一个布尔值时，它返回一个过程，该过程取一布尔值，返回一整
树。

值@tt{proc (f) (f 3)}的类型为@tt{((int -> @${t}) -> @${t})}，@${t}是任意类型，因
为给一个类型为@tt{(int -> @${t})}的过程，它返回类型为@${t}的值。

值@tt{proc (f) proc (x) (f (f x))}的类型为@tt{((@${t} -> @${t}) -> (@${t} ->
@${t}))}，@${t}是任意类型，因为给一个类型为@tt{(@${t} -> @${t})}的过程，它返回另
一过程，该过程取一类型为@${t}的参数，返回一类型为@${t}的值。

}
@; TODO: big bracket

我们用下面的定义解释这些例子。

@nested{

@nested[#:style (make-style "sdef" '())]{
 性质“表达值v的类型为t”由对t进行归纳得到：

 @itemlist[

  @item{当且仅当表达值是一个@tt{num-val}，其类型为@tt{int}。}

  @item{当且仅当表达值是一个@tt{bool-val}，其类型为@tt{bool}。}

  @item{当且仅当表达值是一个@tt{proc-val}，且给定类型为@${t_1}的参数时，发生如下
  之一：

  @itemlist[#:style 'ordered

   @item{返回值类型为@${t_2}}

   @item{不终止}

   @item{发生类型错误之外的错误}
  ]

  其类型为@tt{(@${t_1 \to t_2})}。} ]

}

有时，我们不说“@${v}类型为@${t}”，而说“@${v}具有类型@${t}”。

}

此定义归纳自@${t}。但是它依赖于上面另行定义的类型错误。

在该系统中，值@${v}可以有多个类型。比如，值@tt{proc (x) x}类型为@tt{(@${t \to
t})}，@${t}是任意类型。有些值可能没有类型，比如@tt{proc (x) if x then 11 else
zero?(11)}。

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

根据定义7.1.1，有没有表达值恰好有两种类型？

}

@exercise[#:level 2 #:tag "ex7.3"]{

在语言LETREC中，能否判定表达值@${val}的类型为@${t}？

}

@section[#:tag "s7.2"]{赋予表达值类型}

目前，我们只处理了表达值的类型，要分析程序，我们要写出过程，取一表达式，预测其类
型。

更准确地说，我们的目标是写出过程@tt{type-of}，它取一个表达式（名为@${exp}），一
个将每个变量映射到一个类型的@emph{类型环境} (@emph{type environment})（名为
@${tenv}），赋给@${exp}一个类型@${t}，且@${t}具有性质：

@bold{type-of规范}
@; TODO: big bracket
@nested[#:style 'noindent]{

不论何时求值@${exp}，若环境中每个变量的值类型都由@${tenv}指定，则发生如下之一：

@itemlist[

@item{结果类型为@${t}，}

@item{求值不终止，或}

@item{求值因类型错误之外的原因失败。}

]

}
@; TODO: big bracket

如果我们可以赋予表达式一个类型，我们说该表达式是@emph{正常类型}
(@emph{well-typed})的，否则我们说它是@emph{异常类型} (@emph{ill-typed})或
@emph{无类型} (@emph{has no type})的。

我们分析的原则是，如果能预测表达式中每个子表达式的值类型，就能预测表达式的值类型。

我们用这一想法写出@tt{type-of}遵循的一些规则。设@${tenv}为一@emph{类型环境}，将
各个变量映射到一类型。那么我们有：

@bold{简单@elem[#:style question]{类型(typing)}规则}
@; TODO: big bracket
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
@; TODO: big bracket

若我们在适当的环境中求类型为@${t}的表达式@${exp}的值，我们不仅知道值的类型为
@${t}，也知道与这个值有关的历史信息。因为求值@${exp}保证是安全的，我们知道
@${exp}的值一定是由符合类型@${t}的操作符产生的。在第8章，我们更细致地思考数据抽
象时，这种观点会很有帮助。

过程如何呢？如果@tt{proc(@${var}) @${body}}类型为@${t_1 \to t_2}，那么应该用类型
为@${t_1}的参数调用它。求值@${body}时，绑定到变量@${var}的值类型为@${t_1}。

这给出如下规则：

@$${\infer{@tt{(type-of (proc-exp @${var} @${body}) @${tenv}) = @${t_1 \to t_2}}}
          {@tt{(type-of @${body} [@${var}=@${t_1}]@${tenv}) = @${t_2}}}}

这条规则是健壮的：如果@${type-of}正确预测了@${body}，那么它也能正确预测
@tt{(proc-exp @${var} @${body})}。

只有一个问题：如果我们要计算@tt{proc}表达式的值，我们怎么找出绑定变量的类型
@${t_1}？它无处可寻。

处理这个问题，有两种标准设计：

@itemlist[

 @item{@emph{类型检查} (@emph{Type Checking})：按这种方法，程序员需要指出缺失的
 绑定变量类型，类型检查器推断其他表达式的类型，检查它们是否一致。}

 @item{@emph{类型推导} (@emph{Type Inference})：按这种方法，类型检查器根据程序中
 变量的使用，尝试@emph{推断} (@emph{infer})绑定变量的类型。如果语言设计得当，类
 型检查器可以推断处大多数或所有这样的类型。}

]

我们依次研究它们。

@exercise[#:level 1 #:tag "ex7.4"]{

用本节的规则，像第5页那样，写出@tt{proc (x) x}和@tt{proc (x) (x y)}的类型推导。
运用规则，给每个表达式赋予至少两种类型。这些表达式的值类型相同吗？

}

@section[#:tag "s7.3"]{CHECKED：带有类型检查的语言}

我们的新语言和LETREC相同，但我们要求程序员写出所有绑定变量的类型。对由
@tt{letrec}绑定的变量，我们还要求程序员指定过程结果的类型。

这里是一些CHECKED程序例子。

@nested[#:style 'code-inset]{
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

@tt{double}结果的类型为@tt{int}，但@tt{double}本身的类型为@tt{(int -> int)}，因
为它是一个过程，取一整数，返回一整树。

要定义这种语言的语法，我们改变@tt{proc}和@tt{letrec}表达式的生成式。

@bold{修改后的生成式，适用于CHECKED}
@; TODO: big bracket
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
                                         \end{alignedat}\end{math}}}
@; TODO: big bracket

对指定绑定变量类型的@tt{proc}表达式，规则变为：

@$${\infer{@tt{(type-of (proc-exp @${var} @${t_{var}} @${body}) @${tenv}) = @${t_{var} \to t_{res}}}}
          {@tt{(type-of @${body} [@${var}=@${t_{var}}]@${tenv}) = @${t_{res}}}}}

@tt{letrec}呢？典型的@tt{letrec}如下：

@nested[#:style 'code-inset]{
@verbatim|{
letrec
  |@${t_{res}} |@${p} (|@${var} : |@${t_{var}}) = |@${e_{proc\mbox{-}body}}
in |@${e_{letrec\mbox{-}body}}
}|
}

该表达式声明一个名为@${p}的过程，其形参是类型为@${t_{var}}的变量，主体为
|@${e_{proc\mbox{-}body}}。因此，@${p}的类型应为@${t_{var} \to t_{res}}。

检查@tt{letrec}中的表达式@${e_{proc\mbox{-}body}}和@${e_{letrec\mbox{-}body}}时，
类型环境中的每个变量必须有正确的类型。可以用我们的定界规则判断每个变量是否在作用
范围中，并由此判断对应的类型。

在@${e_{letrec\mbox{-}body}}中，过程名@${p}在范围内。如上所述，@${p}的类型声明为
@${t_{var} \to t_{res}}。因此，检查@${e_{letrec\mbox{-}body}}时的类型环境应为：

@$${tenv_{letrec\mbox{-}body} = @tt["["]p=(t_{var} \to t_{res})@tt["]"]tenv}

@${e_{proc\mbox{-}body}}呢？在@${e_{proc\mbox{-}body}}中，变量@${p}在范围内，其
类型为@${t_{var} \to t_{res}}，变量@${var}在范围内，类型为@${t_{var}}。因此，
@${e_{proc\mbox{-}body}}的类型环境应为：

@$${tenv_{proc\mbox{-}body} = @tt["["]var=t_{var}@tt["]"]tenv_{letrec\mbox{-}body}}

而且，在这个类型环境中，@${e_{proc\mbox{-}body}}的结果类型应为@${t_{res}}。

把这些写成一条规则，我们有：

@$${\infer{@tt{(type-of (letrec-exp @${t_{res}} @${p} (@${var} : @${t_{var}}) = @${e_{proc\mbox{-}body}} in @${e_{letrec\mbox{-}body}}) @${tenv}) = @${t}}}
          {\begin{gathered}
           @tt{(type-of @${e_{proc\mbox{-}body}} [@${var}=@${t_var}][@${p}=@${(t_{var} \to t_{res})}]@${tenv}) = @${t_{res}}} \\
           @tt{(type-of @${e_{letrec\mbox{-}body}} [@${p}=@${(t_{var} \to t_{res})}]@${tenv}) = @${t}}
           \end{gathered}}}

现在我们已经写出了所有规则，可以实现语言的类型检查器了。

@subsection[#:tag "s7.3.1"]{检查器}

我们需要比较类型是否相等。我们用过程@tt{check-equal-type!}做比较，它比较两个类型，
若二者不等则报错。@tt{check-equal-type!}的第三个参数是一表达式，在类型不等时提供
错误信息。

@racketblock[
@#,elem{@bold{@tt{check-equal-type!}} : @${\mathit{Type} \times \mathit{Type} \times \mathit{Exp} \to \mathit{Unspecified}}}
(define check-equal-type!
  (lambda (ty1 ty2 exp)
    (if (not (equal? ty1 ty2))
      (report-unequal-types ty1 ty2 exp))))

@#,elem{@bold{@tt{report-unequal-types}} : @${\mathit{Type} \times \mathit{Type} \times \mathit{Exp} \to \mathit{Unspecified}}}
(define report-unequal-types
  (lambda (ty1 ty2 exp)
    (eopl:error ’check-equal-type!
      "类型不匹配: ~s != ~a 位置~%~a"
      (type-to-external-form ty1)
      (type-to-external-form ty2)
      exp)))
]

我们不使用@tt{check-equal-type!}调用的返回值，因此@tt{check-equal-type!}的执行只
求效果，如同@secref{s4.2.2}中的@tt{setref}那样。

过程@tt{report-unequal-types}用@tt{type-to-external-form}，将类型转换为易读的列
表。

@racketblock[
@#,elem{@bold{@tt{type-to-external-form}} : @${\mathit{Type} \to \mathit{List}}}
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (int-type () ’int)
      (bool-type () ’bool)
      (proc-type (arg-type result-type)
        (list
          (type-to-external-form arg-type)
          ’->
          (type-to-external-form result-type))))))
]

现在，我们可以将规则转换为程序，就像处理@secref{expr}中的解释器那样。结果如图
7.1-7.3所示。

@nested[#:style eopl-figure]{
@racketblock[
@#,elem{@${\mathit{Tenv} = \mathit{Var} \to \mathit{Type}}}

@#,elem{@bold{@tt{type-of-program}} : @${\mathit{Program} \to \mathit{Type}}}
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1) (type-of exp1 (init-tenv))))))

@#,elem{@bold{@tt{type-of-program}} : @${\mathit{Exp} \times \mathit{Tenv} \to \mathit{Type}}}
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
...)))
]

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "CHECKED的" (tt "type-of")))]
}

@nested[#:style eopl-figure]{
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

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "CHECKED的" (tt "type-of") "，续"))]

}

@nested[#:style eopl-figure]{
@racketblock[
(((
      @#,elem{@${\fbox{\infer{@tt{(type-of letrec @${t_{res}} @${p} (@${var} : @${t_{var}}) = @${e_{proc\mbox{-}body}} in @${e_{letrec\mbox{-}body}} @${tenv}) = @${t}}}{\begin{gathered}@tt{(type-of @${e_{proc\mbox{-}body}} [@${var}=@${t_{var}}][@${p} =(@${t_{var} \to t_{res}})]@${tenv}) = @${t_{res}}} \\ @tt{(type-of @${e_{letrec\mbox{-}body}} [@${p} = (@${t_{var} \to t_{res}})]@${tenv}) = @${t}}\end{gathered}}}}}
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

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "CHECKED的" (tt "type-of") "，续"))]
}

@exercise[#:level 2 #:tag "ex7.5"]{

扩展检查器，处理多声明@tt{let}，多参数过程，以及多声明@tt{letrec}。你需要添加形
如@tt{@${t_1} * @${t_2} * @${\dots} * @${t_n} -> @${t}}的类型处理多参数过程。

}

@exercise[#:level 1 #:tag "ex7.6"]{

扩展检查器，处理赋值（@secref{s4.3}）。

}

@exercise[#:level 1 #:tag "ex7.7"]{

修改检查@tt{if-exp}的代码，若条件不是布尔值，则不检查其他表达式。给出一个例子，
在两个检查器中行为不同。

}

@exercise[#:level 2 #:tag "ex7.8"]{

给语言添加类型@tt{pairof}。比如，当且仅当一个值是序对，且所含值类型为@${t_1}和
@${t_2}时，其类型为@tt{pairof @${t_1} * @${t_2}}。给语言添加下列生成式：

@envalign*{\mathit{Type} &::= @tt{pairof @m{\mathit{Type}} * @m{\mathit{Type}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{pair-type (ty1 ty2)}} \\[5pt]
     \mathit{Expression} &::= @tt{pair (@m{\mathit{Expression}} , @m{\mathit{Expression}})} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{pair-exp (exp1 exp2)}} \\[5pt]
     \mathit{Expression} &::= @tt{unpair @m{\mathit{Identifier}} @m{\mathit{Identifier}} = @m{\mathit{Expression}}} \\[-3pt]
       &\mathrel{\phantom{::=}} @tt{in @m{\mathit{Expression}}} \\[-3pt]
       &\mathrel{\phantom{::=}} \fbox{@tt{unpair-exp (var1 var2 exp body)}}}

@tt{pair}表达式生成一个序对，@tt{unpair}表达式（类似练习3.18）将两个变量绑定到表
达式的两部分。这些变量的作用范围是@tt{body}。@tt{pair}和@tt{unpair}的推类规则为：

@$${\infer{@tt{(type-of (pair-exp @${e_1} @${e_2}) @${tenv}) = pairof @${t_1} * @${t_2}}}
          {\begin{gathered}
           (type-of @${e_1} @${tenv}) = @${t_1} \\
           (type-of @${e_2} @${tenv}) = @${t_2} \\
           \end{gathered}}}

@$${\infer{@tt{(type-of (unpair-exp @${var_1} @${var_2} @${e_1} @${e_{body}}) @${tenv}) = @${t_{body}}}}
          {\begin{gathered}
           @tt{(type-of @${e_{pair}} @${tenv}) = (pairof @${t_1} @${t_2})} \\
           @tt{(type-of @${e_{body}} [@${var_1}=@${t_1}][@${var_2}=@${t_2}]@${tenv}) = @${t_{body}}} \\
           \end{gathered}}}

扩展CHECKED，实现这些规则。在@tt{type-to-external-form}中，用列表@tt{(pairof
@${t_1} @${t_2})}表示序对。

}

@exercise[#:level 2 #:tag "ex7.9"]{

给语言添加类型@tt{listof}，其操作与练习3.9类似。当且仅当值是列表，且所有元素类型
均为@${t}时，值类型为@tt{listof @${t}}。用下列生成式扩展语言：

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

以及四个与类型相关的规则：

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

虽然@tt{cons}和@tt{pair}类似，它们的类型规则却非常不同。

为@tt{car}和@tt{cdr}写出类似的规则，扩展检查器，处理这些和上述表达式。用练习7.8
中的小技巧避免与@tt{proc-type-exp}的冲突。这些规则应保证@tt{car}和@tt{cdr}应用于
列表，但它们不保证列表非空。为什么让规则确保列表非空不合理？为什么@tt{emptylist}
中的类型参数是必需的？

}

@exercise[#:level 2 #:tag "ex7.10"]{

扩展检查器，处理EXPLICIT-REFS。处理方式如下：

@itemlist[

 @item{给类型系统添加类型@tt{refto @${t}}，其中，@${t}是任意类型。这个类型表示引
 用，指向的位置包含类型为@${t}的值。那么，若@${e}类型为@${t}，@tt{(newref
 @${e})类型为@tt{refto @${t}}}。}

 @item{给类型系统添加类型@tt{void}。@tt{seref}的返回值为此类型。对类型为
 @tt{void}的值，不能进行任何操作，所以@tt{setref}返回什么值都不要紧。这是用类型
 作为信息隐藏机制的例子。}

 @item{写出@tt{newref}，@tt{deref}和@tt{setref}的类型规则。}

 @item{在检查器中实现这些规则。}

]

}

@exercise[#:level 2 #:tag "ex7.11"]{

扩展检查器，处理MUTABLE-PAIRS。

}

@section[#:tag "s7.4"]{INFERRED：带有类型推导的语言}
