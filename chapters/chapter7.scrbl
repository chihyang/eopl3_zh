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

  其类型为@tt{(@${t_1} @${\to} @${t_2})}。} ]

}

有时，我们不说“@${v}类型为@${t}”，而说“@${v}具有类型@${t}”。

}

此定义归纳自@${t}。但是它依赖于上面另行定义的类型错误。

在该系统中，值@${v}可以有多个类型。比如，值@tt{proc (x) x}类型为@tt{(@${t}
@${\to} @${t})}，@${t}是任意类型。有些值可能没有类型，比如@tt{proc (x) if x then
11 else zero?(11)}。

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

@section[#:tag "s7.2"]{赋给表达值类型}

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

@bold{简单推类规则}
@; TODO: big bracket

@; TODO: big bracket
