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
