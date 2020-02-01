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

@title[#:style 'numbered #:tag "state"]{状态}

@section[#:tag "s4.1"]{计算的效果}

到目前为止，我们只考虑了计算产生的@emph{值} (@emph{value})，但是计算也有@emph{效
果} (@emph{effect})：它可以读取，打印，修改内存或者文件系统的状态。在现实世界中，
我们对效果@emph{总是}很感兴趣：如果一次计算不显示答案，那对我们完全没用！

产生值和产生效果有什么不同？效果是@emph{全局性} (@emph{global})的，整个计算都能
看到。@elem[#:style question]{效果对整个计算有效。（双关是什么？未见）}

我们主要关心一种效果：给内存中的位置赋值。赋值与绑定有何区别？如我们所见，绑定是
局部的，但变量赋值有可能是全局的。那是在本不相关的几部分计算之间@emph{分享}
(@emph{share})值。如果两个过程都知道内存中的同一位置，它们就能分享信息。如果把信
息留在已知位置，同一个过程就能在当前和后续调用之间分享信息。

我们把内存建模为从@emph{位置} (@emph{location})到值集合的的有限映射，并把这个值
集合叫做@emph{可存储值} (@emph{storable values})。出于历史原因，我们称之为
@emph{存储器} (@emph{store})。通常，一种语言中的可存储值与表达值相同，但不总是这
样。这个选择是语言设计的一部分。

代表内存位置的数据结构叫做@emph{引用} (@emph{reference})。位置是内存中可用来存值
的地方，引用是指向那个地方的数据结构。位置和引用的区别可以这样类比：位置就像文件，
引用就像一个URL。URL指向一个文件，文件包含一些数据。类似地，引用指代一个位置，位
置包含一些数据。

引用有时候又叫@emph{左值} (@emph{L-values})。这名字反映了这种数据结构与赋值语句
左边变量的联系。类似地，表达值，比如赋值语句右边的值，叫做@emph{右值}
(@emph{R-values})。

我们考虑两种有存储器的语言设计，分别称之为@emph{显式引用} (@emph{explicit
reference})和@emph{隐式引用} (@emph{implicit reference})。

@section[#:tag "s4.2"]{EXPLICIT-REFS：显式引用语言}

在这种设计中，我们添加引用，作为另一种表达值。那么，我们有：

@nested{
@envalign*{
\mathit{ExpVal} &= \mathit{Int} + \mathit{Bool} + \mathit{Proc} + \mathit{Ref(ExpVal)} \\
\mathit{DenVal} &= \mathit{ExpVal}
}

这里，@${\mathit{Ref(ExpVal)}}表示包含表达值的内存位置引用集合。

}

我们沿用语言中的绑定数据结构，但是添加三个新操作，用来创建和使用引用。

@itemlist[

 @item{@tt{newref}，分配新的位置，返回其引用。}

 @item{@tt{deref}，@emph{索值} (@emph{deference}) ：返回引用指向位置中的内容。}

 @item{@tt{setref}，改变引用指向位置中的内容。}

]

我们把得到的语言称作EXPLICIT-REFS。让我们用这些构造器写几个程序。

下面是两个过程@tt{even}和@tt{odd}。它们取一参数，但是忽略它，并根据位置@tt{x}处
的内容是偶数还是奇数返回1或0。它们不是通过明确传递数据来通信，而是改变共享变量的
内容。

这个程序判断13是否为奇数，并返回1。过程@tt{even}和@tt{odd}不使用它们的实参，而是
查看@tt{x}绑定的位置中的内容。

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

这个程序使用能声明多个变量的@tt{letrec}（练习3.32）以及@tt{begin}表达式（练习
4.4）。@tt{begin}表达式按顺序求值每个子表达式，并返回最后一个的值。

}

为了同我们的单参数语言保持一致，我们给@tt{even}和@tt{odd}传一个无用参数；如果我
们的过程支持任意数量参数（练习3.21），我们就不用给这些过程传参数。

当两个过程需要分享很多量时，这种通信方式很方便；只须给某些随调用而改变的量赋值。
同样地，一个过程可能通过一长串调用间接使用另一过程。它们可以通过一个共享变量直接
交换数据，居间的过程不需要知道它。以共享变量通信的方式可作为一种隐藏信息的方式。

赋值的另一种作用是通过私有变量创建隐藏状态。这里是一个例子。

@nested[#:style 'code-inset]{
@verbatim|{
let g = let counter = newref(0)
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

这里，过程@tt{g}保留了一个私有变量，用来存储@tt{g}被调用的次数。因此，第一次调用
@tt{g}返回1，第二次返回2，整个程序的值为@${-1}。

下图是@tt{g}绑定时所在的环境。

@centered{
@(image "../images/g-bound"
  #:scale 0.95
  #:suffixes (list ".pdf" ".svg")
  "g绑定时的环境")
}
