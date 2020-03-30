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

@title[#:style 'numbered #:tag "modules"]{模块}

对只有几百行代码的系统，我们介绍的语言特性已非常强大。如果我们要设计更大的系统，
有数千行代码，我们就还需要一些别的佐料。

@itemlist[#:style 'ordered

 @item{我们需要一种好办法，将系统分为相对独立的部分，并能说明各部分之间的依赖关
 系。}

 @item{我们需要一种更好的方式来控制名字的作用范围和绑定。词法定界是控制命名的有
 效工具，但当程序更大或有多个来源时还是不够。}

 @item{我们需要一种方式加强抽象边界。在@secref{da}，我们介绍了抽象数据类型的思想。
 在类型的实现中，类型的值只能通过类型接口中的过程创建和操作。我们把这叫做
 @emph{抽象边界} (@emph{abstraction boundary})。如果程序尊重这种界限，我们可以改
 变数据类型的实现。但是，如果某些代码打破了抽象，依赖实现细节，那么我们就无法任
 意修改实现而不破坏其他代码。}

 @item{最后，我们需要一种方式，将这些部分灵活组合，那么同一部分可复用于不同地方。}

]

本章，我们介绍@emph{模块} (@emph{module})，以满足这些需求。具体来说，我们展示了
如何用类型系统创建和强化抽象边界。

我们的模块语言中，程序包含一系列@emph{模块定义} (@emph{module definition})，后跟
待求值的表达式。每个模块定义把一个名字绑定到一个@emph{模块}。创建的模块可能是
@emph{简单模块} (@emph{simple module})，即类似环境的一些绑定；也可能是@emph{模块
过程} (@emph{module procedure})，取一模块，生成另一模块。

每个模块都有一套@emph{接口} (@emph{interface})。简单模块具有@emph{简单接口}
(@emph{simple interface})，接口列出模块提供的绑定及其类型。模块过程的接口指定参
数模块和返回模块的接口，就像过程的类型指明参数和结果的类型。

这些接口就像类型一样，决定了模块如何组合。因为求出示例程序的值非常简单，因此我们
强调其类型。如前所见，理解这些语言的定界和绑定规则是程序分析和求值的关键。

@section[#:tag "s8.1"]{简单模块系统}

我们的第一种语言名叫SIMPLE-MODULES，只有简单模块。它没有模块过程，只创建非常简单
的抽象边界。几种流行语言使用与之类似的模块系统。

@subsection[#:tag "s8.1.1"]{例子}

设想一个软件项目中有三名开发者：爱丽丝，鲍伯和查理。爱丽丝、鲍伯和查理正在开发项
目中相对独立的几部分。这些开发者散居各处，时区都可能不同。项目的每部分都要实现一
套@secref{s2.1}那样的接口，但接口的实现可能涉及大量其他过程。而且，开发者们需要
确保没有命名冲突，当个部分集成到一起时，不会干扰项目的其他部分。

要实现这一目标，开发者们需要公布一套接口，列出每个供他人使用的过程名字。模块系统
要保证这些名字是公开的，而它们使用的其他名字则是私有的，不会被项目中其他代码篡改。

我们可以用@secref{expr}中的定界技术，但这些无法应对更大的工程。所以，我们使用模
块系统。开发者们各给出一个模块，包含公开接口和私有实现。他们能看到自己模块的接口
和实现，但爱丽丝只能看到他人模块的接口。她的所做所为不会影响其他模块的实现，其他
模块的实现也不会影响她的。（如图8.1所示）

@nested[#:style eopl-figure]{
@centered{
@(image "../images/alice-view"
  #:suffixes (list ".pdf" ".svg")
  "项目中，爱丽丝所见的三个模块")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "项目中，爱丽丝所见的三个模块"))]
}

这是SIMPLE-MODULES的简单例子。

@nested[#:style eopl-example]{
@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [a : int
   b : int
   c : int]
 body
  [a = 33
   x = -(a,1)  % = 32
   b = -(a,x)  % = 1
   c = -(x,b)] % = 31
let a = 10
in -(-(from m1 take a,
       from m1 take b),
     a)
}|
}

类型为@tt{int}，值为@${((33-1)-10)=22}。
}
}

此程序从名为@tt{m1}的模块定义开始。像其他模块一样，它有@emph{接口}和@tt{主体}。
主体@emph{实现}接口。接口@emph{声明}变量@tt{a}、@tt{b}和@tt{c}。主体@emph{定义}
了@tt{a}、@tt{x}、@tt{b}和@tt{c}。

求程序的值时，也会求出@tt{m1}主体中表达式的值。变量@tt{from m1 take a}、@tt{from
m1 take b}和@tt{from m1 take c}绑定到适当的值，模块定义之后在它们的作用范围内。
由于@tt{from m1 take x}未在接口中声明，所以模块定义之后不在它的作用范围内。

为了同@emph{简单变量} (@emph{simple variable})却别，我们称这些新变量为@emph{受限
变量} (@emph{qualified})。在传统语言中，受限变量写作@tt{m1.a}、@tt{m1:a}或
@tt{m1::a}。在第9章探讨的面向对象语言中，@tt{m1.a}常表示其他内容。

我们说接口@emph{提出} (@emph{offer})（或称@emph{公布} (@emph{advertise})，或称
@emph{承诺} (@emph{promise})）三个整型值，主体@emph{供应}（@emph{supply}或
@emph{provide}）（或称@emph{输出} (@emph{export})）这些值。当模块主体供应的值类
型与接口命名变量时公布的相符时，称主体@emph{满足} (@emph{satisfy})接口。

在主体中，定义具有@tt{let*}那样的作用范围，所以@tt{x}、@tt{b}和@tt{c}在@tt{a}的
作用范围内。部分作用范围如图8.2所示。

本例中，以@tt{let a = 10}开头的表达式是@emph{程序主体} (@emph{program body})。它
的值即程序的值。

每个模块都在模块主体和程序其他部分之间建立了抽象边界。模块主体中的表达式在抽象边
界@emph{之内}，其他部分在抽象边界@emph{之外}。模块主体也可以供应不在接口中的名字
绑定，但那些绑定在程序主体和其他模块中不可见，正如图8.1所示。在我们的例子中，程
序主体不在@tt{from m1 take x}的作用范围内。如果我们写@tt{-(from m1 take a, from
m1 take x)}，程序就会是异常类型。

@nested[#:style eopl-example]{
程序

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : bool]
 body
  [u = 33]

44
}|
}

类型异常。就算程序的其他部分不使用那些值，模块主体也得将接口中的名字与适当类型的
值关联起来。
}
}

@nested[#:style eopl-figure]{
@centered{
@(image "../images/module-contour"
  #:suffixes (list ".pdf" ".svg")
  "简单模块中的一些作用范围")
}

@make-nested-flow[
 (make-style "caption" (list 'multicommand))
 (list (para "简单模块中的一些作用范围"))]
}

@nested[#:style eopl-example]{
模块主体必须供应接口中声明的所有绑定。例如，

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : int
   v : int]
 body
  [u = 33]

44
}|
}

类型异常，因为@tt{m1}的主体没有提供接口中公布的所有值。
}
}

@nested[#:style eopl-example]{
为了让实现简单一点，我们的语言要求模块主体按照接口声明的顺序给出各值。因此

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : int
   v : int]
 body
  [v = 33
   u = 44]

from m1 take u
}|
}

类型异常。可以弥补这一点（练习8.8，8.17）。
}
}

@nested[#:style eopl-example]{
在我们的语言中，模块具有@tt{let*}式的作用范围（练习3.17）。例如，

@nested{
@nested[#:style 'code-inset]{
@verbatim|{
module m1
 interface
  [u : int]
 body
  [u = 44]

module m2
 interface
  [v : int]
 body
  [v = -(from m1 take u,11)]

-(from m1 take u, from m2 take v)
}|
}

类型为@tt{int}。但如果我们交换定义的顺序，得

@nested[#:style 'code-inset]{
@verbatim|{
module m2
 interface
  [v : int]
 body
  [v = -(from m1 take u,11)]

module m1
 interface
  [u : int]
 body
  [u = 44]

-(from m1 take u, from m2 take v)
}|
}

类型异常，因为@tt{m2}主体中使用@tt{from m1 take u}之处不在后者的作用范围内。
}
}

@subsection[#:tag "s8.1.2"]{实现简单模块系统}

@subsubsection[#:style 'unnumbered #:tag "s8.1.2.1"]{语法}

SIMPLE-MODULES的程序包含一串模块定义，然后是一个表达式。

@envalign*{
           \mathit{Program} &::= \{\mathit{ModuleDefn}\}^{*} \mathit{Expression} \\[-3pt]
          &\mathrel{\phantom{::=}} \fbox{@tt{a-program (m-defs body)}}}

模块定义包含名字、接口和主体。

@envalign*{
           \mathit{ModuleDefn} &::= @tt{module} \mathit{Identifier} @tt{interface} \mathit{Iface} @tt{body} \mathit{ModuleBody} \\[-3pt]
            &\mathrel{\phantom{::=}} \fbox{@tt{a-module-definition (m-name expected-iface m-body)}}}

简单模块的接口包含任意数量的声明。每个声明指定程序中一个变量的类型。我们称之为
@emph{值声明} (@emph{value declaration})，因为要声明的变量表示一个值。在后面几节
中，我们介绍其他种类的接口和声明。

@envalign*{
           \mathit{Iface} &::= @tt["["] \{\mathit{Decl}\}^{*} @tt["]"] \\[-3pt]
        &\mathrel{\phantom{::=}} \fbox{@tt{simple-iface (decls)}} \\[-5pt]
            \mathit{Decl} &::= \mathit{Identifier} @tt{:} \mathit{Type} \\[-3pt]
        &\mathrel{\phantom{::=}} \fbox{@tt{val-decl (var-name ty)}}
            }

模块主体包含任意数量的定义。每个定义将变量和某个表达式的值关联起来。

@envalign*{
           \mathit{ModuleBody} &::= @tt["["] \{\mathit{Defn}\}^{*} @tt["]"] \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{defns-module-body (defns)}} \\[-5pt]
                 \mathit{Defn} &::= \mathit{Identifier} @tt{=} \mathit{Expression} \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{val-defn (var-name exp)}}
            }

我们的表达式与CHECKED（@secref{s7.3}）相同，但要修改语法，新增一种表达式，以便使
用受限变量。

@envalign*{
           \mathit{Expression} &::= @tt{from} \mathit{Identifier} @tt{take} \mathit{Identifier} \\[-3pt]
             &\mathrel{\phantom{::=}} \fbox{@tt{qualified-var-exp (m-name var-name)}}
            }

@subsubsection[#:style 'unnumbered #:tag "s8.1.2.2"]{解释器}
